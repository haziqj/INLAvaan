get_SEM_param_matrix <- function(x, mat, lavmodel) {
  nG <- lavmodel@ngroups
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)

  GLIST <- Map(
    function(mat, dn) {
      rownames(mat) <- dn[[1]]
      colnames(mat) <- dn[[2]]
      mat
    },
    lavmodel_x@GLIST,
    lavmodel_x@dimNames
  )

  uniq_names <- unique(names(GLIST))
  k <- length(uniq_names)
  out <- vector("list", nG)
  for (g in seq_len(nG)) {
    idx <- ((g - 1) * k + 1):(g * k)
    out[[g]] <- GLIST[idx]
    names(out[[g]]) <- uniq_names
  }

  if (mat == "all" | mat == "GLIST") {
    return(out)
  } else {
    return(lapply(out, function(glist) glist[[mat]]))
  }
}

# For factor scores, there is the plugin marginal_method and sampling marginal_method.
#
# For plugin marginal_method, eta | y ~ N(mu(theta, y), V(theta)), where
# mu(theta,y) = E(eta | y,theta) = Phi Lambda Sigma^{-1} y
# V(theta) = Phi - Phi Lambda' Sigma^{-1} Lambda Phi
# Phi = (I - B)^{-1} Psi (I - B')^{-1}'
#
# For sampling marginal_method just sample from the above distribution.

#' @exportS3Method predict inlavaan_internal
#' @keywords internal
predict.inlavaan_internal <- function(
  object,
  type = c("lv", "yhat", "ov", "ypred", "ydist"),
  nsamp = 250,
  ...
) {
  type <- match.arg(type)

  theta_star <- object$theta_star
  Sigma_theta <- object$Sigma_theta
  marginal_method <- object$marginal_method
  approx_data <- object$approx_data
  pt <- object$partable
  lavmodel <- object$lavmodel
  lavdata <- object$lavdata

  samp <- sample_params(
    theta_star = theta_star,
    Sigma_theta = Sigma_theta,
    method = marginal_method,
    approx_data = approx_data,
    pt = pt,
    lavmodel = lavmodel,
    nsamp = nsamp
  )
  x_samp <- samp$x_samp

  nG <- lavdata@ngroups
  group_labels <- lavdata@group.label
  y <- lavdata@X

  if (type == "lv") {
    sample_lv <- function(xx) {
      GLIST <- get_SEM_param_matrix(xx, "all", lavmodel)
      out <- vector("list", nG)
      names(out) <- group_labels
      for (g in seq_len(nG)) {
        glist <- GLIST[[g]]
        Lambda <- glist$lambda
        Psi <- glist$psi
        Theta <- glist$theta
        B <- glist$beta
        alpha <- glist$alpha

        if (is.null(alpha)) alpha <- 0

        if (is.null(B)) {
          # Pure CFA: IminB_inv = I, so Phi = Psi
          Phi <- Psi
          front <- Lambda
        } else {
          IminB_inv <- solve(diag(nrow(B)) - B)
          Phi <- IminB_inv %*% Psi %*% t(IminB_inv)
          front <- Lambda %*% IminB_inv
        }

        Sigmay_inv <- solve(front %*% Psi %*% t(front) + Theta)

        # Precompute Phi %*% t(Lambda) %*% Sigmay_inv (used for both mu and V)
        PhiLtSinv <- Phi %*% t(Lambda) %*% Sigmay_inv

        # Conditional mean: n x nlv matrix
        mu_eta <- t(as.numeric(alpha) + PhiLtSinv %*% t(y[[g]]))

        # Conditional variance (same for all obs given theta)
        V_eta <- Phi - PhiLtSinv %*% Lambda %*% Phi
        chol_V <- t(chol(V_eta))

        # Vectorised draw: one rnorm batch for all observations
        n_obs <- nrow(mu_eta)
        nlv <- ncol(mu_eta)
        Z <- matrix(rnorm(n_obs * nlv), nrow = nlv, ncol = n_obs)
        outg <- mu_eta + t(chol_V %*% Z)

        out[[g]] <- outg
      }

      if (nG == 1L) {
        colnames(out[[1L]]) <- colnames(Psi)
        out <- out[[1L]]
      } else {
        out <- do.call(
          rbind,
          Map(function(g, df) data.frame(group = g, df), names(out), out)
        )
        colnames(out)[-1] <- colnames(Psi)
      }
      rownames(out) <- NULL
      out
    }

    out <- vector("list", nsamp)
    cli_progress_bar(
      "Sampling latent variables",
      total = nsamp,
      clear = FALSE
    )
    for (i in seq_len(nsamp)) {
      out[[i]] <- sample_lv(x_samp[i, ])
      cli_progress_update()
    }
    cli_progress_done()
  } else {
    cli_abort("Only type = 'lv' is currently implemented.")
  }

  attr(out, "nobs") <- lavdata@nobs
  structure(out, class = "predict.inlavaan_internal")
}

#' @exportS3Method print predict.inlavaan_internal
#' @keywords internal
print.predict.inlavaan_internal <- function(
  x,
  stat = c("mean", "sd", ""),
  ...
) {
  cat("Predicted values from inlavaan model\n")
  cat("Number of samples:", length(x), "\n")
  cat("First sample:\n")
  print(x[[1]])
}

#' @exportS3Method summary predict.inlavaan_internal
#' @keywords internal
summary.predict.inlavaan_internal <- function(object, ...) {
  is_group <- FALSE
  if (!is.null(names(object[[1]])[1])) {
    is_group <- names(object[[1]])[1] == "group" &
      is.character(object[[1]][, 1])
  }

  if (is_group) {
    # Remove the group column, assuming it's always the first column
    group_id <- object[[1]][, 1]
    object <- lapply(object, function(df) as.matrix(df[-1]))
  } else {
    group_id <- NULL
  }
  arr <- simplify2array(object)

  Mean <- apply(arr, c(1, 2), mean)
  SD <- apply(arr, c(1, 2), sd)
  Q <- apply(arr, c(1, 2), quantile, probs = c(0.025, 0.5, 0.975))

  res <- list(
    group_id = group_id,
    Mean = Mean,
    SD = SD,
    `2.5%` = Q[1, , ],
    `50%` = Q[2, , ],
    `97.5%` = Q[3, , ]
  )
  structure(res, class = "summary.predict.inlavaan_internal")
}

#' @exportS3Method print summary.predict.inlavaan_internal
#' @keywords internal
print.summary.predict.inlavaan_internal <- function(x, stat = "Mean", ...) {
  cat(paste0(stat, " of predicted values from inlavaan model\n\n"))
  print(x[[stat]])
}

# #' @exportS3Method plot predict.inlavaan_internal
# #' @keywords internal
# plot.predict.inlavaan_internal <- function(x, nrow = NULL, ncol = NULL, ...) {
#   summ <- summary(x)
#   nobs <- attr(x, "nobs")
#   nG <- length(nobs)
#   groups <- rep(seq_len(nG), times = nobs)
#
#   means <- summ$Mean
#   score <- rowSums(means)
#   ranks <- order(score, decreasing = TRUE)
#   summ <- lapply(summ, function(mat) cbind(group = groups, mat))
#
#   plot_df <-
#     bind_rows(lapply(summ, function(x) {
#       as.data.frame(x) |>
#         rownames_to_column("id")
#     }), .id = "statistic") |>
#     pivot_longer(-c(statistic, id, group), names_to = "var", values_to = "val") |>
#     pivot_wider(names_from = statistic, values_from = val) |>
#     mutate(
#       id = factor(id, levels = ranks),
#       group = factor(group, labels = attr(x, "group.label"))
#     )
#
#   if (nG > 1L) {
#     browser()
#     p <-
#       ggplot(plot_df) +
#       geom_pointrange(aes(x = id, y = Mean, ymin = `2.5%`, ymax = `97.5%`, col = group), size = 0)
#   } else {
#     p <-
#       ggplot(plot_df) +
#       geom_pointrange(aes(x = id, y = Mean, ymin = `2.5%`, ymax = `97.5%`), size = 0)
#   }
#
#   p +
#     facet_wrap(~ var, nrow = nrow, ncol = ncol) +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5)) +
#     labs(x = "Individual ID", y = "Value")
#
# }

#' @inheritParams inlavaan
#' @rdname INLAvaan-class
#' @param object An object of class [INLAvaan].
#' @export
setMethod("predict", "INLAvaan", function(object, nsamp = 1000, ...) {
  predict.inlavaan_internal(
    object@external$inlavaan_internal,
    nsamp = nsamp,
    ...
  )
})
