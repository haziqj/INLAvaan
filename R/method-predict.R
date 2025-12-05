get_SEM_param_matrix <- function(x, mat, lavmodel) {
  nG <- lavmodel@ngroups
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)
  lavimplied <- lavaan::lav_model_implied(lavmodel_x)

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

  x_samp <- sample_params(
    theta_star = theta_star,
    Sigma_theta = Sigma_theta,
    method = marginal_method,
    approx_data = approx_data,
    pt = pt,
    lavmodel = lavmodel,
    nsamp = nsamp,
    return_theta = FALSE
  )

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

        IminB <- if (is.null(B)) diag(nrow(Psi)) else (diag(nrow(B)) - B)
        if (is.null(alpha)) {
          alpha <- 0
        }
        IminB_inv <- solve(IminB)

        front <- Lambda %*% IminB_inv
        Sigmay <- front %*% Psi %*% t(front) + Theta
        Sigmay_inv <- solve(Sigmay)

        Phi <- IminB_inv %*% Psi %*% t(IminB_inv)
        mu_eta <- t(
          as.numeric(alpha) + Phi %*% t(Lambda) %*% Sigmay_inv %*% t(y[[g]])
        )
        V_eta <- Phi - Phi %*% t(Lambda) %*% Sigmay_inv %*% Lambda %*% Phi

        out[[g]] <- t(apply(mu_eta, 1, function(mu) {
          mvtnorm::rmvnorm(1, mean = mu, sigma = V_eta)
        }))
      }

      if (nG == 1L) {
        out <- do.call(rbind, out)
        colnames(out) <- colnames(Psi)
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

    cli::cli_progress_bar(
      "Sampling latent variables",
      total = nsamp,
      clear = FALSE
    )
    out <- vector("list", nsamp)
    for (i in seq_len(nsamp)) {
      out[[i]] <- sample_lv(x_samp[i, ])
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
  } else {
    cli::cli_abort("Only type = 'lv' is currently implemented.")
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
  is_group <- names(object[[1]])[1] == "group" & is.character(object[[1]][, 1])
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

#' @rdname INLAvaan-class
#' @param object An object of class [INLAvaan].
#' @export
setMethod("predict", "INLAvaan", function(object, ...) {
  predict.inlavaan_internal(object@external$inlavaan_internal, ...)
})
