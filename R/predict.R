get_SEM_param_matrix <- function(x, mat, lavmodel) {
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)
  lavimplied <- lavaan::lav_model_implied(lavmodel_x)

  GLIST <- Map(function(mat, dn) {
    rownames(mat) <- dn[[1]]
    colnames(mat) <- dn[[2]]
    mat
  }, lavmodel_x@GLIST, lavmodel_x@dimNames)

  if (mat == "all" | mat == "GLIST") {
    return(GLIST)
  } else {
    return(GLIST[[mat]])
  }
}

# For factor scores, there is the plugin method and sampling method.
#
# For plugin method, eta | y ~ N(mu(theta, y), V(theta)), where
# mu(theta,y) = E(eta | y,theta) = Phi Lambda Sigma^{-1} y
# V(theta) = Phi - Phi Lambda' Sigma^{-1} Lambda Phi
# Phi = (I - B)^{-1} Psi (I - B')^{-1}'
#
# For sampling method just sample from the above distribution.

#' @export
predict.inlavaan_internal <- function(object, type = c("lv", "yhat", "ov", "ypred", "ydist"), nsamp = 250, ...) {
  type <- match.arg(type)

  theta_star <- object$theta_star
  Sigma_theta <- object$Sigma_theta
  method <- object$method
  approx_data <- object$approx_data
  pt <- object$partable
  lavmodel <- object$lavmodel
  lavdata <- object$lavdata

  x_samp <- sample_params(
    theta_star = theta_star,
    Sigma_theta = Sigma_theta,
    method = method,
    approx_data = approx_data,
    pt = pt,
    lavmodel = lavmodel,
    nsamp = nsamp,
    return_theta = FALSE
  )

  y <- lavdata@X[[1]]  # FIXME: What if group data?

  if (type == "lv") {
    sample_lv <- function(xx) {
      GLIST <- get_SEM_param_matrix(xx, "all", lavmodel)

      Lambda <- GLIST$lambda
      Psi <- GLIST$psi
      Theta <- GLIST$theta
      B <- GLIST$beta
      alpha <- GLIST$alpha

      IminB <- diag(nrow(B)) - if (is.null(B)) 0 else B
      if (is.null(alpha)) alpha <- 0
      IminB_inv <- solve(IminB)

      front <- Lambda %*% IminB_inv
      Sigmay <- front %*% Psi %*% t(front) + Theta
      Sigmay_inv <- solve(Sigmay)

      Phi <- IminB_inv %*% Psi %*% t(IminB_inv)
      mu_eta <- t(alpha + Phi %*% t(Lambda) %*% Sigmay_inv %*% t(y))
      V_eta <- Phi - Phi %*% t(Lambda) %*% Sigmay_inv %*% Lambda %*% Phi

      out <- t(apply(mu_eta, 1, function(mu) {
        mvtnorm::rmvnorm(1, mean = mu, sigma = V_eta)
      }))
      colnames(out) <- colnames(Psi)
      out
    }

    cli::cli_progress_bar("Sampling latent variables", total = nsamp, clear = FALSE)
    out <- vector("list", nsamp)
    for (i in seq_len(nsamp)) {
      out[[i]] <- sample_lv(x_samp[i, ])
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
  } else {
    cli::cli_abort("Only type = 'lv' is currently implemented.")
  }

  structure(out, class = "predict.inlavaan_internal")
}

#' @export
print.predict.inlavaan_internal <- function(x, stat = c("mean", "sd", ""), ...) {
  cat("Predicted values from inlavaan model\n")
  cat("Number of samples:", length(x), "\n")
  cat("First sample:\n")
  print(x[[1]])
}

#' @export
summary.predict.inlavaan_internal <- function(object, ...) {
  arr <- simplify2array(object)

  Mean <- apply(arr, c(1, 2), mean)
  SD   <- apply(arr, c(1, 2), sd)
  Q    <- apply(arr, c(1, 2), quantile, probs = c(0.025, 0.5, 0.975))

  res <- list(
    Mean = Mean,
    SD = SD,
    `2.5%` = Q[1, ,],
    `50%` = Q[2, ,],
    `97.5%` = Q[3, ,]
  )
  structure(res, class = "summary.predict.inlavaan_internal")
}

#' @export
print.summary.predict.inlavaan_internal <- function(x, stat = "Mean", ...) {
  cat(paste0(stat, " of predicted values from inlavaan model\n\n"))
  print(x[[stat]])
}

#' @export
plot.predict.inlavaan_internal <- function(x, ...) {
  summ <- summary(x)

  means <- summ$Mean
  score <- rowSums(means)
  ranks <- order(score, decreasing = TRUE)

  bind_rows(lapply(summ, function(x) {
    as.data.frame(x) |>
      rownames_to_column("id")
  }), .id = "statistic") |>
    pivot_longer(-c(statistic, id), names_to = "var", values_to = "val") |>
    pivot_wider(names_from = statistic, values_from = val) |>
    mutate(id = factor(id, levels = ranks)) |>
    ggplot() +
    geom_pointrange(aes(x = id, y = Mean, ymin = `2.5%`, ymax = `97.5%`), size = 0) +
    facet_wrap(~ var) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5)) +
    labs(x = "Individual ID", y = "Value")
}
