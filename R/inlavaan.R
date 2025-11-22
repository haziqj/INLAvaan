#' @export
inlavaan <- function(
    model,
    data,
    lavfun = "sem",
    estimator = "ML",
    method = c("skewnorm", "asymgaus"),
    start = NULL,
    control = list(),
    verbose = FALSE,
    add_priors = TRUE,
    ...
) {

  # Check arguments ------------------------------------------------------------
  method <- match.arg(method)
  estimator <- match.arg(estimator, choices = c("ML", "PML", "DWLS", "WLS"))
  lavargs <- list(...)
  lavargs$model <- model
  lavargs$data <- data
  lavargs$estimator <- estimator
  lavargs$verbose <- FALSE  # FIXME: Need some quiet mode maybe
  lavargs$do.fit <- FALSE
  # if (!isTRUE(lavargs$std.lv)) {
  #   cli::cli_alert_warning("For now, only standardised latent variables are supported. Setting std.lv = TRUE.")
  # }
  # lavargs$std.lv <- TRUE

  # Build joint log posterior --------------------------------------------------
  if (estimator == "ML") {
    if (isTRUE(verbose)) cli::cli_alert_info("Using MVN log-likelihood.")
    loglik <- function(x) {
      mvnorm_loglik_samplestats(x, lavmodel, lavsamplestats, lavdata, lavoptions, lavcache)
    }
  } else if (estimator == "PML") {
    if (isTRUE(verbose)) cli::cli_alert_info("Using pairwise log-likelihood.")
    loglik <- function(x) {
      pl_fn(x, lavmodel, lavsamplestats, lavdata, lavoptions, lavcache)
    }
  } else {
    # FIXME: Add Laplace?
    cli::cli_abort("That's not supported yet.")
  }

  # Initialise lavaan object ---------------------------------------------------
  fit0 <- do.call(get(lavfun, envir = asNamespace("lavaan")), lavargs)
  lavmodel       <- fit0@Model
  lavsamplestats <- fit0@SampleStats
  lavdata        <- fit0@Data
  lavoptions     <- fit0@Options
  lavpartable    <- fit0@ParTable
  lavcache       <- fit0@Cache
  n              <- fit0@SampleStats@ntotal
  m              <- sum(lavpartable$free > 0)

  pt <- inlavaanify_partable(lavpartable, blavaan::dpriors(), lavdata, lavoptions)
  PARIDX <- pt$free > 0

  # Prep work for approximation ------------------------------------------------
  joint_lp <- function(pars) {

    x <- pars_to_x(pars, pt)
    ll <- loglik(x)

    pld <- 0
    if (isTRUE(add_priors)) {
      pld <- prior_logdens(attr(x, "xcor"), pt, PARIDX)
    }

    ll + pld
  }

  # Find posterior mode (maximise joint log posterior)
  if (isTRUE(verbose)) cli::cli_progress_step("Finding posterior mode.")
  parstart <- pt$parstart[pt$free > 0]
  opt <- nlminb(parstart, \(x) -1 * joint_lp(x), control = control)
  theta_star <- opt$par
  lp_max <- joint_lp(theta_star)  # maximum value

  if (isTRUE(verbose)) cli::cli_progress_step("Computing the Hessian.")
  H_neg <- numDeriv::hessian(\(x) -1 * joint_lp(x), theta_star)

  # Stabilise inversion
  H_neg <- 0.5 * (H_neg + t(H_neg))
  eps <- 1e-6 * mean(diag(H_neg) ^ 2) ^ 0.5   # scale by Hessian magnitude
  H_neg_reg <- H_neg + diag(eps, nrow(H_neg))
  ok <- FALSE
  tries <- 0
  while (!ok && tries < 5) {
    tries <- tries + 1
    ok <- tryCatch({
      L <- chol(solve(H_neg_reg))  # directly get Sigma^{1/2}
      TRUE
    }, error = function(e) FALSE)
    if (!ok) H_neg_reg <- H_neg_reg + diag(eps * 10 ^ tries, nrow(H_neg))
  }
  Sigma_theta <- solve(H_neg_reg)

  # For whitening transformation: z = L^{-1}(theta - theta*)
  L <- t(chol(Sigma_theta))
  L_inv <- solve(L)

  # Approximations -------------------------------------------------------------
  pars_list <- setNames(as.list(1:m), paste0("pars[", 1:m, "]"))

  if (method == "asymgaus") {
    if (isTRUE(verbose)) cli::cli_alert_info("Using asymmetric Gaussian approximation.")
    if (isTRUE(verbose)) cli::cli_progress_step("Calibrating asymmetric Gaussians.")

    approx_data <-
      do.call(what = "rbind", lapply(
        pars_list,
        function(j) {
          k <- 2
          dplus  <- max(0.01, lp_max - joint_lp(theta_star + L[, j] * k))
          dminus <- max(0.01, lp_max - joint_lp(theta_star - L[, j] * k))
          c(sigma_plus = sqrt(k ^ 2 / (2 * dplus)),
            sigma_minus = sqrt(k ^ 2 / (2 * dplus)))
        }
      ))

    post_marg <- function(j, g, ginv, ginv_prime) {
      post_marg_asymgaus(j = j, g = g, ginv = ginv, ginv_prime = ginv_prime,
                         theta_star = theta_star, Sigma_theta = Sigma_theta,
                         sigma_asym = approx_data)
    }
  } else {
    if (isTRUE(verbose)) cli::cli_alert_info("Using skew normal approximation.")
    if (isTRUE(verbose)) cli::cli_progress_step("Fitting skew normal to marginals.")

    approx_data <- matrix(NA, nrow = m, ncol = 4)
    colnames(approx_data) <- c("xi", "omega", "alpha", "logC")

    approx_data <-
      do.call(what = "rbind", lapply(
        pars_list,
        function(j) {
          mv <- seq(-4, 4, length = 21)
          tt <- theta_star[j] + mv * sqrt(Sigma_theta[j, j])
          yy <- numeric(length(mv))
          for (k in seq_along(mv)) {
            yy[k] <- joint_lp(theta_star + L[, j] * mv[k])
          }
          yy <- yy - max(yy)  # normalise to have maximum at zero
          fit_sn <- fit_skew_normal(tt, yy)
          unlist(fit_sn)
        }
      ))

    post_marg <- function(j, g, ginv, ginv_prime) {
      post_marg_skewnorm(j = j, g = g, ginv = ginv, ginv_prime = ginv_prime,
                         theta_star = theta_star, Sigma_theta = Sigma_theta,
                         sn_params = approx_data)
    }
  }

  # Prepare output -------------------------------------------------------------
  if (isTRUE(verbose)) cli::cli_progress_step("Preparing output.")

  pt$names <- mapply(paste0, pt$lhs, pt$op, pt$rhs)
  parnames <- pt$names[pt$free > 0]

  tmp <- Map(
    f          = post_marg,
    j          = seq_len(m),
    g          = pt$g[pt$free > 0],
    ginv       = pt$ginv[pt$free > 0],
    ginv_prime = pt$ginv_prime[pt$free > 0]
  )

  summ <- do.call("rbind", Map(
    f = function(x, y) {
      out <- t(data.frame(x$summary))
      row.names(out) <- y
      out
    },
    x = tmp,
    y = parnames
  ))

  pdf_data <- lapply(tmp, \(x) x$pdf_data)
  names(pdf_data) <- parnames

  out <- list(
    coefficients = summ[, "Mean"],
    summary = summ,
    theta_star = as.numeric(theta_star),
    Sigma_theta = Sigma_theta,
    theta_star_trans = pars_to_x(theta_star, pt),
    approx_data = approx_data,
    pdf_data = pdf_data,
    partable = pt
  )
  class(out) <- "inlavaan_internal"
  out
}

#' @export
print.inlavaan_internal <- function(x, digits = 3, ...) {
  print(round(x$coefficients, digits))
  invisible(x)
}

#' @export
summary.inlavaan_internal <- function(object, ...) {
  structure(
    list(
      summary = object$summary
    ),
    class = "summary.inlavaan_internal"
  )
}

#' @export
print.summary.inlavaan_internal <- function(x, digits = 3, ...) {
  print(round(x$summary, digits))
  invisible(x)
}

#' @export
plot.inlavaan_internal <- function(x, ...) {
  all_plots <- list()
  postmode <- x$summary[, "Mode"]

  for (j in seq_along(x$pdf_data)) {
    param <- names(x$pdf_data)[j]
    plot_df <- x$pdf_data[[param]]

    p_dens <-
      ggplot(plot_df, aes(x, y)) +
      geom_line() +
      geom_vline(xintercept = postmode[j], linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(x = NULL, y = NULL, subtitle = param)

    all_plots[[param]] <- p_dens
  }

  cowplot::plot_grid(plotlist = all_plots)
}


