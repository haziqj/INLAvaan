#' Standardised solution of a latent variable model
#'
#' @inheritParams lavaan::standardizedSolution
#' @inheritParams INLAvaan-class
#' @param object An object of class [INLAvaan].
#' @param nsamp The number of samples to draw from the approximate posterior
#'   distribution for the calculation of standardised estimates.
#' @param ... Additional arguments sent to `lavaan::standardizedSolution()`.
#'
#' @returns A `data.frame` containing standardised model parameters.
#' @export
#' @example inst/examples/ex-stdsoln.R
standardisedsolution <- function(
  object,
  type = "std.all",
  se = TRUE,
  ci = TRUE,
  level = 0.95,
  postmedian = FALSE,
  postmode = FALSE,
  cov.std = TRUE,
  remove.eq = TRUE,
  remove.ineq = TRUE,
  remove.def = FALSE,
  nsamp = 250,
  ...
) {
  if (class(object) == "lavaan") {
    return(lavaan::standardizedSolution(object))
  } else if (class(object) == "blavaan") {
    return(blavaan::standardizedPosterior(object))
  }

  fit_inlv <- get_inlavaan_internal(object)
  pt <- fit_inlv$partable

  x_samp <- with(
    fit_inlv,
    sample_params(
      theta_star = theta_star,
      Sigma_theta = Sigma_theta,
      method = marginal_method,
      approx_data = approx_data,
      pt = partable,
      lavmodel = lavmodel,
      nsamp = nsamp,
      return_theta = FALSE
    )
  )

  xstd_samp <- vector("list", nrow(x_samp))
  for (i in seq_len(nrow(x_samp))) {
    xi <- x_samp[i, ]
    lavmodel <- lavaan::lav_model_set_parameters(object@Model, xi)

    esti <- pt$est
    esti[pt$free > 0] <- xi[pt$free[pt$free > 0]]
    if (any(pt$op == ":=")) {
      pt_def_rows <- which(pt$op == ":=")
      def_names <- pt$names[pt_def_rows]
      esti[pt_def_rows] <- fit_inlv$summary[def_names, "Mean"]
    }
    xstd_samp[[i]] <- lavaan::standardizedSolution(
      object = object,
      est = esti,
      GLIST = lavmodel@GLIST,
      type = type,
      cov.std = cov.std,
      remove.eq = remove.eq,
      remove.ineq = remove.ineq,
      remove.def = remove.def,
      ...
    )$est.std
  }
  xstd_samp <- do.call("rbind", xstd_samp)

  res <- list(
    mean = apply(xstd_samp, 2, mean),
    sd = apply(xstd_samp, 2, sd),
    ci_lower = apply(xstd_samp, 2, quantile, probs = (1 - level) / 2),
    ci_upper = apply(xstd_samp, 2, quantile, probs = 1 - (1 - level) / 2),
    median = apply(xstd_samp, 2, median),
    mode = apply(xstd_samp, 2, modeest::mfv1)
  )

  out <- lavaan::standardizedSolution(
    object = object,
    est = esti,
    type = type,
    cov.std = cov.std,
    remove.eq = remove.eq,
    remove.ineq = remove.ineq,
    remove.def = remove.def,
    ...
  )
  out$est.std <- res$mean
  if (isTRUE(se)) {
    out$se <- res$sd
  }
  if (isTRUE(ci)) {
    out$ci.lower <- res$ci_lower
    out$ci.upper <- res$ci_upper
  }
  if (isTRUE(postmedian)) {
    out$median <- res$median
  }
  if (isTRUE(postmode)) {
    out$mode <- res$mode
  }
  out
}

#' @rdname standardisedsolution
#' @export
standardisedSolution <- function(
  object,
  type = "std.all",
  se = TRUE,
  ci = TRUE,
  level = 0.95,
  postmedian = FALSE,
  postmode = FALSE,
  cov.std = TRUE,
  remove.eq = TRUE,
  remove.ineq = TRUE,
  remove.def = FALSE,
  nsamp = 250,
  ...
) {
  standardisedsolution(
    object = object,
    type = type,
    se = se,
    ci = ci,
    level = level,
    postmedian = postmedian,
    postmode = postmode,
    cov.std = cov.std,
    remove.eq = remove.eq,
    remove.ineq = remove.ineq,
    remove.def = remove.def,
    nsamp = nsamp,
    ...
  )
}

#' @rdname standardisedsolution
#' @export
standardizedsolution <- function(
  object,
  type = "std.all",
  se = TRUE,
  ci = TRUE,
  level = 0.95,
  postmedian = FALSE,
  postmode = FALSE,
  cov.std = TRUE,
  remove.eq = TRUE,
  remove.ineq = TRUE,
  remove.def = FALSE,
  nsamp = 250,
  ...
) {
  standardisedsolution(
    object = object,
    type = type,
    se = se,
    ci = ci,
    level = level,
    postmedian = postmedian,
    postmode = postmode,
    cov.std = cov.std,
    remove.eq = remove.eq,
    remove.ineq = remove.ineq,
    remove.def = remove.def,
    nsamp = nsamp,
    ...
  )
}

#' @rdname standardisedsolution
#' @export
standardizedSolution <- function(
  object,
  type = "std.all",
  se = TRUE,
  ci = TRUE,
  level = 0.95,
  postmedian = FALSE,
  postmode = FALSE,
  cov.std = TRUE,
  remove.eq = TRUE,
  remove.ineq = TRUE,
  remove.def = FALSE,
  nsamp = 250,
  ...
) {
  standardisedsolution(
    object = object,
    type = type,
    se = se,
    ci = ci,
    level = level,
    postmedian = postmedian,
    postmode = postmode,
    cov.std = cov.std,
    remove.eq = remove.eq,
    remove.ineq = remove.ineq,
    remove.def = remove.def,
    nsamp = nsamp,
    ...
  )
}
