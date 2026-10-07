#' Standardised solution of a latent variable model
#'
#' @inheritParams lavaan::standardizedSolution
#' @inheritParams INLAvaan-class
#' @param object An object of class [INLAvaan].
#' @param cov.std Logical. If `TRUE`, the (residual) observed covariances
#'   are scaled by the square root of the `Theta` diagonal elements, and the
#'   (residual) latent covariances are scaled by the square root of the
#'   `Psi` diagonal elements. If `FALSE`, the (residual) observed
#'   covariances are scaled by the square root of the diagonal elements of
#'   the model-implied observed covariance matrix, and the (residual)
#'   latent covariances are scaled similarly using the model-implied
#'   covariance matrix of the latent variables. Documented explicitly here
#'   (rather than inherited) because lavaan >= 0.7-1 renamed this and the
#'   next three arguments to snake_case.
#' @param remove.eq Logical. If `TRUE`, filter the output by removing all
#'   rows containing equality constraints, if any.
#' @param remove.ineq Logical. If `TRUE`, filter the output by removing all
#'   rows containing inequality constraints, if any.
#' @param remove.def Logical. If `TRUE`, filter the output by removing all
#'   rows containing parameter definitions, if any.
#' @param nsamp The number of samples to draw from the approximate posterior
#'   distribution for the calculation of standardised estimates.
#' @param ... Additional arguments sent to `lavaan::standardizedSolution()`.
#'
#' @returns A `data.frame` containing standardised model parameters.
#'
#' @seealso [summary()], [coef()], [vcov()]
#'
#' @export
#' @name standardisedsolution
#' @rdname standardisedsolution
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
  if (is_lavaan(object)) {
    return(lavaan::standardizedSolution(object))
  } else if (is_blavaan(object)) {
    return(blavaan::standardizedPosterior(object))
  }

  if (!isTRUE(nsamp >= 2)) {
    cli_abort("{.arg nsamp} must be at least 2 to summarise posterior draws.")
  }
  fit_inlv <- get_inlavaan_internal(object)
  pt <- fit_inlv$partable

  samp <- with(
    fit_inlv,
    sample_params(
      theta_star = theta_star,
      Sigma_theta = Sigma_theta,
      method = marginal_method,
      approx_data = approx_data,
      pt = partable,
      lavmodel = lavmodel,
      nsamp = nsamp
    )
  )
  x_samp <- samp$x_samp
  comp_rows <- composite_derived_rows(pt)

  xstd_samp <- vector("list", nrow(x_samp))
  for (i in seq_len(nrow(x_samp))) {
    xi <- x_samp[i, ]
    lavmodel <- lavaan::lav_model_set_parameters(object@Model, xi)

    esti <- pt$est
    esti[pt$free > 0] <- xi[pt$free[pt$free > 0]]
    # The composite variances and intercepts of this draw, because lavaan
    # divides est by the variances in glist.
    if (length(comp_rows) > 0L) {
      esti[comp_rows] <- lavaan::lav_model_get_parameters(
        lavmodel,
        type = "user",
        extra = FALSE
      )[comp_rows]
    }
    if (any(pt$op == ":=")) {
      pt_def_rows <- which(pt$op == ":=")
      def_names <- pt$names[pt_def_rows]
      esti[pt_def_rows] <- fit_inlv$summary[def_names, "Mean"]
    }
    xstd_samp[[i]] <- muffle_nan_warnings(lavaan::standardizedSolution(
      object = object,
      est = esti,
      glist = lavmodel@GLIST,
      type = type,
      cov_std = cov.std,
      remove_eq = remove.eq,
      remove_ineq = remove.ineq,
      remove_def = remove.def,
      ...
    ))$est.std
  }
  xstd_samp <- do.call("rbind", xstd_samp)

  out <- muffle_nan_warnings(lavaan::standardizedSolution(
    object = object,
    est = esti,
    type = type,
    cov_std = cov.std,
    remove_eq = remove.eq,
    remove_ineq = remove.ineq,
    remove_def = remove.def,
    ...
  ))

  # Summarise each row over its finite draws, since a := parameter can be
  # undefined for part of the posterior. Warn about := rows that the fit did not
  # already report, e.g. those undefined only on the standardised scale.
  ok <- is.finite(xstd_samp)
  def_rows <- which(out$op == ":=")
  share <- setNames(colMeans(!ok[, def_rows, drop = FALSE]), out$lhs[def_rows])
  reported <- names(which(fit_inlv$def_undefined > 0))
  new_bad <- share > 0 & !names(share) %in% reported
  if (any(new_bad)) {
    n_defined <- colSums(ok[, def_rows, drop = FALSE])
    warn_undefined_draws(share[new_bad], n_defined[new_bad], scale = type)
  }
  finite_stat <- function(f, ...) {
    vapply(
      seq_len(ncol(xstd_samp)),
      function(j) {
        y <- xstd_samp[ok[, j], j]
        if (length(y) < 2) NA_real_ else unname(f(y, ...))
      },
      numeric(1)
    )
  }
  res <- list(
    mean = finite_stat(mean),
    sd = finite_stat(sd),
    ci_lower = finite_stat(quantile, probs = (1 - level) / 2),
    ci_upper = finite_stat(quantile, probs = 1 - (1 - level) / 2),
    median = finite_stat(median),
    mode = finite_stat(dmode)
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

#' @name standardisedsolution
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

#' @name standardizedsolution
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

#' @name standardizedsolution
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
