# --- Bayesian fit index helpers -----------------------------------------------

# Saturated log-likelihood (constant for ML; sum across groups)
# Under FIML, uses the pattern-based formula to stay on the same scale as
# inlav_model_loglik (which delegates to lavaan:::lav_model_loglik).
compute_loglik_sat <- function(lavsamplestats, lavdata) {
  ngroups <- lavdata@ngroups
  logl_sat <- 0
  for (g in seq_len(ngroups)) {
    if (lavsamplestats@missing.flag) {
      logl_sat <- logl_sat +
        lavaan___lav_mvnorm_missing_loglik_samplestats(
          Yp     = lavsamplestats@missing[[g]],
          Mu     = lavsamplestats@mean[[g]],
          Sigma  = lavsamplestats@cov[[g]],
          x.idx  = lavsamplestats@x.idx[[g]],
          x.mean = lavsamplestats@mean.x[[g]],
          x.cov  = lavsamplestats@cov.x[[g]]
        )
    } else {
      logl_sat <- logl_sat +
        lavaan___lav_mvnorm_loglik_samplestats(
          sample.mean = lavsamplestats@mean[[g]],
          sample.cov  = lavsamplestats@cov[[g]],
          sample.nobs = lavsamplestats@nobs[[g]],
          Mu          = lavsamplestats@mean[[g]],
          Sigma       = lavsamplestats@cov[[g]],
          x.idx       = lavsamplestats@x.idx[[g]],
          x.mean      = lavsamplestats@mean.x[[g]],
          x.cov       = lavsamplestats@cov.x[[g]]
        )
    }
  }
  logl_sat
}

# Per-sample deviance chi-square:  chisq_s = 2 * (loglik_sat - loglik(x_s))
# This equals N * F_ML(x_s).
compute_chisq_dev <- function(x_samp, lavmodel, lavsamplestats, lavdata,
                              lavoptions, lavcache) {
  loglik_sat <- compute_loglik_sat(lavsamplestats, lavdata)
  vapply(seq_len(nrow(x_samp)), function(i) {
    ll_i <- inlav_model_loglik(
      x_samp[i, ], lavmodel, lavsamplestats, lavdata, lavoptions, lavcache
    )
    2 * (loglik_sat - ll_i)
  }, numeric(1))
}

# Number of sample statistics:  sum_g [ p_g(p_g+1)/2 + meanstructure * p_g ]
compute_p_samplestats <- function(nvar, meanstructure) {
  sum(vapply(nvar, function(nv) {
    nMom <- nv * (nv + 1) / 2
    if (isTRUE(meanstructure)) nMom <- nMom + nv
    nMom
  }, numeric(1)))
}

# Absolute fit indices (vectorised over posterior samples) ---------------------
compute_BRMSEA <- function(nonc, df, N, Ngr) {
  sqrt(nonc / (df * N)) * sqrt(Ngr)
}
compute_BGammaHat <- function(nonc, nvar_total, N) {
  nvar_total / (nvar_total + 2 * nonc / N)
}
compute_adjBGammaHat <- function(BGammaHat, p, df) {
  1 - (p / df) * (1 - BGammaHat)
}
compute_BMc <- function(nonc, N) exp(-0.5 * nonc / N)

# Incremental fit indices (vectorised) ----------------------------------------
compute_BCFI <- function(nonc, nonc_null) 1 - nonc / nonc_null
compute_BTLI <- function(adj_dev, df, adj_dev_null, df_null) {
  tli_null <- adj_dev_null / df_null
  (tli_null - adj_dev / df) / (tli_null - 1)
}
compute_BNFI <- function(adj_dev, adj_dev_null) {
  (adj_dev_null - adj_dev) / adj_dev_null
}

# Reconstruct lavoptions suitable for inlav_model_loglik() from the INLAvaan
# object (whose @Options$estimator was changed to "Bayes").
reconstruct_lavoptions <- function(object) {
  opts <- object@Options
  opts$estimator <- object@external$inlavaan_internal$lavmodel@estimator
  opts
}

# Compute rescaled chi-square, adjusted deviance, df, and noncentrality for
# a single model under the chosen rescaling method.
#
# Returns list(chisq, adj_dev, df, nonc, N_adj, pD) where chisq/adj_dev/nonc
# are per-sample vectors and df/N_adj/pD are scalars.
compute_rescaled_quantities <- function(
  object, x_samp, lavmodel, lavsamplestats, lavdata, lavoptions, lavcache,
  p, rescale
) {
  N    <- lavsamplestats@ntotal
  Ngr  <- lavdata@ngroups
  npar <- object@Fit@npar

  chisq_dev <- compute_chisq_dev(
    x_samp, lavmodel, lavsamplestats, lavdata, lavoptions, lavcache
  )

  if (rescale == "devM") {
    pD <- object@external$inlavaan_internal$DIC$pD
    if (is.null(pD) || pD <= 0 || pD >= p) pD <- npar # nocov
    adj_dev <- chisq_dev - pD              # obs - reps
    df      <- p - pD
    N_adj   <- N
  } else {
    # MCMC: use classical chi-square = (N-1)/N * deviance
    pD      <- npar
    chisq_dev <- (N - 1) / N * chisq_dev
    adj_dev <- chisq_dev                   # reps = 0
    df      <- p - npar
    N_adj   <- N - Ngr                     # EQS-style: Min1 = TRUE
  }

  nonc <- pmax(adj_dev - df, 0)
  list(chisq = chisq_dev, adj_dev = adj_dev, df = df, nonc = nonc,
       N_adj = N_adj, pD = pD)
}

# ---------------------------------------------------------------------------
# bfit_indices: compute per-sample Bayesian fit index vectors and return
# an S3 object of class "bfit_indices" with a summary() and print() method.
# ---------------------------------------------------------------------------

#' Bayesian Fit Indices
#'
#' Compute posterior distributions of Bayesian fit indices for an INLAvaan
#' model, analogous to [blavaan::blavFitIndices()].
#'
#' @param object An object of class [INLAvaan].
#' @param baseline.model An optional [INLAvaan] object representing the
#'   baseline (null) model. Required for incremental fit indices (BCFI, BTLI,
#'   BNFI).
#' @param rescale Character string controlling how the Bayesian chi-square
#'   is rescaled. `"devM"` (default) subtracts pD from the deviance at each
#'   sample. `"MCMC"` uses the classical chi-square and classical df at each
#'   sample.
#' @param nsamp Number of posterior samples to draw. Defaults to the value
#'   used when fitting the model.
#' @param samp_copula Logical. When `TRUE` (default), posterior samples are
#'   drawn using the copula method with the fitted marginals. When `FALSE`,
#'   samples are drawn from the Gaussian (Laplace) approximation.
#'
#' @returns An S3 object of class `"bfit_indices"` containing:
#' \describe{
#'   \item{`indices`}{Named list of numeric vectors (one per posterior sample)
#'     for each computed fit index.}
#'   \item{`details`}{List with `chisq` (per-sample deviance), `df`, `pD`,
#'     `rescale`, and `nsamp`.}
#' }
#' Use [summary()] to obtain a table of posterior summaries (Mean, SD,
#' quantiles, Mode) for each index.
#'
#' @seealso [lavaan::fitMeasures()]
#' @export
bfit_indices <- function(object, baseline.model = NULL,
                         rescale = c("devM", "MCMC"),
                         nsamp = NULL, samp_copula = TRUE) {
  rescale <- match.arg(rescale)
  if (!is(object, "INLAvaan")) {
    cli_abort("{.arg object} must be an {.cls INLAvaan} object.")
  }

  int <- object@external$inlavaan_internal
  lavmodel       <- int$lavmodel
  lavsamplestats <- int$lavsamplestats
  lavdata        <- int$lavdata

  nsamp <- nsamp %||% int$nsamp %||% 500L
  method <- if (isTRUE(samp_copula)) int$marginal_method else "sampling"
  samp <- sample_params(
    theta_star  = int$theta_star,
    Sigma_theta = int$Sigma_theta,
    method      = method,
    approx_data = int$approx_data,
    pt          = int$partable,
    lavmodel    = lavmodel,
    nsamp       = nsamp,
    R_star      = int$R_star
  )
  x_samp <- samp$x_samp

  if (lavmodel@estimator != "ML") {
    cli_abort("Bayesian fit indices are only supported for the ML estimator.")
  }
  if (rescale == "devM" && is.null(int$DIC)) {
    cli_abort("DIC not available. Refit with {.code test != \"none\"}, or use {.code rescale = \"MCMC\"}.")
  }

  lavoptions <- reconstruct_lavoptions(object)
  lavcache   <- object@Cache
  N     <- lavsamplestats@ntotal
  Ngr   <- lavdata@ngroups
  nvar  <- lavmodel@nvar
  ms    <- isTRUE(lavoptions$meanstructure)
  p     <- compute_p_samplestats(nvar, ms)

  rq <- compute_rescaled_quantities(
    object, x_samp, lavmodel, lavsamplestats, lavdata, lavoptions, lavcache,
    p, rescale
  )

  indices <- list()

  if (rq$df > 0) {
    indices$BRMSEA       <- compute_BRMSEA(rq$nonc, rq$df, rq$N_adj, Ngr)
    bgh                  <- compute_BGammaHat(rq$nonc, sum(nvar), rq$N_adj)
    indices$BGammaHat    <- bgh
    indices$adjBGammaHat <- compute_adjBGammaHat(bgh, p, rq$df)
    indices$BMc          <- compute_BMc(rq$nonc, rq$N_adj)
  } # else: df == 0, no absolute fit indices (nocov – saturated model)

  # Incremental indices
  if (!is.null(baseline.model)) {
    if (!is(baseline.model, "INLAvaan")) {
      cli_abort("{.arg baseline.model} must be an {.cls INLAvaan} object.")
    }
    bint <- baseline.model@external$inlavaan_internal

    bmethod <- if (isTRUE(samp_copula)) bint$marginal_method else "sampling"
    samp_null <- sample_params(
      theta_star  = bint$theta_star,
      Sigma_theta = bint$Sigma_theta,
      method      = bmethod,
      approx_data = bint$approx_data,
      pt          = bint$partable,
      lavmodel    = bint$lavmodel,
      nsamp       = nsamp,
      R_star      = bint$R_star
    )
    x_samp_null <- samp_null$x_samp
    n_use <- min(nrow(x_samp), nrow(x_samp_null))

    rq_null <- compute_rescaled_quantities(
      baseline.model,
      x_samp_null[seq_len(n_use), , drop = FALSE],
      bint$lavmodel, bint$lavsamplestats, bint$lavdata,
      reconstruct_lavoptions(baseline.model),
      baseline.model@Cache,
      p, rescale
    )

    adj_dev_use <- rq$adj_dev[seq_len(n_use)]
    nonc_use    <- rq$nonc[seq_len(n_use)]

    indices$BCFI <- compute_BCFI(nonc_use, rq_null$nonc)
    indices$BTLI <- compute_BTLI(
      adj_dev_use, rq$df, rq_null$adj_dev, rq_null$df
    )
    indices$BNFI <- compute_BNFI(adj_dev_use, rq_null$adj_dev)
  }

  structure(
    list(
      indices = indices,
      details = list(
        chisq   = rq$chisq,
        df      = rq$df,
        pD      = rq$pD,
        rescale = rescale,
        nsamp   = nrow(x_samp)
      )
    ),
    class = "bfit_indices"
  )
}

#' @exportS3Method summary bfit_indices
summary.bfit_indices <- function(object, ...) {
  summ_one <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 3) return(rep(NA_real_, 8))
    dens <- stats::density(x)
    c(
      Mean   = mean(x),
      SD     = stats::sd(x),
      `2.5%` = stats::quantile(x, 0.025, names = FALSE),
      `25%`  = stats::quantile(x, 0.250, names = FALSE),
      `50%`  = stats::quantile(x, 0.500, names = FALSE),
      `75%`  = stats::quantile(x, 0.750, names = FALSE),
      `97.5%`= stats::quantile(x, 0.975, names = FALSE),
      Mode   = dens$x[which.max(dens$y)]
    )
  }
  tab <- as.data.frame(do.call(rbind, lapply(object$indices, summ_one)))
  attr(tab, "header") <- paste0(
    "Posterior summary of ", object$details$rescale,
    "-based Bayesian fit indices (nsamp = ", object$details$nsamp, "):"
  )
  class(tab) <- c("lavaan.data.frame", "data.frame")
  tab
}

#' @exportS3Method print bfit_indices
print.bfit_indices <- function(x, ...) {
  tab <- summary(x)
  hdr <- attr(tab, "header")
  if (!is.null(hdr)) cat(hdr, "\n\n")
  eap <- vapply(x$indices, mean, numeric(1), na.rm = TRUE)
  print(round(eap, 3))
  invisible(x)
}

# --- Main fitMeasures function ------------------------------------------------

inlav_fit_measures <- function(
  object,
  fit.measures = "all",
  baseline.model = NULL,
  ...
) {
  dots <- list(...)
  rescale <- match.arg(dots$rescale %||% "devM", c("devM", "MCMC"))

  # Has the model converged?
  if (object@Fit@npar > 0L & !object@optim$converged) { # nocov
    cli_alert_warning("Optimiser did not converge.") # nocov
  } # nocov

  out <- vector("numeric")
  out["npar"] <- object@Fit@npar
  out["margloglik"] <- object@external$inlavaan_internal$mloglik

  # If test != "none"
  if (length(object@Fit@test$ppp) > 0) {
    out["ppp"] <- object@external$inlavaan_internal$ppp
    out["dic"] <- object@external$inlavaan_internal$DIC$dic
    out["p_dic"] <- object@external$inlavaan_internal$DIC$pD
  }

  # Validate baseline.model early (before tryCatch)
  if (!is.null(baseline.model) && !is(baseline.model, "INLAvaan")) {
    cli_abort("{.arg baseline.model} must be an {.cls INLAvaan} object.")
  }

  # Bayesian fit indices (BRMSEA, BGammaHat, etc.)
  bfi <- tryCatch(
    bfit_indices(object, baseline.model, rescale),
    error = function(e) NULL
  )
  if (!is.null(bfi)) {
    for (nm in names(bfi$indices)) {
      out[nm] <- mean(bfi$indices[[nm]], na.rm = TRUE)
    }
  }

  # Diagnostics based on gradients
  grad <- object@external$inlavaan_internal$opt$dx
  pars <- object@external$inlavaan_internal$theta_star
  out["grad_inf"] <- max(abs(grad))
  out["grad_inf_rel"] <- max(abs(grad) / (abs(pars) + 1e-06))
  out["grad_l2"] <- sqrt(sum(grad^2))

  # Filter if specific measures requested
  if (!identical(fit.measures, "all")) {
    idx <- which(names(out) %in% fit.measures)
    if (length(idx) == 0L) {
      cli_abort("No matching fit measures found.")
    }
    out <- out[idx]
  }

  class(out) <- c("fitmeasures.inlavaan_internal", "numeric")
  out
}

#' @exportS3Method print fitmeasures.inlavaan_internal
print.fitmeasures.inlavaan_internal <- function(x, ...) {
  nm <- names(x)

  # Apply conditional formatting
  formatted_values <- sapply(seq_along(x), function(i) {
    val <- x[i]
    name <- nm[i]

    if (name == "npar") {
      return(as.character(as.integer(round(val))))
    } else if (startsWith(name, "grad_")) {
      # Use formatC to force scientific and maintain 3 significant digits
      return(formatC(val, digits = 2, format = "e"))
    } else {
      # Round to 3 decimal places
      return(formatC(val, digits = 3, format = "f", drop0trailing = FALSE))
    }
  })

  # Set names back onto the formatted character vector
  names(formatted_values) <- nm

  # Print using the standard named vector style without quotes
  print(formatted_values, quote = FALSE, right = TRUE)

  invisible(x)
}

#' Fit Measures for a Latent Variable Model estimated using INLA
#'
#' @param object An object of class [INLAvaan].
#' @param fit.measures If `"all"`, all fit measures available will be returned. If
#'   only a single or a few fit measures are specified by name, only those are
#'   computed and returned.
#' @param baseline.model An optional [INLAvaan] object representing the
#'   baseline (null) model. Required for incremental fit indices (BCFI, BTLI,
#'   BNFI). Must have been fitted with `test != "none"`.
#' @param h1.model Ignored (included for compatibility with the lavaan
#'   generic).
#' @param fm.args Ignored (included for compatibility with the lavaan
#'   generic).
#' @param output Ignored (included for compatibility with the lavaan
#'   generic).
#' @param ... Additional arguments. Currently supports:
#' \describe{
#'   \item{`rescale`}{Character string controlling how the Bayesian chi-square
#'     is computed, following `blavaan::blavFitIndices()`. Options are `"devM"`
#'     (default) which uses the deviance rescaled by `pD` from DIC, or `"MCMC"`
#'     which uses the classical chi-square (`(N-1) * F_ML`) and classical
#'     degrees of freedom (`p - npar`) at each posterior sample.}
#' }
#'
#' @returns A named numeric vector of fit measures.
#'
#' @importMethodsFrom lavaan fitMeasures
#' @rdname fitMeasures
#' @export
setMethod("fitMeasures", "INLAvaan", inlav_fit_measures)

#' @importMethodsFrom lavaan fitmeasures
#' @rdname fitMeasures
#' @export
setMethod("fitmeasures", "INLAvaan", inlav_fit_measures)
