# --- Bayesian fit index helpers -----------------------------------------------

# Saturated log-likelihood (constant for ML; sum across groups). lavaan
# stores it in the fit's h1 slot for every model type it supports, including
# two-level and missing-data fits, so that is read first. The single-level
# formulas below remain as a fallback for objects without an h1 slot. Under
# FIML they use the pattern-based formula to stay on the same scale as
# inlav_model_loglik (which delegates to lavaan___lav_model_loglik).
compute_loglik_sat <- function(object, lavsamplestats, lavdata) {
  h1 <- tryCatch(object@h1$logl$loglik, error = function(e) NULL)
  if (is.numeric(h1) && length(h1) == 1L && is.finite(h1)) {
    return(h1)
  }
  # nocov start
  ngroups <- lavdata@ngroups
  logl_sat <- 0
  for (g in seq_len(ngroups)) {
    if (lavsamplestats@missing.flag) {
      # nocov start
      logl_sat <- logl_sat +
        lavaan___lav_mvn_mi_loglik_samp(
          yp = lavsamplestats@missing[[g]],
          mu = lavsamplestats@mean[[g]],
          sigma_1 = lavsamplestats@cov[[g]],
          x_idx = lavsamplestats@x.idx[[g]],
          x_mean = lavsamplestats@mean.x[[g]],
          x_cov = lavsamplestats@cov.x[[g]]
        )
    } else {
      # nocov end
      logl_sat <- logl_sat +
        lavaan___lav_mvn_loglik_samp(
          sample_mean = lavsamplestats@mean[[g]],
          sample_cov = lavsamplestats@cov[[g]],
          sample_nobs = lavsamplestats@nobs[[g]],
          mu = lavsamplestats@mean[[g]],
          sigma_1 = lavsamplestats@cov[[g]],
          x_idx = lavsamplestats@x.idx[[g]],
          x_mean = lavsamplestats@mean.x[[g]],
          x_cov = lavsamplestats@cov.x[[g]]
        )
    }
  }
  logl_sat
  # nocov end
}

# Per-sample deviance chi-square:  chisq_s = 2 * (loglik_sat - loglik(x_s))
# This equals N * F_ML(x_s), so both logliks profile the saturated means of a
# model without a mean structure. With loglik_samp (the log-likelihoods of the
# draws, as score_draws() keeps them) x_samp is not used and nothing is scored.
compute_chisq_dev <- function(
  object,
  x_samp,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavoptions,
  lavcache,
  loglik_sat = NULL,
  loglik_samp = NULL
) {
  if (is.null(loglik_sat)) {
    loglik_sat <- compute_loglik_sat(object, lavsamplestats, lavdata)
  }
  if (!is.null(loglik_samp)) {
    return(2 * (loglik_sat - loglik_samp))
  }
  fiml <- fiml_setup(lavmodel, lavsamplestats, lavdata)
  multilevel <- multilevel_setup(
    lavmodel,
    lavsamplestats,
    lavdata,
    lavcache
  )
  vapply(
    seq_len(nrow(x_samp)),
    function(i) {
      ll_i <- inlav_model_loglik(
        x_samp[i, ],
        lavmodel,
        lavsamplestats,
        lavdata,
        lavoptions,
        lavcache,
        marginalise_means = FALSE,
        fiml = fiml,
        multilevel = multilevel
      )
      2 * (loglik_sat - ll_i)
    },
    numeric(1)
  )
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
compute_BCFI <- function(nonc, nonc_null) {
  out <- 1 - nonc / nonc_null
  # A baseline with no noncentrality leaves nothing to scale by. Such a draw
  # takes lavaan's CFI convention: 1 when the model has none either, else 0.
  zero <- !(nonc_null > 0)
  out[zero] <- as.numeric(nonc[zero] <= 0)
  out
}
compute_BTLI <- function(adj_dev, df, adj_dev_null, df_null) {
  tli_null <- adj_dev_null / df_null
  denom <- tli_null - 1
  out <- (tli_null - adj_dev / df) / denom
  # A baseline whose own ratio is 1 leaves nothing to scale by
  out[abs(denom) < 1e-8] <- NA_real_
  out
}
compute_BNFI <- function(adj_dev, adj_dev_null) {
  (adj_dev_null - adj_dev) / adj_dev_null
}

# Number of sample moments, counted as lavaan counts them for the degrees of
# freedom: per group and level, without the moments of fixed exogenous
# covariates or of composite indicator (co)variances fixed at their sample
# values. lavaan's own count removes the fixed composite rows of every group
# from each group, so a fit with composites is counted one group at a time on
# single-group parameter tables. That stays right if lavaan comes to count them
# per group. lavaan also removes an indicator (co)variance that the user fixes
# at a value, which is a constraint the data test, so those count again.
# Composites are single-level, so their blocks are the groups.
count_sample_moments <- function(pt) {
  if (!any(pt$op == "<~") || length(unique(pt$level)) > 1L) {
    return(lavaan::lav_partable_ndat(pt))
  }
  n_row <- length(pt$lhs)
  blocks <- sort(unique(pt$block[pt$block > 0L]))
  per_group <- vapply(
    blocks,
    function(b) {
      in_b <- pt$block == b
      pt_b <- lapply(pt, function(col) {
        if (length(col) == n_row) col[in_b] else col
      })
      pt_b$block[] <- 1L
      if (!is.null(pt_b$group)) {
        pt_b$group[] <- 1L
      }
      ind <- unique(pt_b$rhs[pt_b$op == "<~"])
      user_fixed <- pt_b$op == "~~" &
        pt_b$lhs %in% ind &
        pt_b$rhs %in% ind &
        pt_b$free == 0L &
        !is.na(pt_b$ustart)
      lavaan::lav_partable_ndat(pt_b) + sum(user_fixed)
    },
    numeric(1)
  )
  sum(per_group)
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
  object,
  x_samp,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavoptions,
  lavcache,
  p,
  rescale,
  loglik_sat = NULL,
  loglik_samp = NULL
) {
  chisq_dev <- compute_chisq_dev(
    object,
    x_samp,
    lavmodel,
    lavsamplestats,
    lavdata,
    lavoptions,
    lavcache,
    loglik_sat,
    loglik_samp
  )
  rescale_chisq(
    chisq_dev,
    object@external$inlavaan_internal$DIC$pD,
    object@Fit@npar,
    p,
    lavsamplestats@ntotal,
    lavdata@ngroups,
    rescale
  )
}

# The rescaled quantities of compute_rescaled_quantities() from the per-draw
# chi-squares chisq_dev of a model with pD, npar free parameters, p sample
# moments, N observations and Ngr groups.
rescale_chisq <- function(chisq_dev, pD, npar, p, N, Ngr, rescale) {
  if (rescale == "devM") {
    if (is.null(pD) || pD <= 0 || pD >= p) {
      pD <- npar
    } # nocov
    adj_dev <- chisq_dev - pD # obs - reps
    df <- p - pD
    N_adj <- N
  } else {
    # MCMC: use classical chi-square = (N-1)/N * deviance
    pD <- npar
    chisq_dev <- (N - 1) / N * chisq_dev
    adj_dev <- chisq_dev # reps = 0
    df <- p - npar
    N_adj <- N - Ngr # EQS-style: Min1 = TRUE
  }

  nonc <- pmax(adj_dev - df, 0)
  list(
    chisq = chisq_dev,
    adj_dev = adj_dev,
    df = df,
    nonc = nonc,
    N_adj = N_adj,
    pD = pD
  )
}

# The log-likelihoods of the first nsamp posterior draws of a fit, as
# score_draws() keeps them with its DIC. The draws are independent, so the
# first nsamp of them are a sample of that size. NULL when the fit has fewer
# than nsamp of them (or none: no DIC, or a fit made before they were kept).
stored_draw_loglik <- function(int, nsamp) {
  ll <- int$DIC$loglik_samp
  if (is.null(ll) || length(ll) < nsamp) {
    return(NULL)
  }
  ll[seq_len(nsamp)]
}

# ---------------------------------------------------------------------------
# Independence baseline for the incremental indices
# ---------------------------------------------------------------------------

# Keys of the free parameters of a parameter table, for structural equality
free_param_keys <- function(pt) {
  i <- pt$free > 0
  grp <- pt$group %||% rep(1L, length(pt$lhs))
  lvl <- pt$level %||% rep(1L, length(pt$lhs))
  sort(paste(pt$lhs[i], pt$op[i], pt$rhs[i], grp[i], lvl[i]))
}

# TRUE when every free parameter is a variance or an intercept, which is
# what the independence model consists of
is_independence_partable <- function(pt) {
  i <- pt$free > 0
  all((pt$op[i] == "~~" & pt$lhs[i] == pt$rhs[i]) | pt$op[i] == "~1")
}

# Fit the independence (null) model that the incremental indices BCFI, BTLI
# and BNFI are scaled against, on the same data and likelihood options as
# `object`. lavaan writes that model's parameter table, and the fitted
# object's data and sample-statistics slots are reused directly, so no data
# frame is re-read. The indices need only this model's posterior draws and
# its pD, so the marginals are Gaussian and the VB shift is skipped, which
# makes the fit take a fraction of a second even for many items.
fit_independence_baseline <- function(object, nsamp = NULL) {
  int <- object@external$inlavaan_internal
  pt0 <- lavaan::lav_partable_independence(object)
  opt <- object@Options
  inlavaan(
    model = pt0,
    data = NULL,
    slotData = object@Data,
    slotSampleStats = object@SampleStats,
    missing = opt$missing %||% "default",
    meanstructure = opt$meanstructure %||% "default",
    fixed.x = opt$fixed.x %||% "default",
    conditional.x = opt$conditional.x %||% "default",
    likelihood = opt$likelihood %||% "default",
    estimator = int$lavmodel@estimator,
    marginal_method = "gaussian",
    vb_correction = "none",
    test = "dic",
    nsamp = nsamp %||% int$nsamp %||% 1000L,
    silent = TRUE
  )
}

# What bfit_indices() needs of a baseline fit: the log-likelihoods of its
# draws (NULL for a fit that did not keep them), its saturated log-likelihood,
# pD, number of free parameters, number of sample moments, number of
# observations and number of groups. A few kilobytes, so it can be kept with
# the fitted model. compare() passes it to fitMeasures() as baseline.model.
baseline_record <- function(baseline) {
  bint <- baseline@external$inlavaan_internal
  rec <- list(
    loglik_samp = bint$DIC$loglik_samp,
    loglik_sat = compute_loglik_sat(
      baseline,
      bint$lavsamplestats,
      bint$lavdata
    ),
    pD = bint$DIC$pD,
    npar = baseline@Fit@npar,
    p = count_sample_moments(baseline@ParTable),
    N = bint$lavsamplestats@ntotal,
    Ngr = bint$lavdata@ngroups
  )
  structure(rec, class = "inlavaan_baseline_record")
}

# The baseline record of the independence model of `object`, for nsamp draws.
# The baseline is fitted with as many draws as the fit has, and its record is
# kept in the fit's cache (an environment, so that a call can fill it although
# S4 objects are copied), so later calls neither fit nor score it again. More
# draws than the fit has need a new baseline fit, which is not kept. Fits made
# before the cache have none, and fit the baseline at each call.
independence_record <- function(object, nsamp) {
  cache <- object@external$cache
  n_fit <- object@external$inlavaan_internal$nsamp %||% nsamp
  if (nsamp > n_fit) {
    return(baseline_record(fit_independence_baseline(object, nsamp)))
  }
  if (is.environment(cache) && !is.null(cache$independence)) {
    return(cache$independence)
  }
  rec <- baseline_record(fit_independence_baseline(object, n_fit))
  if (is.environment(cache)) {
    cache$independence <- rec
  }
  rec
}

# Resolve the `baseline.model` argument of bfit_indices(): NULL gives the
# record of the independence model (none for a model that already is one, see
# independence_record()), FALSE skips the incremental indices, a supplied
# fit is checked and returned, and so is a baseline record (from compare()).
resolve_baseline_model <- function(object, baseline.model, nsamp = NULL) {
  if (isFALSE(baseline.model)) {
    return(NULL)
  }
  if (is.null(baseline.model)) {
    if (is_independence_partable(object@ParTable)) {
      return(NULL)
    }
    return(tryCatch(
      independence_record(object, nsamp),
      error = function(e) {
        cli_warn(c(
          "Could not fit the independence baseline, so BCFI, BTLI and BNFI
           are not reported.",
          "x" = conditionMessage(e)
        ))
        NULL
      }
    ))
  }
  if (inherits(baseline.model, "inlavaan_baseline_record")) {
    return(baseline.model)
  }
  if (!is(baseline.model, "INLAvaan")) {
    cli_abort(
      "{.arg baseline.model} must be an {.cls INLAvaan} object, or
       {.code FALSE} to skip the incremental indices."
    )
  }
  if (
    identical(
      free_param_keys(object@ParTable),
      free_param_keys(baseline.model@ParTable)
    )
  ) {
    cli_warn(
      "{.arg baseline.model} has the same free parameters as {.arg object},
       so BCFI, BTLI and BNFI are zero by construction."
    )
  }
  baseline.model
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
#' For a random-slope model, which has no saturated model, the chi-square and
#' the number of sample moments come from the unrestricted random-coefficient
#' model with the same random-effects design, fitted by maximum likelihood.
#' This reference is INLAvaan's own construction. It is available on the
#' closed-form route only, for models without between-only outcomes, and
#' needs enough clusters for its parameters.
#'
#' @param object An object of class [INLAvaan].
#' @param baseline.model The baseline (null) model that the incremental fit
#'   indices (BCFI, BTLI, BNFI) are scaled against. `NULL` (default) fits the
#'   independence model on the same data and options automatically, as
#'   lavaan does: every observed variable keeps its variance (and intercept)
#'   and nothing correlates. That fit uses Gaussian marginals and no VB
#'   shift, since only its posterior draws and pD are needed. It is fitted at
#'   the first call and kept with `object`, so later calls (and
#'   [fitmeasures()]) reuse it and their incremental indices are the same.
#'   Supply an [INLAvaan] object to use another baseline, or `FALSE` to skip
#'   the incremental indices.
#' @param rescale Character string controlling how the Bayesian chi-square
#'   is rescaled. `"devM"` (default) subtracts pD from the deviance at each
#'   sample. `"MCMC"` uses the classical chi-square and classical df at each
#'   sample.
#' @param nsamp Number of posterior samples, by default the number of draws
#'   of the fit. The fit keeps the log-likelihood of each of its draws with
#'   its DIC, so up to that number the indices use the first `nsamp` draws of
#'   the fit: nothing is drawn or scored again, and the absolute indices do
#'   not depend on the random seed. The independence baseline is used the same
#'   way. A larger value draws that many new samples.
#' @param samp_copula `r badge_deprecated()` Has no effect. The draws always
#'   couple the fitted marginals by a Gaussian copula, as the draws of the fit
#'   do.
#' @param x An object of class `bfit_indices` (for `print`).
#' @param ... Additional arguments passed to methods.
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
#' @seealso [lavaan::fitMeasures()], [blavaan::blavFitIndices()],
#'   [fitmeasures()], [compare()]
#'
#' @examples
#' \donttest{
#' HS.model <- "
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' "
#' utils::data("HolzingerSwineford1939", package = "lavaan")
#' fit <- acfa(HS.model, HolzingerSwineford1939, std.lv = TRUE, nsamp = 100,
#'             silent = TRUE)
#'
#' # Absolute fit indices
#' bf <- bfit_indices(fit)
#' bf
#' summary(bf)
#' }
#'
#' @export
bfit_indices <- function(
  object,
  baseline.model = NULL,
  rescale = c("devM", "MCMC"),
  nsamp = NULL,
  samp_copula = NULL
) {
  rescale <- match.arg(rescale)
  if (!is(object, "INLAvaan")) {
    cli_abort("{.arg object} must be an {.cls INLAvaan} object.")
  }
  warn_samp_copula(samp_copula)

  int <- check_fit_layout(object@external$inlavaan_internal, "bfit_indices")
  lavmodel <- int$lavmodel
  lavsamplestats <- int$lavsamplestats
  lavdata <- int$lavdata

  # A random-slope fit has no saturated model, so it is scaled against the
  # unrestricted random-coefficient model instead (see rs_baseline_fit()).
  rs_ref <- NULL
  if (has_random_slopes(lavmodel)) {
    rs_ref <- rs_baseline_fit(object)
  }

  # The fit kept the log-likelihood of each of its draws (score_draws()), so
  # the indices use the first nsamp of those draws and score nothing. More
  # draws than the fit has, or a fit made before they were kept, draws anew.
  nsamp <- nsamp %||% int$nsamp %||% 500L
  ll_samp <- stored_draw_loglik(int, nsamp)
  x_samp <- if (is.null(ll_samp)) sample_params_posterior(int, nsamp)$x_samp
  n_draws <- if (is.null(ll_samp)) nrow(x_samp) else length(ll_samp)

  if (lavmodel@estimator != "ML") {
    # nocov
    cli_abort("Bayesian fit indices are only supported for the ML estimator.")
  }
  if (rescale == "devM" && !has_test(int, "dic")) {
    cli_abort(
      "DIC not available. Refit with {.arg test} including {.val dic}
       (part of the default {.val standard}), or use
       {.code rescale = \"MCMC\"}."
    )
  }

  lavoptions <- reconstruct_lavoptions(object)
  lavcache <- object@Cache
  N <- lavsamplestats@ntotal
  Ngr <- lavdata@ngroups
  nvar <- lavmodel@nvar
  # Number of sample moments, counted as lavaan counts them for the model's
  # degrees of freedom (see count_sample_moments()).
  p <- if (is.null(rs_ref)) {
    count_sample_moments(object@ParTable)
  } else {
    rs_ref$npar
  }

  rq <- compute_rescaled_quantities(
    object,
    x_samp,
    lavmodel,
    lavsamplestats,
    lavdata,
    lavoptions,
    lavcache,
    p,
    rescale,
    rs_ref$loglik,
    ll_samp
  )

  indices <- list()

  if (rq$df > 0) {
    indices$BRMSEA <- compute_BRMSEA(rq$nonc, rq$df, rq$N_adj, Ngr)
    bgh <- compute_BGammaHat(rq$nonc, sum(nvar), rq$N_adj)
    indices$BGammaHat <- bgh
    indices$adjBGammaHat <- compute_adjBGammaHat(bgh, p, rq$df)
    indices$BMc <- compute_BMc(rq$nonc, rq$N_adj)
  } # else: df == 0, no absolute fit indices (nocov – saturated model)

  # Incremental indices, scaled against the independence model unless the
  # caller supplies a baseline or asks to skip them
  baseline <- resolve_baseline_model(object, baseline.model, nsamp)
  if (!is.null(baseline)) {
    rec <- if (is(baseline, "INLAvaan")) baseline_record(baseline) else baseline
    if (!is.null(rec$loglik_samp)) {
      # The log-likelihoods the baseline fit kept, as for the model
      n_use <- min(n_draws, length(rec$loglik_samp))
      chisq_null <- 2 *
        ((rs_ref$loglik %||% rec$loglik_sat) - rec$loglik_samp[seq_len(n_use)])
      # The baseline's own moment count, which is larger than the model's when
      # the model fixes composite indicator (co)variances at their sample
      # values.
      rq_null <- rescale_chisq(
        chisq_null,
        rec$pD,
        rec$npar,
        if (is.null(rs_ref)) rec$p else p,
        rec$N,
        rec$Ngr,
        rescale
      )
    } else {
      # A supplied baseline that kept no log-likelihoods: draw and score
      bint <- baseline@external$inlavaan_internal
      x_samp_null <- sample_params_posterior(bint, nsamp)$x_samp
      n_use <- min(n_draws, nrow(x_samp_null))
      rq_null <- compute_rescaled_quantities(
        baseline,
        x_samp_null[seq_len(n_use), , drop = FALSE],
        bint$lavmodel,
        bint$lavsamplestats,
        bint$lavdata,
        reconstruct_lavoptions(baseline),
        baseline@Cache,
        if (is.null(rs_ref)) rec$p else p,
        rescale,
        rs_ref$loglik
      )
    }

    adj_dev_use <- rq$adj_dev[seq_len(n_use)]
    nonc_use <- rq$nonc[seq_len(n_use)]

    indices$BCFI <- compute_BCFI(nonc_use, rq_null$nonc)
    indices$BTLI <- compute_BTLI(
      adj_dev_use,
      rq$df,
      rq_null$adj_dev,
      rq_null$df
    )
    indices$BNFI <- compute_BNFI(adj_dev_use, rq_null$adj_dev)
  }

  structure(
    list(
      indices = indices,
      details = list(
        chisq = rq$chisq,
        df = rq$df,
        pD = rq$pD,
        rescale = rescale,
        nsamp = n_draws
      )
    ),
    class = "bfit_indices"
  )
}

#' @rdname bfit_indices
#' @exportS3Method summary bfit_indices
#' @usage \method{summary}{bfit_indices}(object, ...)
summary.bfit_indices <- function(object, ...) {
  summ_one <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 3) {
      # nocov
      return(rep(NA_real_, 8))
    }
    dens <- stats::density(x)
    c(
      Mean = mean(x),
      SD = stats::sd(x),
      `2.5%` = stats::quantile(x, 0.025, names = FALSE),
      `25%` = stats::quantile(x, 0.250, names = FALSE),
      `50%` = stats::quantile(x, 0.500, names = FALSE),
      `75%` = stats::quantile(x, 0.750, names = FALSE),
      `97.5%` = stats::quantile(x, 0.975, names = FALSE),
      Mode = dens$x[which.max(dens$y)]
    )
  }
  tab <- as.data.frame(do.call(rbind, lapply(object$indices, summ_one)))
  attr(tab, "header") <- paste0(
    "Posterior summary of ",
    object$details$rescale,
    "-based Bayesian fit indices (nsamp = ",
    object$details$nsamp,
    "):"
  )
  class(tab) <- c("lavaan.data.frame", "data.frame")
  tab
}

#' @rdname bfit_indices
#' @exportS3Method print bfit_indices
print.bfit_indices <- function(x, ...) {
  tab <- summary(x)
  hdr <- attr(tab, "header")
  if (!is.null(hdr)) {
    cat(hdr, "\n\n")
  }
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
  check_fit_layout(object@external$inlavaan_internal, "fitMeasures")
  dots <- list(...)
  rescale <- match.arg(dots$rescale %||% "devM", c("devM", "MCMC"))

  # Has the model converged?
  if (object@Fit@npar > 0L & !object@optim$converged) {
    # nocov
    cli_alert_warning("Optimiser did not converge.") # nocov
  } # nocov

  out <- vector("numeric")
  out["npar"] <- object@Fit@npar
  out["margloglik"] <- object@external$inlavaan_internal$mloglik

  int <- object@external$inlavaan_internal
  if (has_test(int, "ppp")) {
    out["ppp"] <- int$ppp
  }
  if (has_test(int, "dic")) {
    out["dic"] <- int$DIC$dic
    out["p_dic"] <- int$DIC$pD
  }

  # Validate baseline.model early (before tryCatch)
  if (
    !is.null(baseline.model) &&
      !isFALSE(baseline.model) &&
      !is(baseline.model, "INLAvaan") &&
      !inherits(baseline.model, "inlavaan_baseline_record")
  ) {
    cli_abort(
      "{.arg baseline.model} must be an {.cls INLAvaan} object, or
       {.code FALSE} to skip the incremental indices."
    )
  }

  # The independence baseline is fitted only when an incremental index is
  # actually wanted, so a request for absolute indices alone stays free
  incr_measures <- c("BCFI", "BTLI", "BNFI")
  need_incr <- identical(fit.measures, "all") ||
    any(incr_measures %in% fit.measures)
  if (is.null(baseline.model) && !need_incr) {
    baseline.model <- FALSE
  }

  # Bayesian fit indices (BRMSEA, BGammaHat, etc.), only when wanted. A
  # random-slope fit that cannot have them says why when one is asked for by
  # name.
  bfit_names <- c(
    "BRMSEA",
    "BGammaHat",
    "adjBGammaHat",
    "BMc",
    "BCFI",
    "BTLI",
    "BNFI"
  )
  named_bfit <- !identical(fit.measures, "all") &&
    any(bfit_names %in% fit.measures)
  bfi <- NULL
  if (identical(fit.measures, "all") || named_bfit) {
    bfi <- tryCatch(
      bfit_indices(object, baseline.model, rescale),
      error = function(e) {
        if (inherits(e, "inlavaan_rs_bfit") && named_bfit) {
          stop(e)
        }
        NULL
      }
    )
  }
  if (!is.null(bfi)) {
    for (nm in names(bfi$indices)) {
      out[nm] <- mean(bfi$indices[[nm]], na.rm = TRUE)
    }
  }

  # LOO measures: free when stored with the fit (test = "loo"/"waic"/"full",
  # or add_loo()); otherwise computed on demand, and only when requested by
  # name -- the LOO
  # computation is fresh work and cannot be cached into `object` (S4 copy
  # semantics), so it never silently inflates a bare fitMeasures() call
  loo_measures <- c("elpd_loo", "se_loo", "p_loo", "looic")
  res_loo <- object@external$inlavaan_internal$loo
  if (
    is.null(res_loo) &&
      !identical(fit.measures, "all") &&
      any(loo_measures %in% fit.measures)
  ) {
    res_loo <- tryCatch(loo(object), error = function(e) NULL)
  }
  if (!is.null(res_loo)) {
    out["elpd_loo"] <- res_loo$estimates["elpd_loo", "Estimate"]
    out["p_loo"] <- res_loo$estimates["p_loo", "Estimate"]
    out["looic"] <- res_loo$estimates["looic", "Estimate"]
    out["se_loo"] <- res_loo$estimates["looic", "SE"]
    # The order used is a property of the numbers, not of when they were
    # computed. A stored LOO warned at fit time, so without this a reloaded
    # fit would hand back first-order measures without a word.
    n_loo <- res_loo$n_units
    if (isTRUE(res_loo$second_order) && res_loo$n_ok < n_loo) {
      n_bad <- n_loo - res_loo$n_ok
      cli_warn(c(
        "{n_bad} of {n_loo} units {qty(n_bad)}{?has/have} no second-order
         term ({.field k_max} at or above 1).",
        "i" = "{.field elpd_loo}, {.field p_loo} and {.field looic} are
               reported at first order. See {.fun loo}."
      ))
    }
  }

  # WAIC: free when stored with the fit; otherwise computed on demand (a
  # full Taylor pass) when requested by name
  waic_measures <- c("elpd_waic", "se_waic", "p_waic", "waic")
  res_waic <- object@external$inlavaan_internal$waic
  if (
    is.null(res_waic) &&
      !identical(fit.measures, "all") &&
      any(waic_measures %in% fit.measures)
  ) {
    res_waic <- tryCatch(waic(object), error = function(e) NULL)
  }
  if (!is.null(res_waic)) {
    out["elpd_waic"] <- res_waic$estimates["elpd_waic", "Estimate"]
    out["p_waic"] <- res_waic$estimates["p_waic", "Estimate"]
    out["waic"] <- res_waic$estimates["waic", "Estimate"]
    out["se_waic"] <- res_waic$estimates["waic", "SE"]
  }

  # Filter if specific measures requested
  if (!identical(fit.measures, "all")) {
    idx <- which(names(out) %in% fit.measures)
    if (length(idx) == 0L) {
      if (has_random_slopes(int$lavmodel)) {
        cli_abort(
          c(
            "None of {.val {fit.measures}} is available for a random-slope
             model.",
            "i" = "Available: {.val {names(out)}}."
          ),
          class = "inlavaan_rs_fitmeasures"
        )
      }
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
      # nocov start
      # Use formatC to force scientific and maintain 3 significant digits
      return(formatC(val, digits = 2, format = "e"))
    } else {
      # nocov end
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
#' @param fit_measures If `"all"`, all fit measures available will be returned. If
#'   only a single or a few fit measures are specified by name, only those are
#'   computed and returned. The LOO measures `"elpd_loo"`, `"se_loo"`,
#'   `"p_loo"` and `"looic"` (see [loo()]), and the WAIC measures
#'   `"elpd_waic"`, `"se_waic"`, `"p_waic"` and `"waic"` (see [waic()]), are
#'   included in `"all"` only when stored with the fit (`test` including
#'   `"loo"`, `"waic"` or `"full"` in [inlavaan()], or [add_loo()]);
#'   otherwise they are computed on demand when requested by name, and
#'   recomputed on every call -- store the result with `fit <- add_loo(fit)`
#'   (or call [loo()]/[waic()] directly) for repeated access. INLAvaan's
#'   stable spelling `fit.measures` is also accepted.
#' @param baseline_model The baseline (null) model for the incremental fit
#'   indices (BCFI, BTLI, BNFI). `NULL` (default) fits the independence model
#'   automatically, as lavaan does. Supply an [INLAvaan] object to use
#'   another baseline, or `FALSE` to skip the incremental indices. INLAvaan's
#'   stable spelling `baseline.model` is also accepted; see [bfit_indices()].
#' @param h1_model Ignored (included for compatibility with the lavaan
#'   generic).
#' @param fm_args Ignored (included for compatibility with the lavaan
#'   generic).
#' @param output Ignored (included for compatibility with the lavaan
#'   generic).
#' @param level Ignored (included for compatibility with the lavaan
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
#' @seealso [bfit_indices()], [compare()], [diagnostics()]
#'
#' @examples
#' \donttest{
#' HS.model <- "
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' "
#' utils::data("HolzingerSwineford1939", package = "lavaan")
#' fit <- acfa(HS.model, HolzingerSwineford1939, std.lv = TRUE, nsamp = 100,
#'             silent = TRUE)
#'
#' # All available fit measures
#' fitMeasures(fit)
#'
#' # Specific measures
#' fitMeasures(fit, c("npar", "dic", "p_dic", "ppp"))
#' }
#'
#' @usage
#' \S4method{fitMeasures}{INLAvaan}(object, fit_measures = "all",
#'   baseline_model = NULL, h1_model = NULL, fm_args,
#'   output = "vector", level = NULL, ...)
#'
#' @importMethodsFrom lavaan fitmeasures fitMeasures
#' @name fitmeasures
#' @rdname fitmeasures
#' @aliases fitMeasures,INLAvaan-method
#' @rawNamespace exportMethods(fitMeasures)
NULL

#' @usage
#' \S4method{fitmeasures}{INLAvaan}(object, fit_measures = "all",
#'   baseline_model = NULL, h1_model = NULL, fm_args,
#'   output = "vector", level = NULL, ...)
#'
#' @name fitmeasures
#' @rdname fitmeasures
#' @aliases fitmeasures,INLAvaan-method
#' @rawNamespace exportMethods(fitmeasures)
NULL

# With lavaan >= 0.7 required, the generics' argument spellings are fixed
# (fit_measures, baseline_model), so the methods can be registered the
# ordinary way at build time; the load-time re-registration this replaced
# existed only to serve both lavaan generations at once.
#
# INLAvaan's own documented/stable parameter names remain fit.measures and
# baseline.model (used internally, e.g. by compare()). Those don't match the
# generic's formals, so a caller using them falls through into "..."
# unmatched rather than being silently dropped; recover them from there.
inlav_fitmeasures_method <- function(
  object,
  fit_measures = "all",
  baseline_model = NULL,
  h1_model = NULL,
  fm_args = list(
    standard.test = "default",
    scaled.test = "default",
    rmsea.ci.level = 0.90,
    rmsea.close.h0 = 0.05,
    rmsea.notclose.h0 = 0.08,
    robust = TRUE,
    cat.nonpd = "na"
  ),
  output = "vector",
  level = NULL,
  ...
) {
  dots <- list(...)
  if ("fit.measures" %in% names(dots)) {
    fit_measures <- dots[["fit.measures"]]
    dots[["fit.measures"]] <- NULL
  }
  if ("baseline.model" %in% names(dots)) {
    baseline_model <- dots[["baseline.model"]]
    dots[["baseline.model"]] <- NULL
  }
  do.call(
    inlav_fit_measures,
    c(
      list(
        object,
        fit.measures = fit_measures,
        baseline.model = baseline_model
      ),
      dots
    )
  )
}

setMethod("fitMeasures", "INLAvaan", inlav_fitmeasures_method)
setMethod("fitmeasures", "INLAvaan", inlav_fitmeasures_method)
