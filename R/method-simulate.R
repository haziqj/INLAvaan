#' Simulate Datasets from the Generative Model
#'
#' Generate complete synthetic datasets from a fitted INLAvaan model. For each
#' simulation, a single parameter vector is drawn (from the posterior or prior),
#' and then `sample.nobs` observations are generated from the model-implied
#' distribution at that parameter value.
#'
#' @details
#' This function is designed for tasks that require **full replicate datasets**
#' from a single parameter draw, such as simulation-based calibration (SBC) and
#' posterior predictive p-values. It differs from [sampling()] which generates
#' one observation per parameter draw (useful for prior/posterior predictive
#' density overlays).
#'
#' For each simulation \eqn{s = 1, \ldots, S}:
#' 1. Draw \eqn{\boldsymbol\theta^{(s)}} from the posterior (or prior).
#' 2. Compute the model-implied covariance
#'    \eqn{\boldsymbol\Sigma(\boldsymbol\theta^{(s)})}.
#'    If it is not positive-definite, reject and redraw.
#' 3. Generate a dataset of `sample.nobs` rows from
#'    \eqn{N(\boldsymbol\mu(\boldsymbol\theta^{(s)}),\,
#'    \boldsymbol\Sigma(\boldsymbol\theta^{(s)}))}.
#'
#' Parameter draws reuse the same internal machinery as [sampling()]
#' (`sample_params_prior` / `sample_params_posterior`), so the prior
#' specification is consistent.
#'
#' @param object An object of class [INLAvaan].
#' @param nsim Number of replicate datasets to generate (default 1).
#' @param seed Optional random seed (passed to [set.seed()]).
#' @param sample.nobs Number of observations per dataset. Defaults to the
#'   sample size of the original data.
#' @param prior Logical. When `TRUE`, parameters are drawn from the prior;
#'   when `FALSE` (default), from the posterior.
#' @param samp_copula Logical. When `TRUE` (default) and `prior = FALSE`,
#'   posterior parameter draws use the copula method. Ignored when
#'   `prior = TRUE`.
#' @param ... Additional arguments (currently unused).
#'
#' @returns A list of length `nsim`. Each element is a data frame with
#'   `sample.nobs` rows and two attributes:
#'   * `"truth"` — named numeric vector of lavaan-side (x-space, constrained)
#'     parameter values used to generate the dataset.
#'   * `"truth_theta"` — named numeric vector of the corresponding unconstrained
#'     (theta-space) parameter values.
#'
#' @seealso [sampling()] for single-observation draws from the predictive
#'   distribution (prior/posterior predictive checks).
#'
#' @example inst/examples/ex-simulate.R
#' @name simulate
#' @rdname simulate
#' @importFrom stats simulate rgamma
#' @export
setMethod(
  "simulate",
  "INLAvaan",
  function(
    object,
    nsim = 1L,
    seed = NULL,
    sample.nobs = NULL,
    prior = FALSE,
    samp_copula = TRUE,
    ...
  ) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    nsim <- as.integer(nsim)

    int <- object@external$inlavaan_internal
    pt <- int$partable
    lavmodel <- int$lavmodel

    xnames <- pt$names[pt$free > 0 & !duplicated(pt$free)]
    n <- if (is.null(sample.nobs)) object@SampleStats@ntotal else sample.nobs

    # Draw a generous batch of parameters upfront
    oversample <- nsim * 5L
    if (isTRUE(prior)) {
      samp <- sample_params_prior(int, oversample)
    } else {
      samp <- sample_params_posterior(int, oversample, samp_copula)
    }
    colnames(samp$x_samp) <- xnames
    colnames(samp$theta_samp) <- xnames

    results <- vector("list", nsim)
    collected <- 0L
    idx <- 0L # index into pre-drawn batch
    max_attempts <- nsim * 20L
    attempts <- 0L

    while (collected < nsim && attempts < max_attempts) {
      idx <- idx + 1L
      attempts <- attempts + 1L

      # Refill batch if exhausted
      if (idx > nrow(samp$x_samp)) {
        if (isTRUE(prior)) {
          samp <- sample_params_prior(int, oversample)
        } else {
          samp <- sample_params_posterior(int, oversample, samp_copula)
        }
        colnames(samp$x_samp) <- xnames
        colnames(samp$theta_samp) <- xnames
        idx <- 1L
      }

      x_draw <- samp$x_samp[idx, ]
      theta_draw <- samp$theta_samp[idx, ]

      # Check PD of model-implied covariance
      lavmodel_x <- lavaan::lav_model_set_parameters(
        lavmodel,
        as.numeric(x_draw)
      )
      implied <- lavaan::lav_model_implied(lavmodel_x)

      all_pd <- TRUE
      for (g in seq_along(implied$cov)) {
        eigs <- eigen(
          implied$cov[[g]],
          symmetric = TRUE,
          only.values = TRUE
        )$values
        if (any(eigs < 1e-10)) {
          all_pd <- FALSE
          break
        }
      }
      if (!all_pd) {
        next
      }

      # Generate data via lavaan's simulateData using the partable
      pt_sim <- lavaan::lav_partable_complete(lavaan::partable(object))
      pt_sim$est[pt_sim$free > 0] <- as.numeric(x_draw)

      dat <- tryCatch(
        lavaan::simulateData(pt_sim, sample.nobs = n),
        error = function(e) NULL
      )
      if (is.null(dat)) {
        next
      }

      collected <- collected + 1L
      attr(dat, "truth") <- x_draw
      attr(dat, "truth_theta") <- theta_draw
      results[[collected]] <- dat
    }

    rejected <- attempts - collected
    if (rejected > 0L) {
      rej_pct <- round(100 * rejected / attempts, 1)
      cli_inform(
        "simulate: {rejected} of {attempts} draw{?s} ({rej_pct}%) rejected (non-PD model-implied covariance)."
      )
    }

    if (collected < nsim) {
      cli_warn(c(
        "Rejection sampling fell short of the requested {nsim} simulations.",
        "i" = "Only {collected} of {nsim} obtained after {attempts} attempts.",
        "i" = "Consider using more informative priors."
      ))
      if (collected == 0L) {
        cli_abort("No valid draws obtained. Priors may be too vague.")
      }
      results <- results[seq_len(collected)]
    }

    results
  }
)
