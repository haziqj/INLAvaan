#' Fit an Approximate Bayesian Latent Variable Model
#'
#' This function fits a Bayesian latent variable model by approximating the
#' posterior distributions of the model parameters: it finds the posterior
#' mode, takes the Laplace approximation there, corrects its mean by
#' variational Bayes, and fits a skew-normal or a Gaussian marginal to each
#' parameter. It leverages the lavaan package for model specification and
#' estimation.
#'
#' @inheritParams lavaan::lavaan
#' @inheritParams lavaan::simulateData
#' @inheritParams blavaan::blavaan
#' @param model.type The lavaan entry point used to fit `model`: `"cfa"`,
#'   `"sem"`, or `"growth"` (matching lavaan's model-specific wrapper
#'   functions), or `"lavaan"` for the general-purpose interface. Other
#'   values give an error. Set automatically by [acfa()], [asem()], and
#'   [agrowth()]; documented
#'   explicitly here because lavaan >= 0.7-1 renamed the corresponding
#'   `simulateData()` argument to `model_type`, so it can no longer be
#'   inherited from there.
#'
#' @param dp Default prior distributions for the different types of model
#'   parameters; a named character vector as returned by [priors_for()]. Types
#'   left out take their default priors. A name that is not a type, such as a
#'   misspelt one, gives a warning. `dp = blavaan::dpriors()` works too.
#' @param test Character vector naming the post-estimation quantities to
#'   compute and store with the fit. The atoms are `"ppp"` (posterior
#'   predictive p-value), `"dic"` (deviance information criterion and its
#'   `pD`), `"loo"` (leave-one-out cross-validation, see [loo()]) and
#'   `"waic"` (see [waic()]). Three aliases stand for sets of atoms:
#'   `"standard"` (the default) and its synonym `"default"` give
#'   `c("ppp", "dic")`; `"full"` gives all four; `"none"` gives nothing.
#'   Aliases and atoms may be mixed and are unioned, so
#'   `test = c("standard", "loo")` adds the LOO to the default set. The LOO
#'   and the WAIC come from one Taylor pass, so asking for either stores
#'   both. They run only when asked for, with no time budget. On a model
#'   the casewise machinery does not support (PML or ordinal data,
#'   `conditional.x = TRUE`, multigroup two-level) they are skipped with a
#'   warning and the rest of the fit proceeds. For a two-level model the PPP
#'   follows blavaan: each of `ppp_nsamp` posterior draws generates replicate
#'   data, which are scored against the saturated model (see `ppp_method`).
#'   The two-level PPP is experimental.
#'   The fit records what was requested and what was computed
#'   (`get_inlavaan_internal(fit, "test")`);
#'   [summary()], [fitmeasures()], [deviance()], [logLik()] and [timing()]
#'   report only what was computed. [add_loo()] stores the LOO and WAIC
#'   post hoc; [loo()] and [waic()] compute on demand. For a random-slope
#'   model `"ppp"` is dropped, with a warning when it was asked for and a
#'   message otherwise (see the Random slopes section of [inlavaan()]).
#'   `"dic"`, `"loo"` and `"waic"` are unaffected.
#' @param vb_correction The variational Bayes (VB) correction of the posterior
#'   mean of the Laplace approximation. `"onestep"` (default) takes one step
#'   from the mode, `"full"` iterates the correction to its fixed point, and
#'   `"none"` leaves it out. The correction moves the marginal log-likelihood,
#'   the point at which the DIC is computed, the centre of the LOO and the
#'   WAIC, and the Gaussian marginals of `marginal_method = "gaussian"` and
#'   their draws. It does not move the skew-normal marginals or their draws,
#'   which are scanned from the mode.
#' @param n_qmc `r badge_deprecated()` Has no effect. The VB correction now
#'   takes a deterministic step, with no quasi-Monte Carlo nodes.
#' @param vb_method `r badge_deprecated()` Has no effect. The VB correction now
#'   has one rule, and `vb_correction = "full"` iterates it.
#' @param marginal_method How the marginal posterior of each parameter is
#'   approximated. `"skewnorm"` (default) fits a skew-normal distribution to a
#'   scan of the log-posterior along the parameter (see `marginal_scan`).
#'   `"gaussian"` takes the Gaussian marginal of the Laplace approximation,
#'   centred at the VB mean, which is faster. `r badge_deprecated()` The old
#'   values warn: `"marggaus"` is the old name of `"gaussian"`, and
#'   `"asymgaus"` now runs `"skewnorm"`. `"sampling"` is kept for a sampler
#'   that is under development, and gives an error.
#' @param marginal_correction `r badge_deprecated()` Use `marginal_scan`.
#'   `"none"` and `FALSE` are `marginal_scan = "plain"`, which gives the same
#'   fit. `"shortcut"`, `"shortcut_fd"` and `"hessian"` run
#'   `marginal_scan = "linear"`.
#' @param nsamp The number of samples to draw for all sampling-based approaches
#'   (including posterior sampling for model fit indices).
#' @param samp_copula `r badge_deprecated()` Has no effect. The draws always
#'   couple the fitted marginals by a Gaussian copula. With
#'   `marginal_method = "gaussian"` they are the draws of the Laplace
#'   approximation.
#' @param samp_norta `r badge_deprecated()` Has no effect. The draws no longer
#'   adjust the correlations of the copula.
#' @param cov_as_cor Logical. Residual and latent-disturbance covariance
#'   parameters (`~~` between two observed or two latent variables) are
#'   always estimated on the correlation scale internally (an `atanh` link,
#'   the same as for `std.ov`/`std.lv`-standardised parameters); by default
#'   their reported marginal is then re-derived on the covariance scale
#'   \eqn{\sigma_i \sigma_j \rho} from a posterior sample (see
#'   `nsamp`), because that is the scale lavaan/blavaan report by
#'   default. When `TRUE`, that re-derivation is skipped and each such
#'   parameter's own directly profiled correlation-scale marginal
#'   \eqn{\rho \in (-1, 1)} is reported instead -- useful for comparing the
#'   profiling machinery (skew-normal fit, VB, ...) against a
#'   correlation-scale reference without the sampling/copula step in
#'   between. Model estimation is identical either way; only what is
#'   reported for these parameters changes. The returned parameter table keeps
#'   their covariance classes, so [loo()] and [waic()] after the fit score the
#'   same model, and the setting is stored as
#'   `get_inlavaan_internal(fit)$cov_as_cor`. The point estimates of these
#'   parameters are correlations, so methods that use the point estimates as
#'   model parameters (such as [fitted()] and [residuals()]) are not
#'   meaningful when `TRUE`. Not the same as lavaan's
#'   `std.ov`/`std.lv`, which re-parameterises the whole model on a
#'   standardised scale. Defaults to `FALSE`.
#' @param sn_fit_ngrid `r badge_deprecated()` Use
#'   `control = inlavaan_control(n_grid = )`, which gives the same fit.
#' @param sn_fit_logthresh `r badge_deprecated()` Use
#'   `control = inlavaan_control(log_threshold = )`, which gives the same fit.
#' @param sn_fit_temp `r badge_deprecated()` Has no effect. The skew-normal
#'   marginals are fitted with temperature 1.
#' @param sn_fit_sample Logical. When `TRUE` (default), a parametric skew-normal
#'   is fitted to the posterior samples for covariance and defined parameters.
#'   When `FALSE`, these are summarised using kernel density estimation instead.
#' @param control A list of tuning settings, made by [inlavaan_control()], or a
#'   plain list with some of its names: the limits of the search for the
#'   posterior mode, of the VB correction and of the skew-normal scans, and
#'   the settings `hessian`, `debug` and `add_priors`. The defaults suit most
#'   fits. An unknown name gives an error. The controls of [stats::nlminb()]
#'   that `control` took up to INLAvaan 0.3.5, such as `iter.max`, still work,
#'   with a deprecation warning. See [inlavaan_control()].
#' @param verbose Logical. With `FALSE` (default) the fit prints one line per
#'   stage. With `TRUE` it also prints the details of each stage of the fit on
#'   the internal scale: the start, the mode search, the VB correction and the
#'   marginals, with their checks. Use `silent = TRUE` to print nothing.
#' @param debug `r badge_deprecated()` Use
#'   `control = inlavaan_control(debug = TRUE)`, which gives the same fit.
#' @param add_priors `r badge_deprecated()` Use
#'   `control = inlavaan_control(add_priors = FALSE)`, which gives the same
#'   fit.
#' @param optim_method `r badge_deprecated()` Has no effect. The posterior mode
#'   is always found by [stats::nlminb()].
#' @param numerical_grad `r badge_deprecated()` Has no effect. The fit always
#'   uses the analytic gradient.
#' @param hessian_method `r badge_deprecated()` Use
#'   `control = inlavaan_control(hessian = )`, which gives the same fit.
#' @param start Optional numeric vector of starting values for the optimiser,
#'   given as a full vector of free parameters in the internal (unconstrained)
#'   parameterisation. Mainly for internal use by [update()], which warm-starts
#'   mode-finding from a previous fit's posterior mode when the free
#'   parameters keep their names; supplying a hand-built vector requires
#'   knowledge of the internal parameter ordering. Its length must equal the
#'   number of free parameters or an error is raised.
#' @param cores Integer or `NULL`. The number of cores. With `NULL` (default),
#'   the scans of the skew-normal marginals follow `future::plan()`, which is
#'   serial unless a plan is set, and the LOO and the WAIC asked for in `test`
#'   run serially. With `cores > 1`, the scans run in parallel when the
#'   packages \pkg{future} and \pkg{parallelly} are installed, and serially
#'   otherwise. The LOO and the WAIC are then forked via
#'   [parallel::mclapply()] where that is safe, or run over a PSOCK cluster
#'   (separate R processes) inside IDE R sessions (RStudio, Positron) and on
#'   Windows. `1` is serial.
#' @param ppp_method How the PPP of a two-level model scores the observed and
#'   the replicate data against the saturated model. `"onestep"` (default)
#'   takes one Fisher-scoring step from the moments of each posterior draw
#'   towards the saturated fit, and fits by EM where the step would leave the
#'   valid covariance matrices (with few clusters or a small between
#'   variance). `"em"` always fits the saturated model by EM, as blavaan does.
#'   Fits with `missing = "ml"` always use `"em"`. Ignored for single-level
#'   models.
#' @param ppp_nsamp The number of posterior draws, each with one replicate data
#'   set, that the PPP of a two-level model uses. Defaults to `250`, and is
#'   capped at `nsamp`. Draws that cannot be scored are left out, with a
#'   warning. Ignored for single-level models.
#' @param marginal_scan How the scan behind each skew-normal marginal is taken.
#'   `"linear"` (default) scans along a straight line through the mode, and
#'   adds a linear correction for the changing volume of the other parameters.
#'   `"exact"` computes that volume term exactly at each point of the scan.
#'   `"laplace"` maximises the log-posterior over the other parameters at each
#'   point of the scan, and takes the height and the volume there. `"plain"`
#'   adds no volume correction. On the Holzinger and Swineford and the
#'   Political Democracy models, `"exact"` and `"laplace"` take 10 to 25 times
#'   as long as `"linear"`. Ignored with `marginal_method = "gaussian"`, which
#'   has no scan.
#' @param silent Logical. Print nothing at all, and muffle the warnings and
#'   messages raised during the fit? Default `FALSE`. `verbose` and `silent`
#'   cannot both be `TRUE`. Errors are still raised. Whether or not `silent` is
#'   `TRUE`, the texts of the warnings raised during the fit are recorded in
#'   the field `warnings` of [get_inlavaan_internal()].
#' @param ... Additional arguments to be passed to the [lavaan] model fitting
#'   function.
#'
#' @section Deprecated arguments:
#' The arguments marked deprecated still work in INLAvaan 0.4.0, and stop
#' from 0.5.0. Each warns once per session, with a warning of class
#' `inlavaan_deprecated` that names what to use instead. With
#' `options(inlavaan.deprecation = "error")` they stop now, so that tests can
#' find old calls. Those that moved into `control` give the same fit. The
#' others have no effect. INLAvaan 0.3.5 has the old methods.
#'
#' @section Random slopes:
#' Wrapping a level-1 regression coefficient in lavaan's `rv()` modifier
#' turns it into a level-2 latent variable -- a random slope -- which can
#' then be given a mean, a variance and cross-level regressions like any
#' other level-2 latent variable:
#'
#' ```
#' level: 1
#'   fw =~ y1 + y2 + y3
#'   fw ~ rv('s1')*x1
#' level: 2
#'   fb =~ y1 + y2 + y3
#'   s1 ~ w1
#'   s1 ~~ s1
#'   s1 ~ 1
#' ```
#'
#' The slope's variance and intercept are added automatically, so in
#' practice only the cross-level regression `s1 ~ w1` need be written out.
#' [summary()] marks the level-1 carrier row as `x1 (s1)`: that row is
#' fixed at zero, the slope itself being reported under Level 2.
#'
#' The likelihood takes one of two routes. When the covariate carrying the
#' slope is observed and purely within-cluster, the slope integrates out in
#' closed form. When it is latent, or split across both levels (the same
#' variable entering at level 1 and at level 2), the integral is done by
#' Gauss-Hermite quadrature instead; that route warns at fit time and is
#' governed by `integration.ngh`, passed through to lavaan. The node count
#' is an accuracy setting as much as a cost setting, so give every fit that
#' is to be compared with another the same `integration.ngh`.
#'
#' A model with observed exogenous covariates requires `fixed.x = TRUE`
#' (the default): the likelihood is the density of the outcomes *given*
#' those covariates, so their own means and (co)variances are unidentified
#' and would be reported back as their priors. A `fixed.x = FALSE` fit is
#' refused. A model whose covariates are all latent or modelled has nothing
#' to hold fixed, and lavaan reports `fixed.x = FALSE` for it of its own
#' accord; such a fit is accepted as it stands.
#'
#' Equality constraints work as for other models, except that covariances
#' cannot be held equal. Composites (`<~`) cannot be combined with random
#' slopes yet.
#'
#' A random-slope model implies no single within-cluster covariance matrix:
#' the covariance of the outcomes depends on the covariate values.
#' [fitted()] and [residuals()] therefore use the implied moments averaged
#' over the covariates, which include the mean and the variance of each
#' slope, or with `per_cluster = TRUE` the moments of each cluster at its
#' own covariate values (closed-form route only). [standardisedsolution()]
#' and the standardised columns and R-square of [summary()] are scaled by
#' the averaged variances. The carrier row `x1 (s1)` then gives the
#' standardised mean slope, and the slope's own rows are on the same
#' standardised scale. An outcome observed at level 1 only, whose slope
#' covariate has a non-zero mean, has no place in lavaan's two-level layout,
#' and these outputs abort for it.
#'
#' [simulate()] draws each cluster's slopes and other level-2 effects and
#' then its outcomes, at the cluster's own covariates (closed-form route
#' only). [sampling()] gives latent, observed and implied draws. [predict()]
#' gives `type = "lv"` and, on the closed-form route, the cluster-specific
#' `"yhat"` and `"ypred"` from the empirical Bayes random effects, while
#' `fitted(type = "casewise")` gives the outcomes' means given the
#' covariates. [bfit_indices()] scales the Bayesian fit indices against the
#' unrestricted random-coefficient model with the same random-effects design,
#' because a saturated model does not exist here. This reference is
#' INLAvaan's own construction (closed-form route only).
#'
#' The posterior predictive p-value (`test = "ppp"`) is dropped, because a
#' random-slope model has no saturated model to score replicate data against.
#' What aborts with an explanation: the residual types scaled by standard
#' errors, `predict(type = "ymis")`, and `loo(type = "loso")`, which would
#' need a cluster's sufficient statistics downdated by one row.
#'
#' The model-comparison side works throughout. [compare()] reports the
#' marginal likelihood, Bayes factors, the DIC and its \eqn{p_D}, and [loo()]
#' and [waic()] score the fit leave-one-cluster-out on the conditional
#' likelihood.
#'
#' To ask whether there is a random slope at all, compare the fit with the
#' fixed-slope model (`fw ~ x1` at level 1) using [compare()]. On the
#' closed-form route, fixing the slope variance at zero (`s1 ~~ 0*s1`) and
#' dropping any cross-level regression on the slope gives the same model.
#' Keeping `s1 ~ w1` gives a cross-level interaction model instead. The
#' quadrature route refuses a slope variance fixed at zero.
#'
#' @seealso Typically, users will interact with the specific latent variable
#'   model functions instead, including [acfa()], [asem()], and [agrowth()].
#'
#' @example inst/examples/ex-inlavaan.R
#'
#' @return An S4 object of class `INLAvaan` which is a subclass of the
#'   [lavaan-class] class.
#' @export
inlavaan <- function(
  model,
  data = NULL,
  model.type = "sem",
  dp = priors_for(),
  test = "standard",
  vb_correction = c("onestep", "full", "none"),
  n_qmc = NULL,
  vb_method = NULL,
  marginal_method = c("skewnorm", "gaussian"),
  marginal_correction = NULL,
  nsamp = 1000,
  samp_copula = NULL,
  samp_norta = NULL,
  cov_as_cor = FALSE,
  sn_fit_ngrid = NULL,
  sn_fit_logthresh = NULL,
  sn_fit_temp = NULL,
  sn_fit_sample = TRUE,
  control = list(),
  verbose = FALSE,
  debug = NULL,
  add_priors = NULL,
  optim_method = NULL,
  numerical_grad = NULL,
  hessian_method = NULL,
  start = NULL,
  cores = NULL,
  ppp_method = c("onestep", "em"),
  ppp_nsamp = 250L,
  marginal_scan = c("linear", "exact", "laplace", "plain"),
  silent = FALSE,
  ...
) {
  mc <- match.call()
  mc$start <- NULL # warm start is transient; keep it out of the recorded call
  scan_given <- !missing(marginal_scan)
  start_time0 <- proc.time()[3]
  timing <- list(start_time = start_time0)
  check_flag(verbose, "verbose")
  check_flag(silent, "silent")
  if (verbose && silent) {
    cli_abort("{.arg verbose} and {.arg silent} cannot both be {.code TRUE}.")
  }

  # Every warning raised during the fit, by INLAvaan or by lavaan, is recorded
  # once in the field `warnings` of the fit. With silent = TRUE it is then
  # muffled, and so is every message. Errors pass through either way. The fit
  # is the braced expression, the last argument of withCallingHandlers().
  fit_warnings <- character()
  on_fit_warning <- function(w) {
    msg <- warning_text(w)
    if (!msg %in% fit_warnings) {
      fit_warnings <<- c(fit_warnings, msg)
    }
    if (silent) {
      tryInvokeRestart("muffleWarning")
    }
  }
  on_fit_message <- function(m) {
    if (silent) {
      tryInvokeRestart("muffleMessage")
    }
  }
  withCallingHandlers(warning = on_fit_warning, message = on_fit_message, {
    ## ----- Check arguments ---------------------------------------------------
    check_model_type(model.type)
    # The settings of the fit, with the deprecated arguments mapped to them
    # (see R/inlavaan-args.R). The entries of `control` that are the engine's
    # go to the engine, and INLAvaan reads the others.
    settings <- resolve_inlavaan_args(
      old = list(
        n_qmc = n_qmc,
        vb_method = vb_method,
        marginal_correction = marginal_correction,
        samp_copula = samp_copula,
        samp_norta = samp_norta,
        sn_fit_ngrid = sn_fit_ngrid,
        sn_fit_logthresh = sn_fit_logthresh,
        sn_fit_temp = sn_fit_temp,
        debug = debug,
        add_priors = add_priors,
        optim_method = optim_method,
        numerical_grad = numerical_grad,
        hessian_method = hessian_method
      ),
      vb_correction = vb_correction,
      marginal_method = marginal_method,
      marginal_scan = marginal_scan,
      scan_given = scan_given,
      control = control,
      silent = silent
    )
    ctrl <- settings$control
    engine_entries <- intersect(names(ctrl), names(formals(vanilla_control)))
    engine_control <- ctrl[engine_entries]
    hessian_method <- ctrl$hessian %||% "central"
    debug <- isTRUE(ctrl$debug)
    add_priors <- ctrl$add_priors %||% TRUE
    vb_correction <- settings$vb_correction
    marginal_method <- settings$marginal_method
    marginal_scan <- settings$marginal_scan
    if (!is.null(cores)) {
      # nocov start
      cores <- as.integer(cores)
      if (is.na(cores) || cores < 1L) cores <- 1L
    } # nocov end
    ppp_method <- match.arg(ppp_method)
    if (
      !is.numeric(ppp_nsamp) ||
        length(ppp_nsamp) != 1L ||
        is.na(ppp_nsamp) ||
        ppp_nsamp < 1
    ) {
      cli_abort("{.arg ppp_nsamp} must be a number of at least 1.")
    }
    # `test` is an INLAvaan-only selection of post-estimation quantities (see
    # resolve_test() in R/utils.R); it never reaches lavaan as typed
    test_req <- resolve_test(test)
    # Parameter types missing from dp (in a vector saved before a type existed,
    # say) take their default prior.
    if (length(dp) > 0L && (is.null(names(dp)) || !all(nzchar(names(dp))))) {
      cli_abort(
        "{.arg dp} must be a named vector, as returned by {.fn priors_for}."
      )
    }
    dp_default <- priors_for()
    warn_dp_unused(dp, names(dp_default))
    dp <- c(dp, dp_default[setdiff(names(dp_default), names(dp))])
    # What the user asked for, kept for the `test` record below: `test_req`
    # itself loses any atom that does not exist for this model class (see the
    # random-slope checks further down).
    test_requested <- test_req

    lavargs <- list(...)
    check_data_given(data, lavargs)
    lavargs$model <- split_modifiers(model)
    lavargs$data <- data
    lavargs$ceq.simple <- TRUE # FIXME: Force ceq.simple rather than eq.constraints
    lavargs$verbose <- FALSE # FIXME: Need some quiet mode maybe
    lavargs$do.fit <- FALSE
    lavargs$parser <- "old" # To get priors parsed
    # lavaan's Wishart likelihood (likelihood = "wishart", the default under
    # mimic = "EQS" or "LISREL") divides the sample covariance by n - 1. The
    # likelihood here assumes lavaan's divisor-n covariance, and without a mean
    # structure the saturated means are integrated out, which already gives the
    # Wishart form. So the normal likelihood is always used.
    likelihood_req <- tolower(lavargs$likelihood %||% "default")
    mimic_req <- tolower(lavargs$mimic %||% "default")
    if (
      likelihood_req == "wishart" ||
        (likelihood_req == "default" && mimic_req %in% c("eqs", "lisrel"))
    ) {
      cli_inform(c(
        "i" = "INLAvaan always uses the normal likelihood, so
             {.code likelihood = \"wishart\"} is ignored.",
        " " = "Without a mean structure, integrating out the saturated means
             already gives the Wishart likelihood."
      ))
    }
    lavargs$likelihood <- "normal"
    # The (co)variances of a composite's indicators are parameters with priors,
    # unless the user fixes them at their sample values (composites.cov =
    # "fixed", lavaan's default).
    if (
      !any(c("composites.cov", "composites_cov") %in% names(lavargs)) &&
        has_composite_syntax(lavargs$model)
    ) {
      lavargs$composites.cov <- "free"
    }
    # lavaan only ever sees "standard" or "none": its own test statistics are
    # never computed under do.fit = FALSE, and INLAvaan's atoms ("loo", ...)
    # are not lavaan-legal values
    lavargs$test <- if (length(test_req) > 0L) "standard" else "none"

    if ("estimator" %in% names(lavargs)) {
      if (!(lavargs$estimator %in% c("ML", "PML"))) {
        # nocov
        cli_abort("Only 'ML' and 'PML' estimators are supported currently.")
      }
    }

    # Two-level models cannot drop the mean structure: the between-level
    # statistics are the cluster means, and the marginalised treatment of
    # covariance-only analyses is single-level (and multigroup) only. lavaan
    # force-enables meanstructure for multilevel data anyway; if the user
    # *explicitly* asked for FALSE, say so rather than silently complying.
    if (isFALSE(lavargs$meanstructure) && !is.null(lavargs$cluster)) {
      cli_warn(c(
        "Two-level models require a mean structure; fitting with
       {.code meanstructure = TRUE}.",
        "i" = "See {.code vignette(\"meanstructure\", package = \"INLAvaan\")}
       for how INLAvaan treats mean structures."
      ))
      lavargs$meanstructure <- TRUE
    }

    ## ----- Initialise lavaan object ------------------------------------------
    # lavaan evaluates the := parameters at the start values, where they may be
    # undefined. Any draws where they are undefined are reported later.
    fit0 <- muffle_nan_warnings(muffle_rs_test_warning(
      do.call(get(model.type, envir = asNamespace("lavaan")), lavargs)
    ))
    check_efa_blocks(fit0@ParTable)
    check_composite_scope(fit0)
    # Ordinal variables come from `ordered =`, threshold syntax or
    # ordered-factor columns. Name exactly those the model uses, so that the PML
    # refit (and its kappa scaling) sees all of them and nothing else.
    ord_names <- lavaan::lavNames(fit0, "ov.ord")
    if (length(ord_names) > 0) {
      # Redo automatically with PML if ordinal data
      lavargs$ordered <- ord_names
      lavargs$estimator <- "PML"
      lavargs$parameterization <- "theta"
      lavargs$test <- "none"
      fit0 <- muffle_nan_warnings(
        do.call(get(model.type, envir = asNamespace("lavaan")), lavargs)
      )
    }
    if (!fit0@Model@estimator %in% c("ML", "PML")) {
      cli_abort(c(
        "lavaan chose the {.val {fit0@Model@estimator}} estimator, which INLAvaan
       does not support.",
        "i" = "{.arg ordered} must name endogenous observed variables in the
             model."
      ))
    }
    check_composite_derived(fit0@ParTable)
    check_composite_labels(fit0@ParTable)
    # Rebuild from a table where explicit constraints are shared free indices or
    # fixed values (see pack_constraints()).
    pt_packed <- pack_constraints(fit0@ParTable, fit0@Options$effect.coding)
    if (!is.null(pt_packed)) {
      lavargs$model <- pt_packed
      fit0 <- muffle_nan_warnings(muffle_rs_test_warning(
        do.call(get(model.type, envir = asNamespace("lavaan")), lavargs)
      ))
    }
    check_composite_weights(fit0@ParTable)
    check_composite_covariances(fit0@ParTable)
    lavmodel <- fit0@Model
    lavsamplestats <- fit0@SampleStats
    lavdata <- fit0@Data
    lavoptions <- fit0@Options
    lavpartable <- fit0@ParTable
    lavcache <- fit0@Cache
    n <- fit0@SampleStats@ntotal
    ceq.simple <- lavmodel@ceq.simple.only
    ceq.K <- lavmodel@ceq.simple.K # used to pack params/grads

    # Partable. Built here rather than with the rest of the optimisation
    # bookkeeping below because the random-slope checks read the
    # transformation each free parameter carries.
    pt <- inlavaanify_partable(lavpartable, dp, lavdata, lavoptions)
    check_packed_kinds(pt)
    check_composite_means(pt, lavoptions)
    pt$parstart <- composite_start_weights(pt, lavsamplestats, lavdata)

    ## ----- Random-slope checks -----------------------------------------------
    # A random-slope likelihood is a per-cluster kernel conditional on the
    # covariates, which rules out two things the rest of the pipeline would
    # otherwise do. Everything here is a no-op without an `rv()` modifier.
    rs_skipped <- character(0)
    if (has_random_slopes(lavmodel)) {
      # lavaan reports `fixed.x = FALSE` of its own accord for a model with
      # no observed exogenous variables at all -- a slope on a latent
      # covariate, say -- so the slot alone would refuse such a fit over an
      # argument the user never passed. The syntax-level set decides.
      if (
        !isTRUE(lavmodel@fixed.x) &&
          length(lavaan::lavNames(lavpartable, "ov.x")) > 0L
      ) {
        cli_abort(
          c(
            "Random-slope models require {.code fixed.x = TRUE}.",
            "x" = "The likelihood conditions on the exogenous covariates, so
                 their means and (co)variances are unidentified and would be
                 reported back as their priors.",
            "i" = "Refit with {.code fixed.x = TRUE}."
          ),
          class = "inlavaan_rs_fixedx"
        )
      }
      if ("ppp" %in% test_req) {
        test_req <- setdiff(test_req, "ppp")
        # Collapsed to one line because the reason is handed back to the
        # user through `test$skipped`, where the source indentation of a
        # wrapped string would show through
        rs_skipped <- c(
          ppp = gsub(
            "\\s+",
            " ",
            "A posterior predictive p-value scores replicate data against a
           saturated model, and a random-slope model has none. Each cluster
           has its own covariance, which depends on its covariate values."
          )
        )
        if (any(c("ppp", "full") %in% test)) {
          cli_warn(
            c(
              "Dropping {.val ppp} from {.arg test}: a posterior predictive
             p-value does not exist for a random-slope model.",
              "x" = "Its discrepancy scores replicate data against a saturated
                   model, and a random-slope model has none. Each cluster has
                   its own covariance, which depends on its covariate
                   values.",
              "i" = "Use {.fn compare} (marginal likelihood, Bayes factors,
                   DIC) or {.fn loo} instead."
            ),
            class = "inlavaan_rs_ppp"
          )
        } else if (!silent) {
          # Kept to one source line: cli_alert_info() does not re-wrap
          cli_alert_info(
            "No PPP: a random-slope model has no saturated model."
          )
        }
      }
      # A slope on a latent or split covariate replaces the closed-form
      # cluster kernel with Gauss-Hermite quadrature, which is worth saying
      # whatever `verbose` asks for.
      if (isTRUE(lavcache[[1L]]$rs$info$nl.flag)) {
        check_rs_zero_var(lavpartable, lavmodel)
        warn_rs_route_b(list(ngh = lavcache[[1L]]$rs$info$ngh))
      }
      # Equality constraints lavaan's packed random-slope gradient cannot be
      # redistributed over exactly (see rs_unpack_grad())
      check_rs_ceq(pt, lavmodel)
    }

    # Free parameters, always in the reduced space under equality constraints
    PTFREEIDX <- which(pt$free > 0L)
    if (isTRUE(ceq.simple)) {
      # Note: Always work in the reduced space
      PTFREEIDX <- which(pt$free > 0L & !duplicated(pt$free))
    }
    m <- length(PTFREEIDX)
    parnames <- pt$names[PTFREEIDX]
    if (anyDuplicated(parnames) > 0) {
      # nocov start
      cli_abort(
        "Parameter names must be unique, but {.code
       {unique(parnames[duplicated(parnames)])}} appear{?s/} more than once."
      )
    } # nocov end

    # Draw-based summaries: covariances, defined (:=) and delta (~*~)
    # parameters
    needs_draw_summaries <-
      sum(pt$free > 0 & grepl("cov", pt$mat)) > 0 ||
      any(pt$op == ":=") ||
      any(pt$op == "~*~")
    if (needs_draw_summaries && !isTRUE(nsamp >= 2)) {
      cli_abort("{.arg nsamp} must be at least 2 to summarise posterior draws.")
    }

    # Cache partable for prior logdens and grad
    prior_cache <- prepare_priors_for_optim(pt)
    inform_gamma_variance_scale(pt)

    # Saturated-means fast path (see saturated_mean_idx): along each free
    # intercept axis the posterior is exactly Gaussian, so the intercept block
    # of the Hessian is analytic and the marginal scans are redundant for those
    # coordinates. The block between the intercepts and the other parameters
    # is not zero at the mode, and engine_hess_mode() differences it.
    fastpath <- saturated_mean_idx(
      pt,
      lavmodel,
      lavsamplestats,
      lavdata,
      ceq.simple
    )
    fp_idx <- if (is.null(fastpath)) integer(0) else fastpath$idx

    ## ----- Prep work for approximation ---------------------------------------
    joint_lp <- function(pars) {
      if (isTRUE(ceq.simple)) {
        pars_unpacked <- as.numeric(ceq.K %*% pars)
        x <- pars_to_x(pars_unpacked, pt)
      } else {
        x <- pars_to_x(pars, pt)
      }
      ll <- inlav_model_loglik(
        x,
        lavmodel,
        lavsamplestats,
        lavdata,
        lavoptions,
        lavcache
      )
      pld <- 0
      if (isTRUE(add_priors)) {
        # Always take in packed version
        # pld <- prior_logdens(pars, pt)
        pld <- prior_logdens_vectorized(pars, prior_cache, debug = FALSE)
      }
      as.numeric(ll + pld)
    }

    joint_lp_grad <- function(pars) {
      # First, the likelihood gradient
      if (isTRUE(ceq.simple)) {
        pars_unpacked <- as.numeric(ceq.K %*% pars)
        x <- pars_to_x(pars_unpacked, pt)
        jcb <- mapply(
          function(f, x) f(x),
          pt$ginv_prime[pt$free > 0],
          pars_unpacked
        )
      } else {
        x <- pars_to_x(pars, pt)
        jcb <- mapply(function(f, x) f(x), pt$ginv_prime[pt$free > 0], pars)
      }
      gll <- inlav_model_grad(x, lavmodel, lavsamplestats, lavdata, lavcache)

      # Jacobian adjustment: d/dθ log p(y|x(θ)) = d/dx log p(y|x) * dx/dθ.
      # The chain-rule Jacobian is a diagonal (per-parameter ginv_prime, times
      # sd1sd2 for the correlation parameters) plus a handful of off-diagonal
      # variance-into-covariance terms listed in jcb_mat, so the product is
      # applied as vector work plus a short loop over those terms rather than
      # ever forming the dense m x m matrix -- this sits inside every gradient
      # call (optimiser, Hessian columns, VB node sweeps, marginal scans).
      gll_th <- jcb * attr(x, "sd1sd2") * gll
      jcb_mat <- attr(x, "jcb_mat")

      if (!is.null(jcb_mat)) {
        for (k in seq_len(nrow(jcb_mat))) {
          i <- jcb_mat[k, 1]
          j <- jcb_mat[k, 2]
          gll_th[i] <- gll_th[i] + jcb_mat[k, 3] * gll[j]
        }
      }
      if (isTRUE(ceq.simple)) {
        gll_th <- as.numeric(gll_th %*% ceq.K)
      } # Repack

      # Next, the prior gradient
      glp_th <- 0
      if (isTRUE(add_priors)) {
        # Always take in packed version
        # glp_th <- prior_grad(pars, pt)
        glp_th <- prior_grad_vectorized(pars, prior_cache)
      }

      as.numeric(gll_th + glp_th)
    }

    timing <- add_timing(timing, "init")

    ## ----- Start values ------------------------------------------------------
    parstart <- pt$parstart[PTFREEIDX]

    # Warm start: `start` is a full vector of free parameters in the internal
    # (unconstrained) parameterisation, e.g. the posterior mode of an earlier
    # fit reused by `update()`. Only valid when the parameter structure matches.
    # The warm start of update() carries the names of the parameters of the
    # earlier fit. It is used only when they are the names of this model, in
    # the same order, and otherwise left out. A change such as std.lv = TRUE
    # keeps the number of parameters but not their names.
    if (inherits(start, "inlavaan_warm_start")) {
      start <- if (identical(names(start), parnames)) {
        unname(unclass(start))
      } else {
        NULL
      }
    }
    if (!is.null(start)) {
      if (length(start) != length(parstart)) {
        cli_abort(c(
          "{.arg start} has length {length(start)} but the model has
         {length(parstart)} free parameter{?s}.",
          "i" = "{.arg start} must be a full vector of free parameters in the
               internal (unconstrained) parameterisation."
        ))
      }
      parstart <- as.numeric(start)
    }

    ## ----- Engine stages -----------------------------------------------------
    # The mode, the Laplace approximation, the VB correction and the
    # marginals on the theta scale come from the engine (see
    # R/engine-adapter.R). On the saturated-means fast path the intercepts
    # get Gaussian marginals and no VB shift.
    init <- stats::setNames(parstart, parnames)
    engine_check_start(joint_lp, init)
    fns <- engine_fns(joint_lp, joint_lp_grad)
    hess_mode <- engine_hess_mode(
      joint_lp_grad,
      method = hessian_method,
      fastpath = fastpath,
      n = n,
      implied_cov = function(theta) {
        lavaan::lav_model_implied(
          lavaan::lav_model_set_parameters(lavmodel, pars_to_x(theta, pt))
        )$cov[[1L]]
      },
      add_priors = add_priors
    )
    core <- engine_call(
      lp = fns$lp,
      gr = fns$gr,
      init = init,
      hess_mode = hess_mode,
      gaussian_marginals = fp_idx,
      vb_correction = vb_correction,
      marginals = marginal_method,
      marginal_scan = marginal_scan,
      cores = cores,
      control = engine_control,
      verbose = verbose,
      silent = silent
    )
    timing <- engine_timing(timing, core$timing)

    # Two-level fits under fixed.x get a theta-free shift that makes the
    # evidence (and the DIC below) conditional on the covariates.
    theta_mode <- if (isTRUE(ceq.simple)) {
      as.numeric(ceq.K %*% core$mode)
    } else {
      as.numeric(core$mode)
    }
    loglik_x_adj <- twolevel_fixedx_loglik_adj(
      lavmodel,
      pars_to_x(theta_mode, pt),
      lavdata,
      lavsamplestats
    )
    eng <- engine_fields(core, loglik_x_adj)
    # The locals that the rest of the fit reads: theta_star is the mode, and
    # theta_star_vbc the VB mean
    theta_star <- eng$theta_star_novbc
    theta_star_vbc <- eng$theta_star
    Omega <- eng$Omega
    opt <- eng$opt
    vb <- eng$vb
    mloglik <- eng$mloglik
    approx_data <- eng$approx_data
    visual_debug <- eng$visual_debug
    if (ceq.simple) {
      theta_star_trans <- pars_to_x(as.numeric(ceq.K %*% theta_star_vbc), pt)
    } else {
      theta_star_trans <- pars_to_x(theta_star_vbc, pt)
    }
    timing <- add_timing(timing, "loglik")

    post_marg <- function(j, g, g_prime, ginv, ginv_prime) {
      if (marginal_method == "skewnorm") {
        post_marg_skewnorm(
          j = j,
          g = g,
          g_prime = g_prime,
          ginv = ginv,
          ginv_prime = ginv_prime,
          theta_star = theta_star_vbc,
          Omega = Omega,
          sn_params = approx_data
        )
      } else {
        post_marg_marggaus(
          j = j,
          g = g,
          g_prime = g_prime,
          ginv = ginv,
          ginv_prime = ginv_prime,
          theta_star = theta_star_vbc,
          Omega = Omega
        )
      }
    }
    postmargres <- Map(
      f = post_marg,
      j = seq_len(m),
      g = pt$g[PTFREEIDX],
      g_prime = pt$g_prime[PTFREEIDX],
      ginv = pt$ginv[PTFREEIDX],
      ginv_prime = pt$ginv_prime[PTFREEIDX]
    )
    timing <- add_timing(timing, "marginals")
    timing$marginals <- timing$marginals +
      core$timing[["marginals"]] +
      core$timing[["diagnostics"]]
    # The NORTA adjustment of the copula has ended, and its segment of the
    # timing is kept, at about zero, for the segments of earlier versions
    timing <- add_timing(timing, "norta")

    ## ----- Draw posterior samples (once) -------------------------------------
    has_extra_samp_work <- needs_draw_summaries ||
      any(c("ppp", "dic") %in% test_req)
    samp_env <- NULL
    if (!silent) {
      samp_stage <- if (has_extra_samp_work) {
        "Posterior sampling and summarising"
      } else {
        "Drawing posterior samples"
      }
      # Rewritten at the end of the block with an inventory of what the draws
      # were used for
      samp_done <- paste0(samp_stage, ".")
      samp_env <- environment()
      cli_progress_step(
        "{samp_stage}.",
        msg_done = "{samp_done}",
        spinner = TRUE,
        .envir = samp_env
      )
    }
    samp <- sample_params(
      theta_star = theta_star_vbc,
      Omega = Omega,
      method = marginal_method,
      approx_data = approx_data,
      pt = pt,
      lavmodel = lavmodel,
      nsamp = nsamp
    )
    theta_samp <- samp$theta_samp
    x_samp <- samp$x_samp
    vcov_x <- cov(x_samp)
    dimnames(vcov_x) <- list(parnames, parnames)
    timing <- add_timing(timing, "sampling")

    summ <- do.call(
      "rbind",
      Map(
        f = function(x, y) {
          out <- t(data.frame(x$summary))
          row.names(out) <- y
          out
        },
        x = postmargres,
        y = parnames
      )
    )
    summ <- cbind(
      summ,
      kld = vb$kld,
      vb_shift_sigma = vb$correction / sqrt(diag(Omega))
    )

    pdf_data <- lapply(postmargres, function(x) x$pdf_data)
    names(pdf_data) <- parnames

    coefs <- summ[, "Mean"]
    names(coefs) <- parnames

    summ <- as.data.frame(summ)
    summ$Prior <- pt$prior[PTFREEIDX]

    ## ----- Sampling for covariances and defined params -----------------------
    # cov_as_cor skips this re-derivation entirely: the per-axis marginal
    # already computed above (postmargres) is left as the final reported
    # value for these rows, which -- since g/ginv is atanh/tanh for
    # theta_cov/psi_cov exactly as for theta_cor/psi_cor -- is already the
    # correlation, not the covariance. Nothing upstream (pars_to_x(), priors,
    # gradients) reads cov_as_cor, so estimation is unaffected either way.
    if (!isTRUE(cov_as_cor) && sum(pt$free > 0 & grepl("cov", pt$mat)) > 0) {
      if (marginal_method == "skewnorm" && isTRUE(sn_fit_sample)) {
        samp_cov <- sample_covariances_fit_sn(x_samp, pt)
        sn_rows <- approx_data_rows(lapply(samp_cov, `[[`, "sn_params"))
        approx_data <- rbind(approx_data, sn_rows)
      } else {
        samp_cov <- sample_covariances(x_samp, pt)
      }

      for (cov_name in names(samp_cov)) {
        tmp_new_summ <- samp_cov[[cov_name]]$summary
        summ[cov_name, names(tmp_new_summ)] <- tmp_new_summ
        # keep the coefficient vector on the covariance scale too; the
        # per-parameter marginal mean is on the correlation (tanh) scale
        coefs[cov_name] <- tmp_new_summ[["Mean"]]
        pdf_data[[cov_name]] <- samp_cov[[cov_name]]$pdf_data
      }
    }
    timing <- add_timing(timing, "covariances")

    # Defined parameters
    def_undefined <- NULL
    if (any(pt$op == ":=")) {
      if (marginal_method == "skewnorm" && isTRUE(sn_fit_sample)) {
        defpars <- get_defpars(x_samp, pt, lavmodel, summarise_samples_sn)
        sn_rows <- approx_data_rows(lapply(defpars, `[[`, "sn_params"))
        approx_data <- rbind(approx_data, sn_rows)
      } else {
        defpars <- get_defpars(x_samp, pt, lavmodel)
      }
      def_undefined <- attr(defpars, "undefined")

      for (def_name in names(defpars)) {
        tmp_new_summ <- defpars[[def_name]]$summary
        summ[def_name, names(tmp_new_summ)] <- tmp_new_summ
        pdf_data[[def_name]] <- defpars[[def_name]]$pdf_data
      }
    }

    # Composite variances and intercepts are functions of the weights, so their
    # posteriors come from the draws in the same way.
    if (length(composite_derived_rows(pt)) > 0L) {
      comp_draws <- composite_derived_draws(x_samp, pt, lavmodel)
      use_sn <- marginal_method == "skewnorm" && isTRUE(sn_fit_sample)
      summarise <- if (use_sn) summarise_samples_sn else summarise_samples
      comp_summ <- lapply(seq_len(ncol(comp_draws)), function(j) {
        summarise(comp_draws[, j])
      })
      names(comp_summ) <- colnames(comp_draws)
      if (use_sn) {
        sn_rows <- approx_data_rows(lapply(comp_summ, `[[`, "sn_params"))
        approx_data <- rbind(approx_data, sn_rows)
      }
      for (comp_name in names(comp_summ)) {
        tmp_new_summ <- comp_summ[[comp_name]]$summary
        summ[comp_name, names(tmp_new_summ)] <- tmp_new_summ
        pdf_data[[comp_name]] <- comp_summ[[comp_name]]$pdf_data
      }
    }
    timing <- add_timing(timing, "definedpars")

    # For binary and ordinal data, sample the deltas
    if (any(pt$op == "~*~")) {
      deltapars <- get_thetaparamerization_deltas(x_samp, lavmodel)
      names(deltapars) <- pt$names[which(pt$op == "~*~")]

      for (delta_name in names(deltapars)) {
        tmp_new_summ <- deltapars[[delta_name]]$summary
        summ[delta_name, names(tmp_new_summ)] <- tmp_new_summ
        pdf_data[[delta_name]] <- deltapars[[delta_name]]$pdf_data
      }
    }
    timing <- add_timing(timing, "deltapars")

    ## ----- Compute ppp and dic -----------------------------------------------
    ppp <- dic_list <- NULL
    skipped <- rs_skipped
    if (any(c("ppp", "dic") %in% test_req)) {
      if (!silent) {
        samp_stage <- paste0(
          "Computing fit indices (",
          paste(toupper(intersect(c("ppp", "dic"), test_req)), collapse = "/"),
          ")"
        )
        cli_progress_update(.envir = samp_env)
      }
      fixed_both <- if (lavdata@nlevels > 1L) ppp2l_fixed_both(lavdata)
      if ("ppp" %in% test_req && length(fixed_both) > 0L) {
        # nocov start -- lavaan models a covariate at both levels, so it does
        # not mark one as fixed at both today.
        msg <- paste0(
          "The two-level PPP draws its replicate data given the fixed ",
          "covariates of each level, and ",
          paste(fixed_both, collapse = ", "),
          " is fixed at both levels."
        )
        cli_warn(c(
          "Skipping the PPP requested through {.arg test}.",
          "x" = cli_escape(msg),
          "i" = "The rest of the fit is unaffected; the reason is stored in
               {.code get_inlavaan_internal(fit, \"test\")$skipped}."
        ))
        skipped <- c(skipped, ppp = msg)
        # nocov end
      } else if ("ppp" %in% test_req && lavdata@nlevels > 1L) {
        n_ppp <- min(nrow(x_samp), floor(ppp_nsamp))
        ppp <- get_ppp_twolevel(
          x_samp = x_samp[seq_len(n_ppp), , drop = FALSE],
          lavmodel = lavmodel,
          lavsamplestats = lavsamplestats,
          lavdata = lavdata,
          method = ppp_method,
          cli_env = samp_env
        )
        if (is.na(ppp)) {
          skipped <- c(skipped, ppp = "No posterior draw could be scored.")
          ppp <- NULL
        }
      } else if ("ppp" %in% test_req) {
        ppp <- get_ppp(
          x_samp = x_samp,
          lavmodel = lavmodel,
          lavsamplestats = lavsamplestats,
          lavdata = lavdata,
          lavpartable = lavpartable,
          h1 = fit0@h1,
          cli_env = samp_env
        )
      }
      if ("dic" %in% test_req) {
        dic_list <- get_dic(
          x_samp = x_samp,
          theta_star = theta_star_vbc,
          pt = pt,
          lavmodel = lavmodel,
          loglik = function(x) {
            inlav_model_loglik(
              x,
              lavmodel,
              lavsamplestats,
              lavdata,
              lavoptions,
              lavcache
            ) +
              loglik_x_adj
          },
          cli_env = samp_env
        )
      }
    }
    timing <- add_timing(timing, "test")

    ## ----- Fit-time LOO and WAIC ---------------------------------------------
    # Minimal internal view of the fit for the casewise machinery
    int_fit <- list(
      partable = pt,
      lavmodel = lavmodel,
      lavdata = lavdata,
      lavcache = if (has_random_slopes(lavmodel)) lavcache else NULL,
      lavsamplestats = lavsamplestats,
      theta_star = as.numeric(theta_star_vbc),
      Omega = Omega,
      marginal_method = marginal_method,
      approx_data = approx_data,
      nsamp = nsamp
    )
    # LOO and WAIC are one Taylor pass (waic_from_taylor() reads the per-unit
    # quantities inlav_loo() already computed), so asking for either atom
    # computes and stores both, with no time budget. A model the casewise
    # machinery rejects (check_loo_model(), called inside inlav_loo()) warns
    # and skips them rather than failing the whole fit; the reason is kept in
    # the `test` record below rather than only in the transient warning.
    loo_res <- waic_res <- NULL
    if (any(c("loo", "waic") %in% test_req)) {
      if (!silent) {
        samp_stage <- "Computing Taylor LOO and WAIC"
        cli_progress_update(.envir = samp_env)
      }
      loo_try <- tryCatch(
        inlav_loo(
          int = int_fit,
          eff_cores = resolve_loo_cores(cores),
          verbose = FALSE,
          warn_route_b = FALSE
        ),
        error = function(e) e
      )
      if (inherits(loo_try, "error")) {
        msg <- conditionMessage(loo_try)
        cli_warn(c(
          "Skipping the LOO and WAIC requested through {.arg test}.",
          "x" = msg,
          "i" = "The rest of the fit is unaffected; the reason is stored in
               {.code get_inlavaan_internal(fit, \"test\")$skipped}."
        ))
        skipped <- c(skipped, loo = msg, waic = msg)
      } else {
        loo_res <- loo_try
        timing <- add_timing(timing, "loo")
        waic_res <- waic_from_taylor(loo_res)
        timing <- add_timing(timing, "waic")
      }
    }

    computed <- test_atoms[c(
      !is.null(ppp),
      !is.null(dic_list),
      !is.null(loo_res),
      !is.null(waic_res)
    )]
    test_rec <- list(
      requested = test_requested,
      computed = computed,
      skipped = skipped
    )

    if (!silent) {
      # Close the sampling step with an overview; the specific fit measures
      # computed are listed on a separate info line below
      fit_measures <- toupper(computed)
      samp_done <- if (
        needs_draw_summaries || any(c("ppp", "dic") %in% computed)
      ) {
        paste0("Summarise ", nsamp, " posterior draws.")
      } else {
        paste0("Draw ", nsamp, " posterior samples.")
      }
      cli_progress_done(.envir = samp_env)
      if (length(fit_measures)) {
        cli_alert_info(
          paste0("Fit measures: ", paste(fit_measures, collapse = ", "), ".")
        )
      }
      if ("ppp" %in% computed && lavdata@nlevels > 1L) {
        cli_alert_info("The two-level PPP is experimental.")
        cli_alert_info(paste0(
          "Please report any bugs at ",
          "{.url https://github.com/haziqj/INLAvaan/issues}."
        ))
      }
    }

    ## ----- Output ------------------------------------------------------------
    # Latent and residual covariance matrices: positive definite at the
    # estimates and in the draws? Under cov_as_cor the estimates of the
    # covariance rows are correlations, so only the draws are checked.
    cov_pd <- cov_pd_check(
      x_est = if (isTRUE(cov_as_cor)) NULL else coefs,
      x_samp = x_samp,
      lavmodel = lavmodel
    )

    out <- list(
      coefficients = coefs,
      mloglik = mloglik,
      DIC = dic_list,
      summary = summ,
      ppp = ppp,
      loo = loo_res,
      waic = waic_res,
      test = test_rec,
      # Constants since 0.4.0, kept for code that reads them: the mode is
      # found by nlminb(), and the draws use the copula with no NORTA
      optim_method = "nlminb",
      marginal_method = marginal_method,
      cov_as_cor = isTRUE(cov_as_cor),
      samp_copula = TRUE,
      samp_norta = FALSE,
      theta_star_novbc = as.numeric(theta_star),
      theta_star = as.numeric(theta_star_vbc),
      Omega = Omega,
      R_star = NULL,
      vcov_x = vcov_x,
      theta_star_trans = theta_star_trans,
      approx_data = approx_data,
      nsamp = nsamp,
      pdf_data = pdf_data,
      def_undefined = def_undefined,
      partable = pt,
      lavmodel = lavmodel,
      lavsamplestats = lavsamplestats,
      lavdata = lavdata,
      lavcache = if (has_random_slopes(lavmodel)) lavcache else NULL,
      opt = opt,
      timing = timing[-1], # remove start.time
      visual_debug = visual_debug,
      vb = vb,
      cov_pd = cov_pd,
      call = mc,
      version = as.character(utils::packageVersion("INLAvaan")),
      # The engine's own record of the fit
      engine = eng$engine
    )
    class(out) <- "inlavaan_internal"

    # Warn (once, consolidated) if the convergence/approximation diagnostics
    # look off; see warn_fit_diagnostics() for the checks and thresholds
    warn_fit_diagnostics(out)
    # Filled last, so that it holds the fit-time warning too
    out$warnings <- fit_warnings

    if (debug) {
      return(out)
    } else {
      out <- create_lav_from_inlavaan_internal(fit0, out)
      return(new("INLAvaan", out))
    }
  })
}

#' Fit an Approximate Bayesian Confirmatory Factor Analysis Model
#'
#' Fit an Approximate Bayesian Confirmatory Factor Analysis Model
#'
#' The [acfa()] function is a wrapper for the more general [inlavaan()]
#' function, using the following default arguments:
#'   - `int.ov.free = TRUE`
#'   - `int.lv.free = FALSE`
#'   - `auto.fix.first = TRUE` (unless `std.lv = TRUE`)
#'   - `auto.fix.single = TRUE`
#'   - `auto.var = TRUE`
#'   - `auto.cov.lv.x = TRUE`
#'   - `auto.efa = TRUE`
#'   - `auto.th = TRUE`
#'   - `auto.delta = TRUE`
#'   - `auto.cov.y = TRUE`
#'
#' For further information regarding these arguments, please refer to the
#' [lavaan::lavOptions()] documentation.
#'
#' @inherit inlavaan params return seealso
#' @example inst/examples/ex-cfa.R
#' @export
acfa <- function(
  model,
  data = NULL,
  dp = priors_for(),
  test = "standard",
  vb_correction = c("onestep", "full", "none"),
  n_qmc = NULL,
  vb_method = NULL,
  marginal_method = c("skewnorm", "gaussian"),
  marginal_correction = NULL,
  nsamp = 1000,
  samp_copula = NULL,
  samp_norta = NULL,
  cov_as_cor = FALSE,
  sn_fit_ngrid = NULL,
  sn_fit_logthresh = NULL,
  sn_fit_temp = NULL,
  sn_fit_sample = TRUE,
  control = list(),
  verbose = FALSE,
  debug = NULL,
  add_priors = NULL,
  optim_method = NULL,
  numerical_grad = NULL,
  hessian_method = NULL,
  cores = NULL,
  ppp_method = c("onestep", "em"),
  ppp_nsamp = 250L,
  marginal_scan = c("linear", "exact", "laplace", "plain"),
  silent = FALSE,
  ...
) {
  sc <- check_wrapper_call(sys.call(), "acfa")
  sc[["model.type"]] <- quote("cfa")
  sc[[1L]] <- quote(INLAvaan::inlavaan)
  eval(sc, parent.frame())
}

#' Fit an Approximate Bayesian Structural Equation Model
#'
#' Fit an Approximate Bayesian Structural Equation Model
#'
#' The [asem()] function is a wrapper for the more general [inlavaan()]
#' function, using the following default arguments:
#'   - `int.ov.free = TRUE`
#'   - `int.lv.free = FALSE`
#'   - `auto.fix.first = TRUE` (unless `std.lv = TRUE`)
#'   - `auto.fix.single = TRUE`
#'   - `auto.var = TRUE`
#'   - `auto.cov.lv.x = TRUE`
#'   - `auto.efa = TRUE`
#'   - `auto.th = TRUE`
#'   - `auto.delta = TRUE`
#'   - `auto.cov.y = TRUE`
#'
#' For further information regarding these arguments, please refer to the
#' [lavaan::lavOptions()] documentation.
#'
#' @inherit inlavaan params return seealso
#' @example inst/examples/ex-sem.R
#' @export
asem <- function(
  model,
  data = NULL,
  dp = priors_for(),
  test = "standard",
  vb_correction = c("onestep", "full", "none"),
  n_qmc = NULL,
  vb_method = NULL,
  marginal_method = c("skewnorm", "gaussian"),
  marginal_correction = NULL,
  nsamp = 1000,
  samp_copula = NULL,
  samp_norta = NULL,
  cov_as_cor = FALSE,
  sn_fit_ngrid = NULL,
  sn_fit_logthresh = NULL,
  sn_fit_temp = NULL,
  sn_fit_sample = TRUE,
  control = list(),
  verbose = FALSE,
  debug = NULL,
  add_priors = NULL,
  optim_method = NULL,
  numerical_grad = NULL,
  hessian_method = NULL,
  cores = NULL,
  ppp_method = c("onestep", "em"),
  ppp_nsamp = 250L,
  marginal_scan = c("linear", "exact", "laplace", "plain"),
  silent = FALSE,
  ...
) {
  sc <- check_wrapper_call(sys.call(), "asem")
  sc[["model.type"]] <- quote("sem")
  sc[[1L]] <- quote(INLAvaan::inlavaan)
  eval(sc, parent.frame())
}

#' Fit an Approximate Bayesian Growth Curve Model
#'
#' Fit an Approximate Bayesian Growth Curve Model
#'
#' The [agrowth()] function is a wrapper for the more general [inlavaan()]
#' function, using the following default arguments:
#'   - `meanstructure = TRUE`
#'   - `int.ov.free = FALSE`
#'   - `int.lv.free = TRUE`
#'   - `auto.fix.first = TRUE` (unless `std.lv = TRUE`)
#'   - `auto.fix.single = TRUE`
#'   - `auto.var = TRUE`
#'   - `auto.cov.lv.x = TRUE`
#'   - `auto.efa = TRUE`
#'   - `auto.th = TRUE`
#'   - `auto.delta = TRUE`
#'   - `auto.cov.y = TRUE`
#'
#' @inherit inlavaan params return seealso
#' @example inst/examples/ex-growth.R
#' @export
agrowth <- function(
  model,
  data = NULL,
  dp = priors_for(),
  test = "standard",
  vb_correction = c("onestep", "full", "none"),
  n_qmc = NULL,
  vb_method = NULL,
  marginal_method = c("skewnorm", "gaussian"),
  marginal_correction = NULL,
  nsamp = 1000,
  samp_copula = NULL,
  samp_norta = NULL,
  cov_as_cor = FALSE,
  sn_fit_ngrid = NULL,
  sn_fit_logthresh = NULL,
  sn_fit_temp = NULL,
  sn_fit_sample = TRUE,
  control = list(),
  verbose = FALSE,
  debug = NULL,
  add_priors = NULL,
  optim_method = NULL,
  numerical_grad = NULL,
  hessian_method = NULL,
  cores = NULL,
  ppp_method = c("onestep", "em"),
  ppp_nsamp = 250L,
  marginal_scan = c("linear", "exact", "laplace", "plain"),
  silent = FALSE,
  ...
) {
  sc <- check_wrapper_call(sys.call(), "agrowth")
  sc[["model.type"]] <- quote("growth")
  sc[[1L]] <- quote(INLAvaan::inlavaan)
  eval(sc, parent.frame())
}
