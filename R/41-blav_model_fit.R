# Modification of blav_model_fit() to work with inla output
inlav_model_fit <- function(
    lavpartable = NULL,
    lavmodel    = NULL,
    lavjags     = NULL,
    x           = NULL,
    VCOV        = NULL,
    TEST        = NULL) {

  stopifnot(is.list(lavpartable), inherits(lavmodel, c("Model", "lavModel")))
  # if(!inherits(lavjags, "NULL")){
  #   lavmcmc <- make_mcmc(lavjags)
  # } else {
  #   lavmcmc <- NULL
  # }

  # extract information from 'x'
  iterations <- attr(x, "iterations")
  converged  <- attr(x, "converged")
  fx         <- attr(x, "fx")  # TODO: what's this??
  fx.group   <- as.numeric(NA)
  #fx.group   = attr(fx, "fx.group")
  logl.group <- as.numeric(NA)
  logl       <- as.numeric(NA)

  #print(fx.group)
  control    <- attr(x, "control")
  attributes(fx) <- NULL
  x.copy <- x # we are going to change it (remove attributes)
  attributes(x.copy) <- NULL
  est <- lav_model_get_parameters(lavmodel = lavmodel, type = "user")

  # did we compute standard errors?
  # blaboot <- rearr_params(lavmcmc, lavpartable)

  # FIXME: Something's not right here
  se <- lav_model_vcov_se(
    lavmodel = lavmodel,
    lavpartable = lavpartable,
    VCOV = VCOV,
    BOOT = NULL
  )

  # did we compute test statistics
  if(is.null(TEST)) {
    test <- list()
  } else {
    test <- TEST
  }

  # for convenience: compute lavmodel-implied Sigma and Mu
  implied <- lav_model_implied(lavmodel, delta = (lavmodel@parameterization == "delta"))
  # change names back if conditional.x (see lav_model_implied.R)
  if(lavmodel@conditional.x) {
    names(implied) <- c("cov", "mean", "slopes", "th", "group.w")
  }

  # partrace?
  if(!is.null(attr(x, "partrace"))) {
    PARTRACE <- attr(x, "partrace")
  } else {
    PARTRACE <- matrix(0, 0L, 0L)
  }

  new("Fit",
      npar       = as.integer(max(lavpartable$free)),
      x          = x.copy,
      partrace   = PARTRACE,
      start      = lavpartable$start, # needed?
      est        = est,
      se         = se,
      fx         = fx,
      fx.group   = fx.group,
      logl       = logl,
      logl.group = logl.group,
      iterations = as.integer(iterations),
      converged  = converged,
      control    = control,
      Sigma.hat  = implied$cov,
      Mu.hat     = implied$mean,
      TH         = implied$th,
      test       = test
  )
}
