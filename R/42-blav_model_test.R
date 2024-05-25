# Modification of blav_model_test() to work with inla output
inlav_model_test <- function(
    lavmodel = NULL,
    lavpartable = NULL,
    lavsamplestats = NULL,
    lavoptions = NULL,
    x = NULL,
    VCOV = NULL,
    lavcache = NULL,
    lavdata = NULL,
    lavjags = NULL,
    lavobject = NULL,
    samplls = NULL,
    jagextra = NULL,
    stansumm = NULL,
    domll = NULL,
    control = list()) {

  TEST <- list()

  # Marginal log-likelihood ----------------------------------------------------
  mll <- NA
  if (domll) {
    mll <- lavjags$mlik[2, 1]
  }

  TEST[[1]] <- list(
    test = "mloglik",
    stat = as.numeric(mll),
    stat.group = as.numeric(NA),
    df = as.integer(NA),
    refdistr = "NA",
    pvalue = as.numeric(NA)
  )

  # PPP ------------------------------------------------------------------------
  # PPP is a bit trickier to implement! For future improvement
  # TODO: Implement PPP for INLA
  # if (lavoptions$target == "stan") {
  #   ppp <- stansumm["ppp", "mean"]
  # } else {
  #   ppp <- postpred(samplls, lavobject)$ppval
  # }

  ppp <- NA
  TEST[[2]] <- list(
    test = "ppp",
    ## DIC: 2*ll(theta_hat) - 4*mean(ll(theta_samp))
    stat = as.numeric(ppp),
    stat.group = as.numeric(NA),
    df = as.integer(NA),
    refdistr = "NA",
    pvalue = as.numeric(NA)
  )

  TEST
}
