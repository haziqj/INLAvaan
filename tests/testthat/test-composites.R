dat <- lavaan::HolzingerSwineford1939
mod <- "
  C <~ x1 + x2 + x3
  x4 ~ C
  x5 ~ C
  x4 ~~ x5
"
NSAMP <- 3

# The tests look at the posterior mode, so they skip the VB and the marginal
# corrections to stay fast.
fit_quiet <- function(model, data = dat, ...) {
  asem(
    model,
    data,
    verbose = FALSE,
    test = "none",
    nsamp = NSAMP,
    vb_correction = FALSE,
    marginal_correction = "none",
    ...
  )
}

# Posterior mode on the lavaan side, in coef() order
mode_x <- function(fit) {
  int <- get_inlavaan_internal(fit)
  theta <- int$theta_star_novbc
  if (isTRUE(fit@Model@ceq.simple.only)) {
    theta <- as.numeric(fit@Model@ceq.simple.K %*% theta)
  }
  setNames(as.numeric(pars_to_x(theta, int$partable)), names(coef(fit)))
}

fit <- fit_quiet(mod)

test_that("Weights get class wmat and the default prior", {
  pt <- get_inlavaan_internal(fit)$partable
  w <- pt$op == "<~"
  expect_true(all(pt$mat[w] == "wmat"))
  expect_equal(pt$prior[w & pt$free > 0], rep("normal(0,10)", 2))
  expect_true(is.na(pt$prior[w & pt$free == 0]))
  expect_no_error(out <- capture.output(summary(fit)))
  expect_true(any(grepl("Composites:", out)))
})

test_that("A prior() on a weight is used", {
  fit_p <- fit_quiet(sub("x2", "prior(\"normal(0,0.05)\")*x2", mod))
  pt <- get_inlavaan_internal(fit_p)$partable
  expect_equal(pt$prior[pt$op == "<~" & pt$rhs == "x2"], "normal(0,0.05)")
  expect_lt(abs(coef(fit_p)[["C<~x2"]]), abs(coef(fit)[["C<~x2"]]))
})

test_that("The posterior mode is close to the ML estimates", {
  fit_ml <- lavaan::sem(mod, dat)
  expect_equal(mode_x(fit), coef(fit_ml), tolerance = 0.01)

  # One outcome: a reparametrised regression of x4 on x1, x2 and x3
  fit_1 <- fit_quiet("C <~ x1 + x2 + x3\n x4 ~ C", meanstructure = TRUE)
  gam <- coef(lavaan::sem("x4 ~ x1 + x2 + x3", dat, meanstructure = TRUE))
  x1 <- mode_x(fit_1)
  expect_equal(x1[["x4~C"]], gam[["x4~x1"]], tolerance = 0.01)
  expect_equal(
    x1[c("C<~x2", "C<~x3")],
    gam[c("x4~x2", "x4~x3")] / gam[["x4~x1"]],
    tolerance = 0.01,
    ignore_attr = TRUE
  )
})

test_that("Composite variances and intercepts have posterior summaries", {
  fit_m <- fit_quiet(mod, meanstructure = TRUE)
  pt <- lavaan::parTable(fit_m)
  w <- c(1, coef(fit_m)[c("C<~x2", "C<~x3")])
  ind <- c("x1", "x2", "x3")
  tmat <- cov(dat[ind]) * (nrow(dat) - 1) / nrow(dat)
  vrow <- pt$lhs == "C" & pt$op == "~~"
  expect_equal(pt$est[vrow], drop(t(w) %*% tmat %*% w), tolerance = 1e-6)
  mrow <- pt$lhs == "C" & pt$op == "~1"
  nu <- coef(fit_m)[paste0(ind, "~1")]
  expect_equal(pt$est[mrow], sum(w * nu), tolerance = 1e-6)

  # The parameter table keeps the values at the posterior-mean weights above,
  # while the summary reports the posterior of these functions of the weights
  summ <- get_inlavaan_internal(fit_m)$summary
  expect_gt(summ["C~~C", "SD"], 0)
  expect_equal(pt$se[vrow], summ["C~~C", "SD"])
  out <- capture.output(summary(fit_m))
  expect_true(any(grepl(sprintf("%.3f", summ["C~~C", "Mean"]), out)))
  summ_sum <- get_inlavaan_internal(
    fit_quiet("C <~ 1*x1 + 1*x2 + 1*x3\n x4 ~ C")
  )$summary
  expect_equal(summ_sum["C~~C", "SD"], 0)

  std <- standardisedsolution(fit_m, nsamp = 5)
  expect_equal(std$est.std[std$lhs == "C" & std$op == "~~"], 1)
  r2 <- lavaan::lavInspect(
    fit_quiet("C1 <~ x1 + x2 + x3\n C2 <~ x4 + x5 + x6\n C2 ~ C1\n x7 ~ C2"),
    "rsquare"
  )
  expect_true(all(r2 >= 0 & r2 <= 1))
})

test_that("Mean structures, higher-order factors and groups fit", {
  fit_m <- fit_quiet(mod, meanstructure = TRUE)
  fit_ml <- lavaan::sem(mod, dat, meanstructure = TRUE)
  expect_equal(mode_x(fit_m), coef(fit_ml), tolerance = 0.01)

  # With centred indicators the composite mean starts at zero, which used to
  # switch on the saturated-means fast path. The intercept block is not
  # separable for composites, so the fast path must stay off.
  dat_c <- dat
  dat_c[paste0("x", 1:5)] <- scale(dat_c[paste0("x", 1:5)], scale = FALSE)
  fit_c <- fit_quiet(mod, dat_c, meanstructure = TRUE)
  int_c <- get_inlavaan_internal(fit_c)
  expect_null(saturated_mean_idx(
    int_c$partable,
    int_c$lavmodel,
    int_c$lavsamplestats,
    int_c$lavdata,
    FALSE
  ))
  slopes <- c("C<~x2", "C<~x3", "x4~C", "x5~C")
  expect_equal(mode_x(fit_c)[slopes], mode_x(fit_m)[slopes], tolerance = 1e-3)

  ho <- "
    C1 <~ x1 + x2 + x3
    C2 <~ x4 + x5 + x6
    C3 <~ x7 + x8 + x9
    H =~ C1 + C2 + C3
  "
  expect_no_error(
    fit_ho <- suppressWarnings(fit_quiet(ho, meanstructure = TRUE))
  )
  expect_s4_class(fit_ho, "INLAvaan")
  # From lavaan's starts (all weights 1) the fit settles in a poorer mode with
  # these two weights positive.
  expect_true(all(mode_x(fit_ho)[c("C3<~x8", "C3<~x9")] < 0))
  expect_no_error(
    fit_g <- suppressWarnings(fit_quiet(
      mod,
      group = "school",
      group.equal = "composite.weights"
    ))
  )
  pt <- get_inlavaan_internal(fit_g)$partable
  expect_equal(length(unique(pt$free[pt$op == "<~" & pt$free > 0])), 2)
})

test_that("Covariances with a composite are scaled by its current variance", {
  two <- "
    C1 <~ x1 + x2 + x3
    C2 <~ x4 + x5 + x6
    x7 ~ C1 + C2
    x8 ~ C1 + C2
    x9 ~ C2
  "
  fit_2 <- suppressWarnings(fit_quiet(two, meanstructure = TRUE))
  fit_ml <- lavaan::sem(two, dat, meanstructure = TRUE)
  expect_equal(mode_x(fit_2), coef(fit_ml), tolerance = 0.02)

  # Under std.lv a composite keeps its marker scale, so C1 ~~ C2 stays a
  # covariance.
  fit_s <- fit_quiet(two, std.lv = TRUE)
  pt <- get_inlavaan_internal(fit_s)$partable
  expect_equal(pt$mat[pt$lhs == "C1" & pt$rhs == "C2"], "psi_cov")

  # The gradient includes the dependence of Var(C) on the weights
  int <- get_inlavaan_internal(fit_2)
  expect_lt(max(abs(int$opt$dx_analytic - int$opt$dx)), 1e-3)
})

test_that("Weights start from a rank-one moment estimate", {
  ho <- "
    C1 <~ x1 + x2 + x3
    C2 <~ x4 + x5 + x6
    C3 <~ x7 + x8 + x9
    H =~ C1 + C2 + C3
  "
  fit0 <- lavaan::sem(ho, dat, do.fit = FALSE)
  pt <- inlavaanify_partable(
    fit0@ParTable,
    priors_for(),
    fit0@Data,
    fit0@Options
  )
  start <- composite_start_weights(pt, fit0@SampleStats, fit0@Data)
  c3 <- pt$op == "<~" & pt$lhs == "C3" & pt$free > 0
  # lavaan starts at 1, but the best C3 weights are negative
  expect_true(all(pt$parstart[c3] == 1))
  expect_true(all(start[c3] < 0))
  expect_equal(start[pt$free == 0], pt$parstart[pt$free == 0])

  # inlavaan() uses these starts, but keeps a start() of the user's
  pt_fit <- get_inlavaan_internal(fit)$partable
  w <- pt_fit$op == "<~" & pt_fit$free > 0
  expect_false(any(pt_fit$parstart[w] == 1))
  fit_s <- fit_quiet(sub("x2", "start(0.5)*x2", mod))
  pt_s <- get_inlavaan_internal(fit_s)$partable
  expect_equal(pt_s$parstart[pt_s$op == "<~" & pt_s$rhs == "x2"], 0.5)
})

test_that("A dp without a wmat entry uses the default weight prior", {
  dp <- priors_for()
  dp <- dp[names(dp) != "wmat"]
  fit_d <- fit_quiet(mod, dp = dp)
  pt <- get_inlavaan_internal(fit_d)$partable
  expect_equal(pt$prior[pt$op == "<~" & pt$free > 0], rep("normal(0,10)", 2))
})

test_that("Out-of-scope composite models stop early", {
  expect_error(
    fit_quiet(
      "level: 1\n C <~ y1 + y2 + y3\n y4 ~ C\nlevel: 2\n y1 ~~ y2",
      lavaan::Demo.twolevel,
      cluster = "cluster"
    ),
    "two-level"
  )
  expect_error(fit_quiet(mod, composites.cov = "free"), "composites.cov")
  dat_ord <- dat
  dat_ord$x4 <- cut(dat$x4, 3, labels = FALSE)
  dat_ord$x5 <- cut(dat$x5, 3, labels = FALSE)
  expect_error(
    fit_quiet(mod, dat_ord, ordered = c("x4", "x5")),
    "ordinal data"
  )
})

test_that("Each composite needs a unit marker and its own indicators", {
  expect_error(
    fit_quiet("C <~ 2*x1 + x2 + x3\n x4 ~ C\n x5 ~ C"),
    "one weight fixed at 1"
  )
  expect_error(
    inlavaan(
      "C <~ x1 + x2 + x3\n x4 ~ C\n x5 ~ C\n x4 ~~ x4\n x5 ~~ x5",
      dat,
      model.type = "lavaan",
      verbose = FALSE,
      test = "none",
      nsamp = NSAMP
    ),
    "one weight fixed at 1"
  )
  expect_no_error(fit_quiet("C <~ 1*x1 + 1*x2 + 1*x3\n x4 ~ C\n x5 ~ C"))
  expect_error(
    fit_quiet("C <~ x1 + x2 + 0*x3\n x4 ~ C\n x5 ~ C"),
    "cannot be fixed at 0"
  )
  expect_error(
    fit_quiet("C1 <~ x1 + x2 + x3\n C2 <~ x3 + x4 + x5\n x7 ~ C1 + C2"),
    "one composite only"
  )
  expect_error(
    fit_quiet(
      "C1 <~ x1 + x2 + x3\n C2 <~ x4 + x5 + x6\n x7 ~ C1 + C2\n x3 ~~ x4"
    ),
    "outside that composite"
  )
  expect_error(
    fit_quiet("C <~ x1 + x2 + x3\n F =~ x4 + x5 + x6\n F ~ C\n x3 ~~ x6"),
    "outside that composite"
  )
})

test_that("Freed composite variances and intercepts stop the fit", {
  expect_error(
    fit_quiet("C <~ x1 + x2 + x3\n x4 ~ C\n x5 ~ C\n C ~~ NA*C"),
    "cannot be free"
  )
  expect_error(
    fit_quiet("C <~ x1 + x2 + x3\n x4 ~ C\n C ~ NA*1", meanstructure = TRUE),
    "cannot be free"
  )
  expect_no_error(
    fit_quiet(
      "C <~ x1 + x2 + x3\n x4 ~ C\n C ~~ C\n C ~ 1",
      meanstructure = TRUE
    )
  )
})

test_that("Labels on derived composite rows cannot be shared or constrained", {
  expect_error(
    fit_quiet("C <~ x1 + x2 + x3\n C ~~ vc*C\n x4 ~ b4*C\n x5 ~ C\n vc == b4"),
    "labels cannot be shared"
  )
  expect_error(
    fit_quiet("C <~ x1 + x2 + x3\n C ~~ v*C\n x4 ~ C\n x5 ~ C\n x4 ~~ v*x4"),
    "labels cannot be shared"
  )
  # lavaan also ties rows through its default labels
  expect_error(
    fit_quiet("C <~ x1 + x2 + x3\n x4 ~ C\n x5 ~ C\n x4 ~~ equal('C~~C')*x4"),
    "labels cannot be shared"
  )
  expect_no_error(
    fit_quiet("C <~ x1 + w2*x2 + w3*x3\n x4 ~ C\n x5 ~ C\n w2 == w3")
  )
})

test_that("Latent means absorbed by composites stop the fit", {
  ho <- "
    C1 <~ x1 + x2 + x3
    C2 <~ x4 + x5 + x6
    C3 <~ x7 + x8 + x9
    H =~ C1 + C2 + C3
  "
  expect_error(
    fit_quiet(paste(ho, "H ~ 1"), meanstructure = TRUE),
    "composites absorb"
  )

  # Growth on composites: four waves of two indicators each
  set.seed(1)
  n <- 200
  i <- rnorm(n, 3)
  s <- rnorm(n, 0.5, 0.4)
  gdat <- as.data.frame(do.call(
    cbind,
    lapply(0:3, function(t) {
      comp <- i + t * s + rnorm(n, sd = 0.5)
      u <- rnorm(n)
      cbind(comp - u, u)
    })
  ))
  names(gdat) <- paste0("x", rep(1:4, each = 2), 1:2)
  gr <- "
    C1 <~ x11 + x12
    C2 <~ x21 + x22
    C3 <~ x31 + x32
    C4 <~ x41 + x42
    i =~ 1*C1 + 1*C2 + 1*C3 + 1*C4
    s =~ 0*C1 + 1*C2 + 2*C3 + 3*C4
  "
  expect_error(
    suppressWarnings(
      agrowth(gr, gdat, verbose = FALSE, test = "none", nsamp = NSAMP)
    ),
    "agrowth"
  )
  expect_no_error(fit_quiet(gr, gdat, meanstructure = TRUE))

  # Two waves measured directly identify both growth means. With only the
  # first, the slope reaches it through a loading fixed at zero, so its mean
  # is still absorbed.
  gdat$y1 <- gdat$x11 + gdat$x12 + rnorm(n, sd = 0.3)
  gdat$y4 <- gdat$x41 + gdat$x42 + rnorm(n, sd = 0.3)
  mixed <- "
    C2 <~ x21 + x22
    C3 <~ x31 + x32
    i =~ 1*y1 + 1*C2 + 1*C3 + 1*y4
    s =~ 0*y1 + 1*C2 + 2*C3 + 3*y4
    y1 ~ 0*1
    y4 ~ 0*1
    i ~ 1
    s ~ 1
  "
  expect_no_error(fit_quiet(mixed, gdat, meanstructure = TRUE))
  one_wave <- "
    C2 <~ x21 + x22
    C3 <~ x31 + x32
    C4 <~ x41 + x42
    i =~ 1*y1 + 1*C2 + 1*C3 + 1*C4
    s =~ 0*y1 + 1*C2 + 2*C3 + 3*C4
    y1 ~ 0*1
    i ~ 1
    s ~ 1
  "
  expect_error(
    fit_quiet(one_wave, gdat, meanstructure = TRUE),
    "composites absorb"
  )
})

test_that("plot() draws derived rows and explains fixed ones", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_no_error(plot(fit, params = "C~~C", use_ggplot = FALSE))
  expect_error(plot(fit, params = "C<~x1"), "fixed")
})
