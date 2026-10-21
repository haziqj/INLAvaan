# Single-level FIML fits: the saturated-means fast path must stay off, so that
# the intercept block of the Hessian, the VB shift and the marginal scans are
# computed like every other parameter, and the PPP must use the saturated
# (EM) covariance rather than the incomplete sample covariance.

set.seed(5)
hs <- lavaan::HolzingerSwineford1939[, paste0("x", 1:6)]
hs$x1[sample(nrow(hs), 60)] <- NA
hs$x4[sample(nrow(hs), 60)] <- NA
mod_hs <- "
  f1 =~ x1 + x2 + x3
  f2 =~ x4 + x5 + x6
"
fit_fiml <- acfa(mod_hs, hs, missing = "ml", verbose = FALSE, nsamp = 100)
ml_fiml <- lavaan::cfa(mod_hs, hs, missing = "ml")

test_that("the saturated-means fast path is off under FIML", {
  int <- get_inlavaan_internal(fit_fiml)
  expect_null(saturated_mean_idx(
    int$partable,
    int$lavmodel,
    int$lavsamplestats,
    int$lavdata,
    FALSE
  ))
})

test_that("FIML posterior summaries agree with lavaan's FIML estimates", {
  ld <- grep("=~", names(coef(ml_fiml)))
  expect_equal(
    unname(coef(fit_fiml)[ld]),
    unname(coef(ml_fiml)[ld]),
    tolerance = 0.1
  )
  sd_i <- sqrt(diag(vcov(fit_fiml)))
  se_l <- sqrt(diag(vcov(ml_fiml)))
  expect_equal(unname(sd_i[ld]), unname(se_l[ld]), tolerance = 0.25)
  # The skew-normal marginals sit inside the scanned window again
  expect_lt(diagnostics(fit_fiml)["scan_end_mass_max"], 0.05)
})

test_that("PPP under FIML tracks the complete-data PPP for the same data", {
  set.seed(11)
  n <- 500
  eta <- rnorm(n)
  lam <- c(0.8, 0.7, 0.6, 0.7, 0.8)
  Y <- sapply(lam, function(l) l * eta + rnorm(n, sd = sqrt(1 - l^2)))
  colnames(Y) <- paste0("y", 1:5)
  Y <- as.data.frame(Y)
  Ym <- Y
  Ym$y1[sample(n, 100)] <- NA
  Ym$y2[sample(n, 100)] <- NA
  mod <- "f =~ y1 + y2 + y3 + y4 + y5"
  fit_c <- acfa(
    mod,
    Y,
    verbose = FALSE,
    nsamp = 300,
    marginal_method = "marggaus",
    vb_correction = FALSE
  )
  fit_m <- acfa(
    mod,
    Ym,
    missing = "ml",
    verbose = FALSE,
    nsamp = 300,
    marginal_method = "marggaus",
    vb_correction = FALSE
  )
  ppp_c <- unname(fitmeasures(fit_c, "ppp"))
  ppp_m <- unname(fitmeasures(fit_m, "ppp"))
  # Before the fix the FIML value was 0.000 for every correct model
  expect_gt(ppp_m, 0.05)
  expect_lt(abs(ppp_m - ppp_c), 0.3)
})

test_that("The FIML gradient is right with several groups", {
  dat <- lavaan::HolzingerSwineford1939
  set.seed(42)
  for (v in paste0("x", 1:6)) {
    dat[[v]][sample(nrow(dat), 25)] <- NA
  }
  fit0 <- lavaan::cfa(
    "visual =~ x1 + x2 + x3\n textual =~ x4 + x5 + x6",
    dat,
    group = "school",
    missing = "ml",
    do.fit = FALSE
  )
  x <- lavaan::lav_model_get_parameters(fit0@Model) + 0.03
  loglik <- function(x) {
    inlav_model_loglik(
      x,
      fit0@Model,
      fit0@SampleStats,
      fit0@Data,
      fit0@Options,
      fit0@Cache
    )
  }
  h <- 1e-5
  grad_fd <- vapply(
    seq_along(x),
    function(i) {
      e <- replace(numeric(length(x)), i, h)
      (loglik(x + e) - loglik(x - e)) / (2 * h)
    },
    numeric(1)
  )
  grad <- inlav_model_grad(
    x,
    fit0@Model,
    fit0@SampleStats,
    fit0@Data,
    fit0@Cache
  )
  expect_equal(grad, grad_fd, tolerance = 1e-5)
})

# Pattern-wise FIML kernels against lavaan's, at the start values and at a
# perturbed point, for several model shapes
fiml_kernel_fits <- local({
  dat <- lavaan::HolzingerSwineford1939
  set.seed(1)
  for (v in paste0("x", 1:9)) {
    dat[[v]][runif(nrow(dat)) < 0.25] <- NA
  }
  mod <- "visual =~ x1 + x2 + x3\n textual =~ x4 + x5 + x6\n speed =~ x7 + x8 + x9"
  list(
    cfa = lavaan::cfa(mod, dat, missing = "ml", do.fit = FALSE),
    std_lv = lavaan::cfa(
      mod,
      dat,
      missing = "ml",
      std.lv = TRUE,
      do.fit = FALSE
    ),
    multigroup = lavaan::cfa(
      mod,
      dat,
      missing = "ml",
      group = "school",
      group.equal = c("loadings", "intercepts"),
      ceq.simple = TRUE,
      do.fit = FALSE
    ),
    sem = lavaan::sem(
      paste(mod, "\n speed ~ visual + textual\n x1 ~~ x4"),
      dat,
      missing = "ml",
      do.fit = FALSE
    )
  )
})

fiml_kernel_points <- function(fit0) {
  x0 <- lavaan::lav_model_get_parameters(fit0@Model)
  list(x0, x0 * (1 + 0.05 * sin(seq_along(x0))))
}

test_that("the pattern-wise FIML log-likelihood equals lavaan's", {
  for (fit0 in fiml_kernel_fits) {
    expect_true(fiml_fast_path(fit0@Model, fit0@SampleStats, fit0@Data))
    for (x in fiml_kernel_points(fit0)) {
      lm <- lavaan::lav_model_set_parameters(fit0@Model, x)
      imp <- lavaan::lav_model_implied(lm)
      ref <- lavaan___lav_model_loglik(
        lavdata = fit0@Data,
        lavsamplestats = fit0@SampleStats,
        lavimplied = imp,
        lavmodel = lm,
        lavoptions = fit0@Options,
        lavcache = fit0@Cache
      )$loglik
      expect_equal(
        fiml_loglik_samp(fit0@SampleStats, imp),
        ref,
        tolerance = 1e-10
      )
    }
  }
})

test_that("the pattern-wise FIML log-likelihood stays off without missing data", {
  fit0 <- lavaan::cfa(
    "f =~ x1 + x2 + x3",
    lavaan::HolzingerSwineford1939,
    do.fit = FALSE
  )
  expect_false(fiml_fast_path(fit0@Model, fit0@SampleStats, fit0@Data))
})
