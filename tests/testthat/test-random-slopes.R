## ----- Shared random-slope fixture (route A, 24 clusters) --------------------
# The `rv()` modifier makes the level-1 slope of x1 a level-2 latent
# variable. Twenty-four clusters (300 rows) keep every fit in this file to a
# couple of seconds.
d_rs <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster %in% 1:24, ]
mod_rs <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ rv('s1')*x1
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1
    s1 ~ w1
"
fit0_rs <- lavaan::sem(mod_rs, d_rs, cluster = "cluster", do.fit = FALSE)
fit_rs <- asem(
  mod_rs,
  d_rs,
  cluster = "cluster",
  verbose = FALSE,
  test = "none",
  marginal_correction = "none",
  vb_correction = FALSE,
  nsamp = 3
)
# A second fit carrying the one fit measure a random-slope model keeps
fit_rs_dic <- asem(
  mod_rs,
  d_rs,
  cluster = "cluster",
  verbose = FALSE,
  test = "dic",
  marginal_correction = "none",
  vb_correction = FALSE,
  nsamp = 3
)
# The maximum-likelihood comparator. Two variances sit slightly below zero
# on this subset, which lavaan reports and the priors keep positive.
suppressWarnings(
  fit_lav <- lavaan::sem(mod_rs, d_rs, cluster = "cluster")
)

test_that("Random slopes are detected from the lavaan model", {
  expect_true(has_random_slopes(fit0_rs@Model))

  mod_fixed <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1
  "
  fit0_fixed <- lavaan::sem(
    mod_fixed,
    d_rs,
    cluster = "cluster",
    do.fit = FALSE
  )
  expect_false(has_random_slopes(fit0_fixed@Model))
  expect_null(rs_spec(list(lavmodel = fit0_fixed@Model)))
})

test_that("rs_spec() describes the closed-form route", {
  spec <- rs_spec(list(lavmodel = fit0_rs@Model, lavcache = fit0_rs@Cache))

  expect_equal(spec$route, "A")
  expect_equal(spec$slopes, "s1")
  expect_setequal(spec$cond, c("x1", "w1"))
  expect_equal(spec$ncl, 24L)
  expect_equal(sum(spec$nobs), nrow(d_rs))

  # A fit stored before the cache was carried along cannot be described
  expect_error(
    rs_spec(list(lavmodel = fit0_rs@Model, lavcache = NULL)),
    class = "inlavaan_rs_cache"
  )
})

test_that("Random slopes: fit and posterior means", {
  expect_s4_class(fit_rs, "INLAvaan")

  # The slope variance is the boundary parameter here (its MLE is slightly
  # negative), so it is compared on its own
  keep <- setdiff(names(coef(fit_lav)), "s1~~s1.l2")
  expect_equal(coef(fit_rs)[keep], coef(fit_lav)[keep], tolerance = 0.15)
  expect_gte(unname(coef(fit_rs)["s1~~s1.l2"]), 0)

  # The stored cache travels with the fit, so rs_spec() works off the
  # INLAvaan object as well
  spec <- rs_spec(get_inlavaan_internal(fit_rs))
  expect_equal(spec$route, "A")
  expect_equal(spec$ncl, 24L)
})

test_that("Random slopes: the likelihood summaries exist", {
  expect_true(is.finite(as.numeric(logLik(fit_rs))))
  # deviance() needs the DIC components, which only the `dic` fit carries
  expect_true(is.finite(as.numeric(deviance(fit_rs_dic))))
  expect_no_error(capture.output(summary(fit_rs)))
})

test_that("Random slopes: fixed.x = FALSE is refused", {
  expect_error(
    asem(
      mod_rs,
      d_rs,
      cluster = "cluster",
      fixed.x = FALSE,
      verbose = FALSE,
      test = "none",
      nsamp = 3
    ),
    class = "inlavaan_rs_fixedx"
  )
})

test_that("Random slopes: a latent covariate keeps lavaan's own fixed.x", {
  skip_on_cran()
  # lavaan reports `fixed.x = FALSE` for a model with no observed exogenous
  # variables at all, which the gate above used to read as a user request
  # and refuse. Here the slope is carried by a latent covariate, so there is
  # nothing to hold fixed and the integral goes by quadrature.
  mod_lat <- "
    level: 1
      fx =~ x1 + x2 + x3
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*fx
    level: 2
      fb =~ y1 + y2 + y3
      s1 ~~ s1
  "
  expect_warning(
    fit_lat <- asem(
      mod_lat,
      d_rs,
      cluster = "cluster",
      integration.ngh = 5,
      verbose = FALSE,
      test = "none",
      marginal_correction = "none",
      vb_correction = FALSE,
      nsamp = 3
    ),
    class = "inlavaan_rs_route_b"
  )

  int_lat <- get_inlavaan_internal(fit_lat)
  expect_false(isTRUE(int_lat$lavmodel@fixed.x))
  expect_length(lavaan::lavNames(int_lat$partable, "ov.x"), 0L)
  expect_equal(rs_spec(int_lat)$route, "B")
  expect_true(is.finite(as.numeric(logLik(fit_lat))))
})

test_that("Random slopes: PPP is dropped from the default test", {
  # Two warnings must stay inside: lavaan's "test statistics are not
  # available ... test set to none" and the silent PPP drop under the
  # default `test`
  expect_no_warning(
    fit_std <- asem(
      mod_rs,
      d_rs,
      cluster = "cluster",
      verbose = FALSE,
      test = "standard",
      marginal_correction = "none",
      vb_correction = FALSE,
      nsamp = 3
    )
  )

  rec <- get_inlavaan_internal(fit_std, "test")
  expect_false("ppp" %in% rec$computed)
  expect_true("dic" %in% rec$computed)
  expect_true("ppp" %in% rec$requested)
  expect_true("ppp" %in% names(rec$skipped))
  expect_match(rec$skipped[["ppp"]], "no saturated model|has none")

  # Naming ppp explicitly is worth a warning
  expect_warning(
    asem(
      mod_rs,
      d_rs,
      cluster = "cluster",
      verbose = FALSE,
      test = "ppp",
      marginal_correction = "none",
      vb_correction = FALSE,
      nsamp = 3
    ),
    class = "inlavaan_rs_ppp"
  )
})

test_that("Random slopes: fitmeasures keeps only what exists", {
  fm <- fitMeasures(fit_rs_dic)

  expect_true(all(c("npar", "margloglik", "dic", "p_dic") %in% names(fm)))
  # The B-indices are scaled against the random-coefficient reference
  expect_true(all(c("BRMSEA", "BGammaHat", "BCFI") %in% names(fm)))
  gone <- c("ppp", "chisq", "cfi", "rmsea", "aic", "bic")
  expect_false(any(gone %in% names(fm)))

  # Asking for one of them by name says why nothing came back
  expect_error(
    fitMeasures(fit_rs_dic, "chisq"),
    class = "inlavaan_rs_fitmeasures"
  )
})

test_that("Random slopes: the quantities that do not exist are gated", {
  expect_error(predict(fit_rs, type = "ymis"), class = "inlavaan_rs_predict")
  expect_error(loo(fit_rs, type = "loso"), class = "inlavaan_rs_loso")
  expect_equal(dim(sampling(fit_rs, type = "lavaan", nsamp = 4)), c(4L, 19L))
})

test_that("Random slopes: summary() marks the carrier row", {
  out <- capture.output(summary(fit_rs))

  expect_true(any(grepl("(s1)", out, fixed = TRUE)))
  expect_true(any(grepl("random slope", out, fixed = TRUE)))
  # lavaan itself drops the `s1 =~ s1` marker row from the estimates
  expect_false(any(grepl("s1 =~", out, fixed = TRUE)))
})

test_that("Random slopes: latent variables come from the EB kernel", {
  p2 <- predict(fit_rs, type = "lv", level = 2L, nsamp = 5)
  expect_length(p2, 5L)
  expect_true(all(vapply(p2, nrow, integer(1L)) == 24L))
  expect_setequal(colnames(p2[[1L]]), c("fb", "s1"))
  # The old implied-moment path had no slope at all, and would have handed
  # back the same population mean for every cluster
  expect_gt(stats::sd(p2[[1L]][, "s1"]), 0)

  p1 <- predict(fit_rs, type = "lv", level = 1L, nsamp = 5)
  expect_true(all(vapply(p1, nrow, integer(1L)) == nrow(d_rs)))
  expect_setequal(colnames(p1[[1L]]), "fw")

  # At fixed parameters the kernel is lavPredict() to the last bit
  int <- get_inlavaan_internal(fit_rs)
  lm_x <- lavaan::lav_model_set_parameters(int$lavmodel, coef(fit_lav))
  f2 <- fit_lav
  f2@Model <- lm_x
  eb <- lavaan___lav_mvn_cl_rs_eb(
    lavmodel = lm_x,
    lavdata = int$lavdata,
    lavcache = int$lavcache
  )
  expect_equal(
    unname(as.matrix(lavaan::lavPredict(f2, level = 2))),
    unname(eb$l2),
    tolerance = 1e-10
  )

  # Level 2 draws carry each cluster's conditional spread, not only the
  # parameter uncertainty. The spread is taken at the posterior mode, where
  # the slope variance is positive (its MLE on this subset is not).
  set.seed(4)
  p2_many <- predict(fit_rs, type = "lv", level = 2L, nsamp = 200)
  s1_draws <- vapply(p2_many, function(fs) fs[, "s1"], numeric(24L))
  eb_se <- lavaan___lav_mvn_cl_rs_eb(
    lavmodel = lavaan::lav_model_set_parameters(
      int$lavmodel,
      pars_to_x(int$theta_star, int$partable)
    ),
    lavdata = int$lavdata,
    lavcache = int$lavcache,
    se = TRUE
  )$se2[, "s1"]
  expect_true(all(eb_se > 0))
  expect_gt(stats::median(apply(s1_draws, 1L, stats::sd) / eb_se), 0.9)
})

## ----- Route B (Gauss-Hermite quadrature) ------------------------------------

test_that("Random slopes: the closed-form route is silent", {
  expect_no_warning(
    asem(
      mod_rs,
      d_rs,
      cluster = "cluster",
      verbose = FALSE,
      test = "none",
      marginal_correction = "none",
      vb_correction = FALSE,
      nsamp = 3
    )
  )
})

# A covariate that lives at both levels is split into a latent
# within-cluster part, and that part has to be integrated out by
# Gauss-Hermite quadrature rather than in closed form. Twenty clusters of
# eight keep each fit near a second.
set.seed(2)
J <- 20
n <- 8
cl <- rep(seq_len(J), each = n)
xb <- rnorm(J)
xw <- rnorm(J * n)
x1 <- xb[cl] + xw
u0 <- rnorm(J, 0, sqrt(0.5))
u1 <- rnorm(J, 0, 0.5)
y1 <- 1 + u0[cl] + (0.5 + u1[cl]) * xw + rnorm(J * n)
d_b <- data.frame(y1 = y1, x1 = x1, cluster = cl)
mod_b <- "
  level: 1
    y1 ~ rv('s1')*x1
  level: 2
    y1 ~ x1
    y1 ~~ y1
    s1 ~~ s1
"
fit_route_b <- function(ngh) {
  suppressWarnings(
    asem(
      mod_b,
      d_b,
      cluster = "cluster",
      integration.ngh = ngh,
      verbose = FALSE,
      test = "none",
      nsamp = 3,
      marginal_correction = "none",
      vb_correction = FALSE
    )
  )
}

test_that("Random slopes: the quadrature route warns and honours ngh", {
  skip_on_cran()
  expect_warning(
    fit_b <- asem(
      mod_b,
      d_b,
      cluster = "cluster",
      integration.ngh = 5,
      verbose = FALSE,
      test = "none",
      nsamp = 3,
      marginal_correction = "none",
      vb_correction = FALSE
    ),
    class = "inlavaan_rs_route_b"
  )

  spec <- rs_spec(get_inlavaan_internal(fit_b))
  expect_equal(spec$route, "B")
  # The node count used to be dropped on the way to lavaan, leaving every
  # quadrature fit on lavaan's default of 21 nodes
  expect_equal(spec$ngh, 5)
  expect_true(is.finite(fitMeasures(fit_b, "margloglik")))

  fit_b_lav <- suppressWarnings(
    lavaan::sem(mod_b, d_b, cluster = "cluster", integration.ngh = 5)
  )
  expect_lt(
    max(abs(coef(fit_b)[names(coef(fit_b_lav))] - coef(fit_b_lav))),
    0.3
  )
})

test_that("Random slopes: the quadrature route warns once per fit", {
  skip_on_cran()
  # The fit announces the route on its way in, and its own LOO pass used to
  # announce it a second time
  seen <- 0L
  suppressWarnings(withCallingHandlers(
    asem(
      mod_b,
      d_b,
      cluster = "cluster",
      integration.ngh = 5,
      verbose = FALSE,
      test = "loo",
      nsamp = 3,
      marginal_correction = "none",
      vb_correction = FALSE
    ),
    inlavaan_rs_route_b = function(cond) {
      seen <<- seen + 1L
      invokeRestart("muffleWarning")
    }
  ))
  expect_equal(seen, 1L)

  # A user's own call still says which route it is scoring
  expect_warning(loo(fit_route_b(5)), class = "inlavaan_rs_route_b")
})

test_that("Random slopes: comparing quadrature fits needs one node count", {
  skip_on_cran()
  # The quadrature error moves the log-likelihood by an amount comparable
  # with the differences read off a comparison table, so two node counts
  # put the two fits on different scales.
  fit_b5 <- fit_route_b(5)
  fit_b5b <- fit_route_b(5)
  fit_b7 <- fit_route_b(7)

  expect_error(
    compare(fit_b5, fit_b7),
    class = "inlavaan_rs_compare_ngh"
  )

  # One node count throughout is all the guard asks for
  expect_no_error(cmp <- compare(fit_b5, fit_b5b))
  expect_named(cmp, c("Model", "npar", "Marg.Loglik", "logBF"))
  expect_equal(diff(cmp$Marg.Loglik), 0, tolerance = 1e-4)
})

test_that("Random slopes: the quadrature route has averaged moments only", {
  skip_on_cran()
  fit_b <- fit_route_b(5)
  # The averaged moments need only second moments, which are exact here
  f <- fitted(fit_b)
  expect_true(all(is.finite(f$within$cov)))
  expect_true(all(is.finite(residuals(fit_b)$within$cov)))
  set.seed(3)
  std <- standardisedsolution(fit_b, nsamp = 10)
  expect_true(all(is.finite(std$est.std)))
  # The generative draws need no quadrature, the data generator does
  expect_length(sampling(fit_b, type = "implied", nsamp = 2), 2L)
  expect_equal(dim(sampling(fit_b, type = "observed", nsamp = 2)), c(2L, 2L))
  expect_error(simulate(fit_b, nsim = 1), class = "inlavaan_rs_simulate")
  expect_error(
    bfit_indices(fit_b, rescale = "MCMC", nsamp = 5),
    class = "inlavaan_rs_bfit"
  )
  expect_false("BRMSEA" %in% names(fitMeasures(fit_b)))
  expect_error(fitMeasures(fit_b, "BRMSEA"), class = "inlavaan_rs_bfit")
  expect_error(fitted(fit_b, type = "casewise"), class = "inlavaan_rs_casewise")
  expect_error(predict(fit_b, type = "yhat"), class = "inlavaan_rs_casewise")
  # Each cluster is a mixture over the quadrature nodes
  expect_error(
    fitted(fit_b, per_cluster = TRUE),
    class = "inlavaan_rs_per_cluster"
  )
  expect_error(
    residuals(fit_b, per_cluster = TRUE),
    class = "inlavaan_rs_per_cluster"
  )
})

## ----- Equality constraints --------------------------------------------------
# lavaan builds the random-slope gradient from the packed free parameters,
# one entry per equality group, where every other model returns one entry
# per free partable row. A constrained fit used to mismatch the chain rule
# row by row -- recycling warnings all the way into a failed Cholesky
# factorisation -- so these fits are the regression test for the scatter in
# rs_unpack_grad().
mod_ceq <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ rv('s1')*x1
  level: 2
    fb =~ y1 + c*y2 + c*y3
    fb ~ w1
    s1 ~ w1
"
fit_ceq <- NULL

# The log-likelihood and its gradient in the theta space the optimiser
# works in, assembled exactly as joint_lp()/joint_lp_grad() assemble them
# but without the priors
ceq_theta_loglik <- function(int, opts, pars) {
  x <- pars_to_x(as.numeric(int$lavmodel@ceq.simple.K %*% pars), int$partable)
  inlav_model_loglik(
    x,
    int$lavmodel,
    int$lavsamplestats,
    int$lavdata,
    opts,
    int$lavcache
  )
}

ceq_theta_grad <- function(int, pars) {
  pt <- int$partable
  pars_unpacked <- as.numeric(int$lavmodel@ceq.simple.K %*% pars)
  x <- pars_to_x(pars_unpacked, pt)
  jcb <- mapply(function(f, z) f(z), pt$ginv_prime[pt$free > 0], pars_unpacked)
  gll <- inlav_model_grad(
    x,
    int$lavmodel,
    int$lavsamplestats,
    int$lavdata,
    int$lavcache
  )
  out <- jcb * attr(x, "sd1sd2") * gll
  jcb_mat <- attr(x, "jcb_mat")
  if (!is.null(jcb_mat)) {
    jcb_mat <- rbind(jcb_mat)
    for (k in seq_len(nrow(jcb_mat))) {
      i <- jcb_mat[k, 1]
      out[i] <- out[i] + jcb_mat[k, 3] * gll[jcb_mat[k, 2]]
    }
  }
  as.numeric(out %*% int$lavmodel@ceq.simple.K)
}

test_that("Random slopes: a cross-level equality constraint fits", {
  expect_no_warning(
    fit_ceq <<- asem(
      mod_ceq,
      d_rs,
      cluster = "cluster",
      verbose = FALSE,
      test = "none",
      marginal_correction = "none",
      vb_correction = FALSE,
      nsamp = 3
    )
  )

  fit_ceq_lav <- suppressWarnings(
    lavaan::sem(mod_ceq, d_rs, cluster = "cluster")
  )
  keep <- setdiff(names(coef(fit_ceq_lav)), "s1~~s1.l2")
  expect_equal(coef(fit_ceq)[keep], coef(fit_ceq_lav)[keep], tolerance = 0.15)
})

test_that("Random slopes: the constrained gradient is the exact one", {
  int <- get_inlavaan_internal(fit_ceq)
  opts <- fit_ceq@Options
  opts$estimator <- "ML"
  theta <- int$theta_star

  g_an <- ceq_theta_grad(int, theta)
  h <- 1e-5
  g_fd <- vapply(
    seq_along(theta),
    function(k) {
      th_up <- th_dn <- theta
      th_up[k] <- th_up[k] + h
      th_dn[k] <- th_dn[k] - h
      (ceq_theta_loglik(int, opts, th_up) -
        ceq_theta_loglik(int, opts, th_dn)) /
        (2 * h)
    },
    numeric(1)
  )
  expect_length(g_an, length(theta))
  expect_lt(max(abs(g_an - g_fd) / pmax(abs(g_fd), 1)), 1e-5)
})

test_that("Random slopes: LOCO carries the constraint too", {
  int <- get_inlavaan_internal(fit_ceq)
  spec <- rs_spec(int)
  opts <- fit_ceq@Options
  opts$estimator <- "ML"
  theta <- int$theta_star

  res_ceq <- loo(fit_ceq)
  expect_equal(res_ceq$n_units, 24L)
  expect_equal(
    sum(res_ceq$per_unit$l_star),
    ceq_theta_loglik(int, opts, theta),
    tolerance = 1e-6
  )

  units <- 1:2
  s_an <- loco_rs_scores_theta(
    theta,
    spec$rs,
    int$lavmodel,
    int$partable,
    units
  )
  h <- 1e-5
  s_fd <- vapply(
    seq_along(theta),
    function(k) {
      th_up <- th_dn <- theta
      th_up[k] <- th_up[k] + h
      th_dn[k] <- th_dn[k] - h
      (loco_rs_loglik_all(th_up, spec$rs, int$lavmodel, int$partable, units) -
        loco_rs_loglik_all(th_dn, spec$rs, int$lavmodel, int$partable, units)) /
        (2 * h)
    },
    numeric(length(units))
  )
  expect_equal(dim(s_an), dim(s_fd))
  expect_lt(max(abs(s_an - s_fd) / pmax(abs(s_fd), 1)), 1e-5)
})

test_that("Random slopes: constrained variances and shared slope labels fit", {
  # Two residual variances tied together: one group, all of it on the log
  # scale, so the scatter is exact
  mod_var <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1
      y1 ~~ a*y1
      y2 ~~ a*y2
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1
      s1 ~ w1
  "
  expect_no_warning(
    fit_var <- asem(
      mod_var,
      d_rs,
      cluster = "cluster",
      verbose = FALSE,
      test = "none",
      marginal_correction = "none",
      vb_correction = FALSE,
      nsamp = 3
    )
  )
  # One free parameter, reported once for each row it is tied to
  a_vals <- coef(fit_var)[names(coef(fit_var)) == "a"]
  expect_length(a_vals, 2L)
  expect_equal(a_vals[[1L]], a_vals[[2L]])
  expect_gt(a_vals[[1L]], 0)

  # Two random slopes sharing one level-2 regression coefficient
  mod_two <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1 + rv('s2')*x2
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1
      s1 ~ v*w1
      s2 ~ v*w1
  "
  expect_no_warning(
    fit_two <- asem(
      mod_two,
      d_rs,
      cluster = "cluster",
      verbose = FALSE,
      test = "none",
      marginal_correction = "none",
      vb_correction = FALSE,
      nsamp = 3
    )
  )
  expect_setequal(rs_spec(get_inlavaan_internal(fit_two))$slopes, c("s1", "s2"))
})

test_that("Random slopes: an inexact equality group is refused", {
  # A loading and a variance under one label: the two carry different
  # transformations, so the group total cannot be split between them. The
  # general check refuses this for every model, before the random-slope one.
  mod_mix <- "
    level: 1
      fw =~ y1 + a*y2 + y3
      fw ~ rv('s1')*x1
      y1 ~~ a*y1
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1
      s1 ~ w1
  "
  expect_error(
    asem(
      mod_mix,
      d_rs,
      cluster = "cluster",
      verbose = FALSE,
      test = "none",
      nsamp = 3
    ),
    "cannot hold these parameters equal"
  )

  # A covariance in the group: its Jacobian carries the two standard
  # deviations, which differ from row to row
  mod_cov <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1
      s1 ~ w1
      fb ~~ a*s1
      y1 ~~ a*y2
  "
  expect_error(
    asem(
      mod_cov,
      d_rs,
      cluster = "cluster",
      verbose = FALSE,
      test = "none",
      nsamp = 3
    ),
    class = "inlavaan_rs_ceq"
  )
})

test_that("Random slopes: a zero slope variance is refused on the quadrature route", {
  # The slope's covariate x1 enters at both levels, so the slope is
  # integrated by quadrature, which cannot handle a zero variance
  mod_zero <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ x1 + w1
      s1 ~~ 0*s1
  "
  expect_error(
    asem(
      mod_zero,
      d_rs,
      cluster = "cluster",
      integration.ngh = 3,
      verbose = FALSE,
      test = "none",
      nsamp = 3
    ),
    class = "inlavaan_rs_zero_var"
  )
})

test_that("Random slopes: composites are refused", {
  mod_comp <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1
    level: 2
      cb <~ y1 + y2 + y3
      cb ~ w1
  "
  expect_error(
    asem(
      mod_comp,
      d_rs,
      cluster = "cluster",
      verbose = FALSE,
      test = "none",
      nsamp = 3
    ),
    class = "inlavaan_rs_composite"
  )
})
