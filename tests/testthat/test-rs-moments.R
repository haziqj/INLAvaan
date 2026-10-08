## ----- Shared fixtures -------------------------------------------------------
# The 24-cluster subset of Demo.twolevel used by the other random-slope tests.
# The internal checks run on lavaan objects, which are fast, and the method
# checks on one INLAvaan fit.
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

# The Gaussian log-density of every cluster under its stacked moments. A
# missing outcome drops out of the stacked vector, which is the FIML
# marginal.
rs_stacked_loglik <- function(lavmodel, rs, lavdata) {
  imp <- rs_implied_pieces(lavmodel, rs$info)
  vapply(
    rs_cluster_data(lavdata, rs),
    function(cl) {
      mom <- rs_cluster_moments(imp, rs$info, cl$X, cl$exo_b)
      y <- c(as.numeric(t(cl$Y)), cl$zb)
      ok <- !is.na(y)
      if (!any(ok)) {
        return(0)
      }
      R <- chol(mom$cov[ok, ok, drop = FALSE])
      z <- backsolve(R, y[ok] - mom$mean[ok], transpose = TRUE)
      -0.5 * sum(ok) * log(2 * pi) - sum(log(diag(R))) - 0.5 * sum(z^2)
    },
    numeric(1)
  )
}

rs_kernel_loglik <- function(lavmodel, rs) {
  ll <- lavaan___lav_mvn_cl_rs_m2ll(
    lavmodel = lavmodel,
    rs = rs,
    log2pi = TRUE,
    minus_two = FALSE,
    per_cluster = TRUE
  )
  as.numeric(attr(ll, "loglik.cluster"))
}

# A zero-variance random-slope fit moved to the estimates of the matching
# fixed-slope fit, so that the two are compared at one parameter value
# rather than at two optimiser end points. The slope mean takes the fixed
# slope.
move_to_fixed_slope <- function(fit_rs0, fit_fx) {
  pt0 <- lavaan::parTable(fit_rs0)
  ptf <- lavaan::parTable(fit_fx)
  key0 <- paste(pt0$lhs, pt0$op, pt0$rhs, pt0$block)
  keyf <- paste(ptf$lhs, ptf$op, ptf$rhs, ptf$block)
  key0[key0 == "s1 ~1  2"] <- "fw ~ x1 1"
  est <- ptf$est[match(key0, keyf)]
  est[is.na(est)] <- pt0$est[is.na(est)]
  free <- pt0$free > 0
  x <- est[free][order(pt0$free[free])]
  fit_rs0@Model <- lavaan::lav_model_set_parameters(fit_rs0@Model, x)
  fit_rs0@ParTable$est <- est
  fit_rs0
}

mod_rs0 <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ rv('s1')*x1 + x2
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1
    s1 ~~ 0*s1
"
mod_fx <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1 + x2
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1
"

## ----- Per-cluster moments ---------------------------------------------------

test_that("Per-cluster moments give lavaan's cluster kernel", {
  mods <- list(
    mod_rs,
    "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1 + rv('s2')*x2 + x3
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1 + w2
      s1 ~~ s2
    ",
    "
    level: 1
      y1 ~ rv('s1')*x1 + x2
    level: 2
      y1 ~ w1
      s1 ~ w1
      s1 ~~ y1
    ",
    "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1
    level: 2
      fb =~ y1 + y2 + y3 + w2
      fb ~ w1
    ",
    "
    level: within
      fw =~ y1 + a*y2 + a*y3
      fw ~ rv('s1')*x1
    level: between
      fb =~ y1 + y2 + y3
      fb ~ w1
    "
  )
  for (m in mods) {
    fit <- suppressWarnings(lavaan::sem(m, d_rs, cluster = "cluster"))
    rs <- fit@Cache[[1L]]$rs
    expect_lt(
      max(abs(
        rs_stacked_loglik(fit@Model, rs, fit@Data) -
          rs_kernel_loglik(fit@Model, rs)
      )),
      1e-8
    )
  }

  # FIML: outcome cells missing at random and one row missing on all three
  d_mis <- d_rs
  set.seed(2)
  for (v in c("y1", "y2", "y3")) {
    d_mis[[v]][stats::runif(nrow(d_mis)) < 0.05] <- NA
  }
  d_mis[3, c("y1", "y2", "y3")] <- NA
  fit <- suppressWarnings(lavaan::sem(
    mod_rs,
    d_mis,
    cluster = "cluster",
    missing = "ml"
  ))
  rs <- fit@Cache[[1L]]$rs
  expect_lt(
    max(abs(
      rs_stacked_loglik(fit@Model, rs, fit@Data) -
        rs_kernel_loglik(fit@Model, rs)
    )),
    1e-8
  )
})

## ----- Averaged moments ------------------------------------------------------

test_that("A zero slope variance gives the fixed-slope moments", {
  fit_rs0 <- suppressWarnings(lavaan::sem(mod_rs0, d_rs, cluster = "cluster"))
  fit_fx <- suppressWarnings(lavaan::sem(mod_fx, d_rs, cluster = "cluster"))
  fit_rs0 <- move_to_fixed_slope(fit_rs0, fit_fx)
  avg <- rs_avg_implied(fit_rs0@Model, fit_rs0@Cache[[1L]]$rs$info)
  fx <- lavaan::lav_model_implied(fit_fx@Model)
  for (b in 1:2) {
    expect_lt(max(abs(avg$cov[[b]] - fx$cov[[b]])), 1e-8)
    expect_lt(max(abs(avg$mean[[b]] - fx$mean[[b]])), 1e-8)
  }
  # lavaan's own moments drop the mean slope as well as its variance
  lav <- lavaan::lav_model_implied(fit_rs0@Model)
  expect_gt(max(abs(lav$cov[[1]] - fx$cov[[1]])), 0.1)
})

test_that("Averaged moments are the covariate average of the cluster ones", {
  # Shift x1 well away from zero so that the between-level slope term counts
  d_shift <- d_rs
  d_shift$x1 <- d_shift$x1 + 2
  fit <- suppressWarnings(lavaan::sem(
    "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1 + x2
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1
      s1 ~ w1
      s1 ~~ fb
    ",
    d_shift,
    cluster = "cluster"
  ))
  rs <- fit@Cache[[1L]]$rs
  info <- rs$info
  imp <- rs_implied_pieces(fit@Model, info)
  # Every observed covariate row crossed with every cluster's w1: the
  # within-only and between-only covariates independent, each with its own
  # sample distribution, as in lavaan's two-level layout
  X <- fit@Data@X[[1L]][, info$x.data.idx, drop = FALSE]
  W <- rs$stats$exo.b
  grid <- expand.grid(i = seq_len(nrow(X)), j = seq_len(nrow(W)))
  p1 <- info$p1
  M <- matrix(0, nrow(grid), p1)
  V <- matrix(0, p1, p1)
  for (r in seq_len(nrow(grid))) {
    mom <- rs_cluster_moments(
      imp,
      info,
      X[grid$i[r], , drop = FALSE],
      W[grid$j[r], ]
    )
    M[r, ] <- mom$mean[seq_len(p1)]
    V <- V + mom$cov[seq_len(p1), seq_len(p1)] / nrow(grid)
  }
  Mc <- sweep(M, 2, colMeans(M))
  Xg <- X[grid$i, , drop = FALSE]
  Xc <- sweep(Xg, 2, colMeans(Xg))

  avg <- rs_avg_implied(fit@Model, info)
  ov <- fit@Model@dimNames[[1L]][[1L]]
  ys <- match(info$y.names, ov)
  xs <- match(info$x.names, ov)
  ov_b <- fit@Model@dimNames[[fit@Model@nmat[1] + 1L]][[1L]]
  ysb <- match(info$y.names, ov_b)
  expect_lt(
    max(abs(
      avg$cov[[1]][ys, ys] +
        avg$cov[[2]][ysb, ysb] -
        (V + crossprod(Mc) / nrow(grid))
    )),
    1e-10
  )
  expect_lt(
    max(abs(avg$cov[[1]][ys, xs] - crossprod(Mc, Xc) / nrow(grid))),
    1e-10
  )
  expect_lt(
    max(abs(avg$mean[[1]][ys] + avg$mean[[2]][ysb] - colMeans(M))),
    1e-10
  )
})

test_that("A within-only outcome with a shifted covariate is refused", {
  d_shift <- d_rs
  d_shift$x1 <- d_shift$x1 + 1
  fit <- suppressWarnings(lavaan::sem(
    "
    level: 1
      y1 ~ rv('s1')*x1
      y2 ~ x2
    level: 2
      y2 ~~ y2
    ",
    d_shift,
    cluster = "cluster"
  ))
  expect_error(
    rs_avg_implied(fit@Model, fit@Cache[[1L]]$rs$info),
    class = "inlavaan_rs_within_only"
  )
})

## ----- Monte Carlo -----------------------------------------------------------
# Simulation straight from the model equations, with the true values fixed
# in the syntax so that lavaan's do.fit = FALSE model sits at the truth

mc_true <- list(
  lam_w = c(1, 0.8, 0.6),
  th_w = c(0.5, 0.6, 0.7),
  psi_fw = 0.4,
  gam_x2 = 0.3,
  lam_b = c(1, 0.7, 0.5),
  th_b = c(0.2, 0.15, 0.1),
  nu_b = c(0.5, -0.2, 0.1),
  b_fb = 0.4,
  psi_fb = 0.6,
  alpha_s = 0.5,
  g_s = 0.3,
  psi_s = 0.25,
  c_fb_s = 0.1
)
mc_model <- with(
  mc_true,
  sprintf(
    "
  level: 1
    fw =~ %g*y1 + %g*y2 + %g*y3
    fw ~ rv('s1')*x1 + %g*x2
    fw ~~ %g*fw
    y1 ~~ %g*y1
    y2 ~~ %g*y2
    y3 ~~ %g*y3
  level: 2
    fb =~ %g*y1 + %g*y2 + %g*y3
    y1 ~ %g*1
    y2 ~ %g*1
    y3 ~ %g*1
    y1 ~~ %g*y1
    y2 ~~ %g*y2
    y3 ~~ %g*y3
    fb ~ %g*w1
    fb ~~ %g*fb
    s1 ~ %g*1 + %g*w1
    s1 ~~ %g*s1 + %g*fb
  ",
    lam_w[1],
    lam_w[2],
    lam_w[3],
    gam_x2,
    psi_fw,
    th_w[1],
    th_w[2],
    th_w[3],
    lam_b[1],
    lam_b[2],
    lam_b[3],
    nu_b[1],
    nu_b[2],
    nu_b[3],
    th_b[1],
    th_b[2],
    th_b[3],
    b_fb,
    psi_fb,
    alpha_s,
    g_s,
    psi_s,
    c_fb_s
  )
)

# n_rep replicates of one cluster with covariates X (columns x1, x2) and w1
mc_cluster <- function(X, w, n_rep) {
  tru <- mc_true
  S_b <- matrix(c(tru$psi_fb, tru$c_fb_s, tru$c_fb_s, tru$psi_s), 2, 2)
  U <- matrix(stats::rnorm(2 * n_rep), n_rep, 2) %*% chol(S_b)
  fb <- tru$b_fb * w + U[, 1]
  s <- tru$alpha_s + tru$g_s * w + U[, 2]
  yb <- vapply(
    1:3,
    function(k) {
      tru$nu_b[k] +
        tru$lam_b[k] * fb +
        stats::rnorm(n_rep, sd = sqrt(tru$th_b[k]))
    },
    numeric(n_rep)
  )
  yb <- matrix(yb, n_rep, 3)
  out <- matrix(0, n_rep, 3 * nrow(X))
  for (i in seq_len(nrow(X))) {
    fw <- s *
      X[i, 1] +
      tru$gam_x2 * X[i, 2] +
      stats::rnorm(n_rep, sd = sqrt(tru$psi_fw))
    for (k in 1:3) {
      out[, (i - 1) * 3 + k] <- yb[, k] +
        tru$lam_w[k] * fw +
        stats::rnorm(n_rep, sd = sqrt(tru$th_w[k]))
    }
  }
  out
}

test_that("Monte Carlo: the per-cluster moments", {
  skip_on_cran()
  fit <- suppressWarnings(lavaan::sem(
    mc_model,
    d_rs,
    cluster = "cluster",
    do.fit = FALSE
  ))
  rs <- fit@Cache[[1L]]$rs
  imp <- rs_implied_pieces(fit@Model, rs$info)
  cl <- rs_cluster_data(fit@Data, rs)[[7L]]
  mom <- rs_cluster_moments(imp, rs$info, cl$X, cl$exo_b)
  n_rep <- 20000
  set.seed(7)
  sim <- mc_cluster(cl$X, cl$exo_b, n_rep)
  z_mean <- (colMeans(sim) - mom$mean) / sqrt(diag(mom$cov) / n_rep)
  se_cov <- sqrt((mom$cov^2 + tcrossprod(diag(mom$cov))) / n_rep)
  z_cov <- (stats::cov(sim) - mom$cov) / se_cov
  expect_lt(max(abs(z_mean)), 4.5)
  expect_lt(max(abs(z_cov[upper.tri(z_cov, diag = TRUE)])), 4.5)
  expect_lt(mean(abs(z_cov[upper.tri(z_cov, diag = TRUE)]) > 2), 0.1)
})

test_that("Monte Carlo: the averaged moments", {
  skip_on_cran()
  set.seed(8)
  n_clus <- 4000
  n_per <- 8
  dat <- do.call(
    rbind,
    lapply(seq_len(n_clus), function(j) {
      X <- cbind(stats::rnorm(n_per, 1, 1.2), stats::rnorm(n_per))
      w <- stats::rnorm(1, 0.3, 1)
      y <- matrix(mc_cluster(X, w, 1L), n_per, 3, byrow = TRUE)
      data.frame(
        cluster = j,
        y1 = y[, 1],
        y2 = y[, 2],
        y3 = y[, 3],
        x1 = X[, 1],
        x2 = X[, 2],
        w1 = w
      )
    })
  )
  fit <- suppressWarnings(lavaan::sem(
    mc_model,
    dat,
    cluster = "cluster",
    do.fit = FALSE
  ))
  avg <- rs_avg_implied(fit@Model, fit@Cache[[1L]]$rs$info)
  h1 <- lavaan::lavInspect(
    lavaan::sem(
      "
      level: 1
        y1 ~~ y2 + y3
        y2 ~~ y3
      level: 2
        y1 ~~ y2 + y3
        y2 ~~ y3
      ",
      dat[, c("cluster", "y1", "y2", "y3")],
      cluster = "cluster"
    ),
    "implied"
  )
  ys <- c("y1", "y2", "y3")
  # Four Monte Carlo standard deviations, from 40 replicates of this design
  expect_lt(max(abs(avg$cov[[1]][1:3, 1:3] - h1$within$cov[ys, ys])), 0.07)
  expect_lt(max(abs(avg$cov[[2]][1:3, 1:3] - h1$cluster$cov[ys, ys])), 0.13)
  # lavaan's own moments are off by far more
  lav <- lavaan::lav_model_implied(fit@Model)
  expect_gt(max(abs(lav$cov[[1]][1:3, 1:3] - h1$within$cov[ys, ys])), 0.3)
})

## ----- Standardised values ---------------------------------------------------

test_that("Standardised values match a hand derivation", {
  # y1 = (nu + u0_j) + s_j x1 + e, s_j ~ N(mu_s, sig_s2), x1 off centre
  th_w <- 0.8
  th_b <- 0.3
  nu_b <- 0.2
  mu_s <- 0.6
  sig_s2 <- 0.25
  d_shift <- d_rs
  d_shift$x1 <- d_shift$x1 + 1.5
  fit <- lavaan::sem(
    sprintf(
      "
      level: 1
        y1 ~ rv('s1')*x1
        y1 ~~ %g*y1
      level: 2
        y1 ~ %g*1
        y1 ~~ %g*y1
        s1 ~ %g*1
        s1 ~~ %g*s1
      ",
      th_w,
      nu_b,
      th_b,
      mu_s,
      sig_s2
    ),
    d_shift,
    cluster = "cluster",
    do.fit = FALSE
  )
  # lavaan's fixed.x moments of a within-only covariate: total, ML divisor
  x <- d_shift$x1
  s2_x <- mean((x - mean(x))^2)
  var_w <- th_w + (mu_s^2 + sig_s2) * s2_x
  var_b <- th_b + sig_s2 * mean(x)^2
  hand <- c(
    "y1 ~ x1 1" = mu_s * sqrt(s2_x / var_w),
    "y1 ~~ y1 1" = th_w / var_w,
    "y1 ~1  2" = nu_b / sqrt(var_b),
    "y1 ~~ y1 2" = th_b / var_b,
    "s1 ~1  2" = mu_s * sqrt(s2_x / var_w),
    "s1 ~~ s1 2" = sig_s2 * s2_x / var_w
  )
  pt <- fit@ParTable
  std <- rs_std_values(fit, fit@Model, pt$est, fit@Cache[[1L]]$rs$info)
  got <- std[match(names(hand), paste(pt$lhs, pt$op, pt$rhs, pt$block))]
  expect_equal(unname(got), unname(hand), tolerance = 1e-10)
  # The within R-square splits into the mean slope and the slope variance
  expect_equal(
    unname(got[1]^2 + got[6]),
    unname(1 - got[2]),
    tolerance = 1e-10
  )
})

test_that("A zero slope variance gives the fixed-slope standardised values", {
  fit_rs0 <- suppressWarnings(lavaan::sem(mod_rs0, d_rs, cluster = "cluster"))
  fit_fx <- suppressWarnings(lavaan::sem(mod_fx, d_rs, cluster = "cluster"))
  fit_rs0 <- move_to_fixed_slope(fit_rs0, fit_fx)
  info <- fit_rs0@Cache[[1L]]$rs$info
  pt0 <- fit_rs0@ParTable
  ptf <- lavaan::parTable(fit_fx)
  key0 <- paste(pt0$lhs, pt0$op, pt0$rhs, pt0$block)
  keyf <- paste(ptf$lhs, ptf$op, ptf$rhs, ptf$block)
  common <- intersect(key0, keyf)
  for (tp in c("std.lv", "std.all", "std.nox")) {
    a <- rs_std_values(fit_rs0, fit_rs0@Model, pt0$est, info, type = tp)
    b <- lavaan::standardizedSolution(
      fit_fx,
      type = tp,
      remove_eq = FALSE,
      remove_ineq = FALSE,
      remove_def = FALSE
    )$est.std
    expect_false(anyNA(a[match(common, key0)]))
    expect_lt(max(abs(a[match(common, key0)] - b[match(common, keyf)])), 1e-8)
  }
})

test_that("Defined parameters are re-evaluated on the slope metric", {
  fit <- suppressWarnings(lavaan::sem(
    paste(mod_rs, "s1 ~ a*1\n twice := 2*a"),
    d_rs,
    cluster = "cluster"
  ))
  pt <- fit@ParTable
  std <- rs_std_values(fit, fit@Model, pt$est, fit@Cache[[1L]]$rs$info)
  expect_equal(
    std[pt$op == ":="],
    2 * std[pt$lhs == "s1" & pt$op == "~1"]
  )
})

test_that("A slope shared by paths on different scales has no metric", {
  fit <- suppressWarnings(lavaan::sem(
    "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1 + rv('s1')*x2
    level: 2
      fb =~ y1 + y2 + y3
    ",
    d_rs,
    cluster = "cluster"
  ))
  pt <- fit@ParTable
  std <- rs_std_values(fit, fit@Model, pt$est, fit@Cache[[1L]]$rs$info)
  expect_equal(attr(std, "shared"), "s1")
  expect_true(is.na(std[pt$lhs == "s1" & pt$op == "~1"]))
  # The two carriers still hold their own standardised mean slopes
  expect_true(all(is.finite(std[nzchar(pt$rv) & pt$op == "~"])))
})

## ----- Methods on a fit ------------------------------------------------------

test_that("fitted() and residuals() give the averaged moments", {
  info <- rs_spec(get_inlavaan_internal(fit_rs))$rs$info
  avg <- rs_avg_implied(fit_rs@Model, info)
  f <- fitted(fit_rs)
  expect_named(f, c("within", "cluster"))
  expect_equal(
    unclass(f$within$cov)[1:4, 1:4],
    avg$cov[[1]],
    ignore_attr = TRUE
  )
  expect_equal(
    unclass(f$cluster$mean),
    as.numeric(avg$mean[[2]]),
    ignore_attr = TRUE
  )
  expect_equal(fitted.values(fit_rs), f)
  # The slope adds to the within variance of the outcomes
  lav <- lavaan::lav_model_implied(fit_rs@Model)
  expect_true(all(diag(f$within$cov)[1:3] > diag(lav$cov[[1]])[1:3]))

  r <- residuals(fit_rs)
  obs <- lavaan::lavInspect(fit_rs, "sampstat")
  expect_equal(
    unclass(r$within$cov),
    unclass(obs$within$cov) - unclass(f$within$cov),
    ignore_attr = TRUE
  )
  expect_equal(resid(fit_rs), r)
  expect_named(residuals(fit_rs, type = "cor.bentler"), c("within", "cluster"))
})

test_that("Per-cluster moments and residuals", {
  f <- fitted(fit_rs, per_cluster = TRUE)
  r <- residuals(fit_rs, per_cluster = TRUE)
  ids <- as.character(unique(d_rs$cluster))
  expect_length(f, 24L)
  expect_setequal(names(f), ids)
  expect_named(f[[1]], c("cov", "mean"))
  expect_equal(rownames(f[[1]]$cov), c("y1", "y2", "y3", "x1"))
  # Complete data: residual = the cluster's sample moments minus fitted
  d1 <- d_rs[d_rs$cluster == as.numeric(names(f)[1]), c("y1", "y2", "y3", "x1")]
  s1 <- stats::cov(d1) * (nrow(d1) - 1) / nrow(d1)
  expect_equal(r[[1]]$cov, s1 - f[[1]]$cov, ignore_attr = TRUE)
  expect_equal(r[[1]]$mean, colMeans(d1) - f[[1]]$mean, ignore_attr = TRUE)
  # The covariates are held at their values
  expect_equal(unname(r[[1]]$cov[4, 4]), 0)

  rc <- residuals(fit_rs, per_cluster = TRUE, type = "cor")
  expect_equal(rc[[1]]$type, "cor.bollen")
  expect_equal(unname(diag(rc[[1]]$cov)), rep(0, 4))
  expect_equal(
    residuals(fit_rs, per_cluster = TRUE, type = "srmr")[[1]]$type,
    "cor.bentler"
  )
  # As in lavaan, "cor" follows the fit's mimic option
  expect_equal(rs_residual_type("cor", "EQS"), "cor.bentler")
  expect_equal(rs_residual_type("cor_bollen"), "cor.bollen")
  expect_true(is.na(rs_residual_type("normalized")))
})

test_that("The outputs a random-slope fit cannot give are refused", {
  expect_error(fitted(fit_rs, type = "casewise"), class = "inlavaan_rs_moments")
  expect_error(
    residuals(fit_rs, type = "normalized"),
    class = "inlavaan_rs_moments"
  )
  expect_error(
    residuals(fit_rs, type = "standardized"),
    class = "inlavaan_rs_moments"
  )
  fit_fx <- asem(
    "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1
    level: 2
      fb =~ y1 + y2 + y3
    ",
    d_rs,
    cluster = "cluster",
    verbose = FALSE,
    test = "none",
    nsamp = 3
  )
  expect_error(
    fitted(fit_fx, per_cluster = TRUE),
    class = "inlavaan_per_cluster"
  )
  expect_error(
    residuals(fit_fx, per_cluster = TRUE),
    class = "inlavaan_per_cluster"
  )
  # Only a named argument reaches per_cluster
  expect_no_error(suppressWarnings(resid(fit_fx, "raw", TRUE)))
})

test_that("Standardised estimates of a random-slope fit", {
  set.seed(1)
  std <- standardisedsolution(fit_rs, nsamp = 20)
  expect_true(all(is.finite(std$est.std)))
  carrier <- std$lhs == "fw" & std$op == "~" & std$rhs == "x1"
  expect_gt(std$est.std[carrier], 0)
  # lavaan's own output switches pass through
  expect_no_error(standardisedsolution(fit_rs, nsamp = 3, zstat = TRUE))

  out <- capture.output(summary(fit_rs, standardized = TRUE, nsamp = 5))
  expect_true(any(grepl("Std.all", out, fixed = TRUE)))
})

test_that("summary() takes the R-square from the averaged variances", {
  info <- rs_spec(get_inlavaan_internal(fit_rs))$rs$info
  r2 <- rs_rsquare(fit_rs, info)
  r2_fw <- r2$r2[r2$resvar & r2$lhs == "fw" & r2$block == 1L]
  expect_gt(r2_fw, 0.05)
  out <- capture.output(summary(fit_rs, rsquare = TRUE))
  fw_line <- grep("^\\s+fw\\s+[0-9.]+$", out, value = TRUE)
  expect_equal(
    as.numeric(sub(".*\\s", "", fw_line)),
    round(r2_fw, 3)
  )
})

test_that("Constraints, FIML, two slopes and named levels work together", {
  skip_on_cran()
  # One fit with an equality constraint, two slopes and their covariance,
  # named levels, missing outcomes, and a cluster cut down to a single row
  d_mix <- d_rs[-which(d_rs$cluster == 1)[-1], ]
  set.seed(5)
  for (v in c("y1", "y2", "y3")) {
    d_mix[[v]][stats::runif(nrow(d_mix)) < 0.05] <- NA
  }
  d_mix[d_mix$cluster == 1, "y2"] <- d_rs$y2[d_rs$cluster == 1][1]
  # An outcome never observed in one cluster
  d_mix[d_mix$cluster == 3, "y3"] <- NA
  mod_mix <- "
    level: within
      fw =~ y1 + a*y2 + a*y3
      fw ~ rv('s1')*x1 + rv('s2')*x2
    level: between
      fb =~ y1 + y2 + y3
      fb ~ w1
      s1 ~~ s2
  "
  fit <- asem(
    mod_mix,
    d_mix,
    cluster = "cluster",
    missing = "ml",
    verbose = FALSE,
    test = "none",
    marginal_correction = "none",
    vb_correction = FALSE,
    nsamp = 3
  )
  spec <- rs_spec(get_inlavaan_internal(fit))
  expect_lt(
    max(abs(
      rs_stacked_loglik(fit@Model, spec$rs, fit@Data) -
        rs_kernel_loglik(fit@Model, spec$rs)
    )),
    1e-8
  )
  f <- fitted(fit)
  expect_named(f, c("within", "cluster"))
  expect_true(all(is.finite(f$within$cov)))
  r <- residuals(fit, per_cluster = TRUE)
  one <- r[["1"]]
  expect_true(all(is.na(one$cov[1:3, 1:3])))
  expect_true(all(is.finite(one$mean)))
  expect_true(all(is.finite(r[["2"]]$cov)))
  # A single row has exactly no within-cluster spread
  expect_true(all(fitted(fit, per_cluster = TRUE)[["1"]]$cov == 0))
  expect_no_warning(residuals(fit, per_cluster = TRUE, type = "cor"))
  # A variable never observed in a cluster is missing, not undefined
  y3 <- r[["3"]]$mean[["y3"]]
  expect_true(is.na(y3) && !is.nan(y3))
  set.seed(2)
  std <- standardisedsolution(fit, nsamp = 10)
  expect_true(all(is.finite(std$est.std)))
})
