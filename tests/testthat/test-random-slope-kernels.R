# The random-slope kernels of R/random-slope-kernels.R: the log-likelihood and
# its gradient against lavaan's random-slope code and a numerical gradient.

rsk_data <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster %in% 1:30, ]

rsk_models <- list(
  # A random intercept and a random slope of an observed outcome
  lmm = "
    level: 1
      y1 ~ rv('s1')*x1
    level: 2
      y1 ~~ y1
      s1 ~~ s1
      s1 ~~ y1
  ",
  # A random slope of a within factor, regressed on a cluster covariate
  factor = "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1
    level: 2
      fb =~ y1 + y2 + y3
      fb ~ w1
      s1 ~ w1
  ",
  # Equal loadings at level 1, and a cluster covariate on both random effects
  equal_loadings = "
    level: 1
      fw =~ y1 + a*y2 + a*y3
      fw ~ rv('s1')*x1
    level: 2
      fb =~ y1 + y2 + y3
      fb + s1 ~ w1
      fb ~~ s1
  "
)

rsk_fits <- lapply(rsk_models, function(m) {
  suppressWarnings(lavaan::sem(
    m,
    data = rsk_data,
    cluster = "cluster",
    ceq.simple = TRUE,
    se = "none"
  ))
})

rsk_loglik <- function(fit, x, multilevel = NULL) {
  inlav_model_loglik(
    x,
    fit@Model,
    fit@SampleStats,
    fit@Data,
    fit@Options,
    fit@Cache,
    multilevel = multilevel
  )
}

rsk_grad <- function(fit, x, multilevel = NULL) {
  inlav_model_grad(
    x,
    fit@Model,
    fit@SampleStats,
    fit@Data,
    fit@Cache,
    multilevel = multilevel
  )
}

# A point away from the estimates, in lavaan's packed parameters (one per
# equality group), which is what inlav_model_loglik() takes. The estimates of some level-2 variances are at or below
# zero on this small subset, so every variance is moved to at least 0.1.
rsk_point <- function(fit, seed) {
  set.seed(seed)
  x <- lavaan::lav_model_get_parameters(fit@Model)
  x <- x + 0.05 * stats::rnorm(length(x))
  pt <- lavaan::parTable(fit)
  var_idx <- unique(pt$free[pt$op == "~~" & pt$lhs == pt$rhs & pt$free > 0])
  x[var_idx] <- pmax(abs(x[var_idx]), 0.1)
  x
}
# The matrix that unpacks the parameters: lavaan leaves it empty when the
# model has no equality constraints
rsk_k <- function(fit) {
  K <- fit@Model@ceq.simple.K
  if (length(K) == 0L) diag(fit@Model@nx.free) else K
}

test_that("the kernels take route A, and not the quadrature route", {
  for (fit in rsk_fits) {
    expect_true(rs_kernel_ok(fit@Model, fit@Data, fit@Cache))
  }
  split <- suppressWarnings(lavaan::sem(
    "
    level: 1
      y1 ~ rv('s1')*x1
    level: 2
      y1 ~ x1
      s1 ~~ s1
    ",
    data = rsk_data,
    cluster = "cluster"
  ))
  expect_false(rs_kernel_ok(split@Model, split@Data, split@Cache))
})

test_that("the log-likelihood equals lavaan's", {
  for (fit in rsk_fits) {
    x <- lavaan::lav_model_get_parameters(fit@Model)
    expect_equal(rsk_loglik(fit, x), as.numeric(lavaan::logLik(fit)))
    for (seed in 1:3) {
      x <- rsk_point(fit, seed)
      expect_equal(
        rsk_loglik(fit, x),
        rsk_loglik(
          fit,
          x,
          multilevel = list(rs = NULL, twolevel = FALSE, twolevel_fiml = NULL)
        ),
        tolerance = 1e-12
      )
    }
  }
})

test_that("the gradient equals lavaan's and a numerical gradient", {
  skip_if_not_installed("numDeriv")
  for (fit in rsk_fits) {
    for (seed in 1:2) {
      x <- rsk_point(fit, seed)
      g <- rsk_grad(fit, x)
      expect_length(g, fit@Model@nx.unco)
      expect_equal(
        g,
        rsk_grad(
          fit,
          x,
          multilevel = list(rs = NULL, twolevel = FALSE, twolevel_fiml = NULL)
        ),
        tolerance = 1e-10
      )
      # The gradient comes back with one entry per free row, the packed
      # derivative on the first row of each equality group, so its sum over
      # each group is the derivative in the packed parameters.
      g_num <- numDeriv::grad(function(z) rsk_loglik(fit, z), x)
      expect_equal(as.numeric(g %*% rsk_k(fit)), g_num, tolerance = 1e-6)
    }
  }
})

test_that("the kernels return -Inf, or NULL to fall back to lavaan", {
  fit <- rsk_fits$lmm
  plan <- multilevel_setup(
    fit@Model,
    fit@SampleStats,
    fit@Data,
    fit@Cache
  )$rs
  imp <- rs_implied(fit@Model, plan$info)
  bad_w <- imp
  bad_w$sigma_w[] <- -1
  expect_identical(rs_kernel_loglik(plan, bad_w), -Inf)
  expect_null(rs_kernel_dphi(plan, bad_w))
  bad_v <- imp
  bad_v$sigma.v <- -diag(nrow(imp$sigma.v))
  expect_identical(rs_kernel_loglik(plan, bad_v), -Inf)
  expect_null(rs_kernel_dphi(plan, bad_v))
  expect_identical(rs_kernel_loglik(plan, NULL), -Inf)
})

test_that("an indefinite level-2 covariance is scored as lavaan scores it", {
  # A covariance of the random intercept and slope beyond their SDs makes
  # Sigma_v indefinite, while every Z_j keeps positive eigenvalues
  for (fit in rsk_fits[c("lmm", "factor")]) {
    plan <- multilevel_setup(
      fit@Model,
      fit@SampleStats,
      fit@Data,
      fit@Cache
    )$rs
    imp <- rs_implied(fit@Model, plan$info)
    sv <- diag(c(0.05, 0.3, rep(0.5, imp$pv - 2L)), imp$pv)
    sv[1, 2] <- sv[2, 1] <- 0.2
    imp$sigma.v <- sv
    expect_lt(min(eigen(sv, symmetric = TRUE)$values), 0)
    m2ll <- lavaan:::lav_mvn_cl_rs_loglik(
      rs_stats = plan_stats <- fit@Cache[[1L]]$rs$stats,
      imp = imp,
      rs_info = plan$info
    )
    expect_true(is.finite(m2ll))
    expect_equal(
      rs_kernel_loglik(plan, imp),
      -m2ll / 2,
      tolerance = 1e-12
    )
    expect_equal(
      rs_dphi_vec(rs_kernel_dphi(plan, imp)),
      colSums(lavaan:::lav_mvn_cl_rs_dfphi(
        rs_stats = plan_stats,
        imp = imp,
        rs_info = plan$info
      )),
      tolerance = 1e-10
    )
  }
})

## ----- Missing data -------------------------------------------------------------

rsk_data_na <- local({
  dat <- rsk_data
  set.seed(28)
  for (v in c("y1", "y2", "y3")) {
    dat[[v]][stats::runif(nrow(dat)) < 0.12] <- NA
  }
  dat
})

rsk_fits_na <- lapply(
  rsk_models[c("lmm", "factor", "equal_loadings")],
  function(m) {
    suppressWarnings(lavaan::sem(
      m,
      data = rsk_data_na,
      cluster = "cluster",
      missing = "ml",
      ceq.simple = TRUE,
      se = "none"
    ))
  }
)

test_that("the kernels take random slopes with missing data", {
  for (fit in rsk_fits_na) {
    expect_false(is.null(fit@Cache[[1L]]$rs$stats$mp))
    expect_true(rs_kernel_ok(fit@Model, fit@Data, fit@Cache))
  }
})

test_that("with missing data the log-likelihood equals lavaan's", {
  for (fit in rsk_fits_na) {
    x <- lavaan::lav_model_get_parameters(fit@Model)
    expect_equal(rsk_loglik(fit, x), as.numeric(lavaan::logLik(fit)))
    for (seed in 1:3) {
      x <- rsk_point(fit, seed)
      expect_equal(
        rsk_loglik(fit, x),
        rsk_loglik(
          fit,
          x,
          multilevel = list(rs = NULL, twolevel = FALSE, twolevel_fiml = NULL)
        ),
        tolerance = 1e-12
      )
    }
  }
})

test_that("with missing data the gradient equals lavaan's and a numerical one", {
  skip_if_not_installed("numDeriv")
  for (fit in rsk_fits_na) {
    for (seed in 1:2) {
      x <- rsk_point(fit, seed)
      g <- rsk_grad(fit, x)
      expect_equal(
        g,
        rsk_grad(
          fit,
          x,
          multilevel = list(rs = NULL, twolevel = FALSE, twolevel_fiml = NULL)
        ),
        tolerance = 1e-10
      )
      g_num <- numDeriv::grad(function(z) rsk_loglik(fit, z), x)
      expect_equal(as.numeric(g %*% rsk_k(fit)), g_num, tolerance = 1e-6)
    }
  }
})

test_that("a missing-data pattern with one cell is scored as lavaan scores it", {
  dat <- rsk_data_na
  # A pattern that only one case has
  same <- is.na(dat$y1) & is.na(dat$y2) & !is.na(dat$y3)
  dat$y1[same] <- 0.1
  dat[2L, c("y1", "y2", "y3")] <- c(NA, NA, 0.5)
  fit <- suppressWarnings(lavaan::sem(
    rsk_models$factor,
    data = dat,
    cluster = "cluster",
    missing = "ml",
    se = "none"
  ))
  n_cells <- vapply(
    fit@Cache[[1L]]$rs$stats$mp$pattern,
    function(cell) length(cell$j1),
    integer(1)
  )
  expect_true(any(n_cells == 1L))
  x <- rsk_point(fit, 1)
  lavaan_route <- list(rs = NULL, twolevel = FALSE, twolevel_fiml = NULL)
  expect_equal(
    rsk_loglik(fit, x),
    rsk_loglik(fit, x, multilevel = lavaan_route),
    tolerance = 1e-12
  )
  expect_equal(
    rsk_grad(fit, x),
    rsk_grad(fit, x, multilevel = lavaan_route),
    tolerance = 1e-10
  )
})

test_that("the implied pieces equal lavaan's with a between-only response", {
  fit <- suppressWarnings(lavaan::sem(
    "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1
    level: 2
      fb =~ y1 + y2 + y3 + w2
      fb ~ w1
    ",
    data = rsk_data,
    cluster = "cluster",
    se = "none"
  ))
  info <- fit@Cache[[1L]]$rs$info
  expect_gt(info$kz, 0L)
  expect_identical(
    rs_implied(fit@Model, info),
    asNamespace("lavaan")$lav_mvn_cl_rs_implied(
      lavmodel = fit@Model,
      rs_info = info
    )
  )
})

test_that("the implied pieces equal lavaan's and their chain rule is exact", {
  lav_ns <- asNamespace("lavaan")
  for (fit in c(rsk_fits, rsk_fits_na["factor"])) {
    info <- fit@Cache[[1L]]$rs$info
    x <- rsk_point(fit, 3)
    lm <- lavaan::lav_model_set_parameters(fit@Model, x)
    imp <- rs_implied(lm, info)
    expect_identical(
      imp,
      lav_ns$lav_mvn_cl_rs_implied(lavmodel = lm, rs_info = info)
    )
    # A linear function of the pieces, F = sum(w * phi), has dF/dphi = w
    set.seed(4)
    w <- stats::rnorm(length(rs_dphi_vec(imp)))
    shapes <- list(
      sigma_w = dim(imp$sigma_w),
      mu_y = NULL,
      P = dim(imp$P),
      lmat = dim(imp$lmat),
      q0 = dim(imp$q0),
      sigma.v = dim(imp$sigma.v),
      mu.v = NULL,
      cc = dim(imp$cc),
      mu.exo = NULL
    )
    sizes <- vapply(
      names(shapes),
      function(nm) length(as.numeric(imp[[nm]])),
      numeric(1)
    )
    off <- cumsum(c(0, sizes))
    g <- lapply(seq_along(shapes), function(k) {
      v <- w[off[k] + seq_len(sizes[k])]
      if (is.null(shapes[[k]])) {
        v
      } else {
        matrix(v, shapes[[k]][1L], shapes[[k]][2L])
      }
    })
    names(g) <- names(shapes)
    an <- rs_implied_back(lm, rs_implied_steps(lm, info), g)
    num <- numDeriv::grad(
      function(z) {
        sum(
          w *
            rs_dphi_vec(rs_implied(
              lavaan::lav_model_set_parameters(fit@Model, z),
              info
            ))
        )
      },
      x
    )
    expect_equal(an, num, tolerance = 1e-7)
  }
})

## ----- Between-only responses -----------------------------------------------------

test_that("between-only responses are scored as lavaan scores them", {
  skip_if_not_installed("numDeriv")
  model <- "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ rv('s1')*x1
    level: 2
      fb =~ y1 + y2 + y3 + w2
      fb ~ w1
  "
  lavaan_route <- list(rs = NULL, twolevel = FALSE, twolevel_fiml = NULL)
  for (dat in list(rsk_data, rsk_data_na)) {
    fit <- suppressWarnings(lavaan::sem(
      model,
      data = dat,
      cluster = "cluster",
      missing = if (anyNA(dat$y1)) "ml" else "listwise",
      se = "none"
    ))
    expect_gt(fit@Cache[[1L]]$rs$info$kz, 0L)
    expect_true(rs_kernel_ok(fit@Model, fit@Data, fit@Cache))
    x <- lavaan::lav_model_get_parameters(fit@Model)
    expect_equal(rsk_loglik(fit, x), as.numeric(lavaan::logLik(fit)))
    x <- rsk_point(fit, 2)
    expect_equal(
      rsk_loglik(fit, x),
      rsk_loglik(fit, x, multilevel = lavaan_route),
      tolerance = 1e-12
    )
    g <- rsk_grad(fit, x)
    expect_equal(
      g,
      rsk_grad(fit, x, multilevel = lavaan_route),
      tolerance = 1e-8
    )
    g_num <- numDeriv::grad(function(z) rsk_loglik(fit, z), x)
    expect_equal(as.numeric(g %*% rsk_k(fit)), g_num, tolerance = 1e-6)
  }
})
