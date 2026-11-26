# The two-level moment maps of R/twolevel-moments.R give lavaan's results.

test_that("the moment maps equal lavaan's, with between- and within-only data", {
  dat <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster %in% 1:40, ]
  fit <- suppressWarnings(lavaan::sem(
    "
    level: 1
      fw =~ y1 + y2 + y3
      fw ~ x1
    level: 2
      fb =~ y1 + y2 + y3
      w2 ~ fb + w1
    ",
    data = dat,
    cluster = "cluster",
    se = "none"
  ))
  lp <- fit@Data@Lp[[1L]]
  expect_gt(length(lp$between.idx[[2L]]), 0L)
  expect_gt(length(lp$within.idx[[2L]]), 0L)
  imp <- lavaan::lav_model_implied(fit@Model)
  lav_ns <- asNamespace("lavaan")
  expect_identical(
    twolevel_implied22l(
      lp,
      imp$mean[[1L]],
      imp$mean[[2L]],
      imp$cov[[1L]],
      imp$cov[[2L]]
    ),
    lav_ns$lav_mvn_cl_implied22l(
      lp = lp,
      mu_w = imp$mean[[1L]],
      mu_b = imp$mean[[2L]],
      sigma_w = imp$cov[[1L]],
      sigma_b = imp$cov[[2L]]
    )
  )
  out <- twolevel_implied22l(
    lp,
    imp$mean[[1L]],
    imp$mean[[2L]],
    imp$cov[[1L]],
    imp$cov[[2L]]
  )
  set.seed(3)
  fill <- function(m) {
    m[] <- stats::rnorm(length(m))
    m
  }
  args <- list(
    sigma_w = fill(out$sigma_w),
    sigma_b = fill(out$sigma_b),
    sigma_yz = fill(out$sigma_yz),
    sigma_zz = fill(out$sigma_zz),
    mu_y = stats::rnorm(length(out$mu_y)),
    mu_z = stats::rnorm(length(out$mu_z))
  )
  expect_identical(
    do.call(twolevel_2l2implied, c(list(lp = lp), args)),
    do.call(lav_ns$lav_mvn_cl_2l2implied, c(list(lp = lp), args))
  )
})
