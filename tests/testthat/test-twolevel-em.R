# The saturated two-level EM of R/twolevel-em.R gives lavaan's results.

test_that("the saturated EM equals lavaan's, complete and missing data", {
  lav_ns <- asNamespace("lavaan")
  dat <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster %in% 1:40, ]
  dat_na <- dat
  set.seed(29)
  for (v in c("y1", "y2", "y3")) {
    dat_na[[v]][stats::runif(nrow(dat)) < 0.1] <- NA
  }
  dat_na$w2[dat_na$cluster %in% c(3, 8)] <- NA
  model <- "
    level: 1
      fw =~ y1 + y2 + y3
    level: 2
      fb =~ y1 + y2 + y3
      w2 ~ fb + w1
  "
  fit <- suppressWarnings(lavaan::sem(
    model,
    data = dat,
    cluster = "cluster",
    se = "none"
  ))
  fit_na <- suppressWarnings(lavaan::sem(
    model,
    data = dat_na,
    cluster = "cluster",
    missing = "ml",
    se = "none"
  ))
  ylp <- fit@SampleStats@YLp[[1L]]
  lp <- fit@Data@Lp[[1L]]
  x_na <- fit_na@Data@X[[1L]]
  lp_na <- fit_na@Data@Lp[[1L]]
  y2_na <- rowsum.default(
    x_na,
    group = lp_na$cluster.idx[[2L]],
    reorder = FALSE
  ) /
    lp_na$cluster.size[[2L]]
  for (acc in c("none", "squarem")) {
    expect_equal(
      suppressWarnings(twolevel_em_sat(
        ylp = ylp,
        lp = lp,
        tol = 1e-4,
        max_iter = 5000L,
        acceleration = acc
      )),
      suppressWarnings(lav_ns$lav_mvn_cl_em_sat(
        ylp = ylp,
        lp = lp,
        tol = 1e-4,
        max_iter = 5000L,
        acceleration = acc
      )),
      tolerance = 1e-10
    )
    expect_equal(
      suppressWarnings(twolevel_mi_em_sat(
        y1 = x_na,
        y2 = y2_na,
        lp = lp_na,
        mp = fit_na@Data@Mp[[1L]],
        loglik_x = 0,
        tol = 1e-4,
        max_iter = 5000L,
        acceleration = acc
      )),
      suppressWarnings(lav_ns$lav_mvn_cl_mi_em_sat(
        y1 = x_na,
        y2 = y2_na,
        lp = lp_na,
        mp = fit_na@Data@Mp[[1L]],
        loglik_x = 0,
        tol = 1e-4,
        max_iter = 5000L,
        acceleration = acc
      )),
      tolerance = 1e-10
    )
  }
})
