## ----- Shared fit (complete data) --------------------------------------------
mod_ml <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1 + x2 + x3
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1 + w2
"
fit_ml <- asem(
  mod_ml,
  lavaan::Demo.twolevel,
  cluster = "cluster",
  verbose = FALSE,
  test = "none",
  marginal_correction = "none",
  vb_correction = FALSE,
  nsamp = 3
)

test_that("Multilevel: fit and summary", {
  fit_lav <- lavaan::sem(mod_ml, lavaan::Demo.twolevel, cluster = "cluster")

  expect_s4_class(fit_ml, "INLAvaan")
  expect_no_error(capture.output(summary(fit_ml)))
  expect_equal(coef(fit_ml), coef(fit_lav), tolerance = 0.1)
  expect_equal(fit_ml@optim$dx, rep(0, length(coef(fit_ml))), tolerance = 1e-2)
})

test_that("Multilevel predict lv", {
  nsamp <- 5

  # Level 1
  pred1 <- predict(fit_ml, type = "lv", level = 1L, nsamp = nsamp)
  expect_length(pred1, nsamp)
  m1 <- pred1[[1]]
  expect_equal(nrow(m1), 2500)
  expect_true("fw" %in% colnames(m1))
  expect_true(ncol(m1) >= 1)

  # Level 2
  pred2 <- predict(fit_ml, type = "lv", level = 2L, nsamp = nsamp)
  expect_length(pred2, nsamp)
  m2 <- pred2[[1]]
  expect_equal(nrow(m2), 200)
  expect_true("fb" %in% colnames(m2))
  expect_true(ncol(m2) >= 1)
})

test_that("Multilevel predict yhat and ypred", {
  nsamp <- 5

  # yhat
  pred_yhat <- predict(fit_ml, type = "yhat", nsamp = nsamp)
  expect_length(pred_yhat, nsamp)
  m <- pred_yhat[[1]]
  expect_equal(nrow(m), 2500)
  expect_equal(ncol(m), 8)
  expect_true(all(
    c("y1", "y2", "y3", "x1", "x2", "x3", "w1", "w2") %in% colnames(m)
  ))
  expect_false(any(is.na(m)))

  # ypred
  pred_ypred <- predict(fit_ml, type = "ypred", nsamp = nsamp)
  expect_length(pred_ypred, nsamp)
  m2 <- pred_ypred[[1]]
  expect_equal(nrow(m2), 2500)
  expect_equal(ncol(m2), 8)
  expect_false(any(is.na(m2)))
})

test_that("Multilevel predict errors for unsupported options", {
  expect_error(
    predict(fit_ml, type = "lv", newdata = lavaan::Demo.twolevel, nsamp = 3),
    "not supported for multilevel"
  )
})

## ----- Missing data (FIML) fit -----------------------------------------------
test_that("Multilevel predict ymis works", {
  dat <- lavaan::Demo.twolevel
  dat <- dat[dat$cluster <= 10, ]
  set.seed(123)
  dat$y1[sample(nrow(dat), 10)] <- NA

  fit_miss <- asem(
    mod_ml,
    dat,
    cluster = "cluster",
    missing = "ML",
    test = "none",
    marginal_correction = "none",
    vb_correction = FALSE,
    verbose = FALSE,
    nsamp = 3
  )

  nsamp <- 5

  # Full imputed dataset (default)
  pred <- predict(fit_miss, type = "ymis", nsamp = nsamp)
  expect_length(pred, nsamp)
  expect_false(any(is.na(pred[[1]])))
  expect_equal(ncol(pred[[1]]), 8L)  # 8 model variables (y1-y3, x1-x3, w1-w2)

  # ymis_only: named vector of just the imputed cells
  pred_only <- predict(fit_miss, type = "ymis", nsamp = nsamp, ymis_only = TRUE)
  expect_length(pred_only, nsamp)
  v <- pred_only[[1]]
  expect_true(is.numeric(v))
  expect_length(v, 10L)  # exactly the 10 NAs we injected
  expect_true(all(grepl("^y1\\[", names(v))))  # all from y1
})
