test_that("Plot method works", {
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  pdf(file = NULL)
  on.exit(dev.off(), add = TRUE)
  expect_silent(plt <- plot(fit))
  expect_silent(plt <- plot(fit, truth = coef(fit)))
  # expect_equal(length(plt@layers), length(coef(fit)))
})

test_that("Plot with use_ggplot = TRUE returns ggplot object", {
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  pdf(file = NULL)
  on.exit(dev.off(), add = TRUE)

  plt <- plot(fit, use_ggplot = TRUE)
  expect_s3_class(plt, "ggplot")

  plt_truth <- plot(fit, truth = coef(fit), use_ggplot = TRUE)
  expect_s3_class(plt_truth, "ggplot")
})

test_that("Plot with use_ggplot = FALSE uses base graphics", {
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none")
  pdf(file = NULL)
  on.exit(dev.off(), add = TRUE)

  res <- plot(fit, use_ggplot = FALSE)
  expect_null(res)

  res_truth <- plot(fit, truth = coef(fit), use_ggplot = FALSE)
  expect_null(res_truth)
})

test_that("the skew-normal fit plots differ between the two scales", {
  skip_if_not_installed("ggplot2")
  dat <- lavaan::HolzingerSwineford1939
  fit <- acfa(
    "visual =~ x1 + x2 + x3",
    dat,
    verbose = FALSE,
    nsamp = 3,
    test = "none"
  )
  pdf(file = NULL)
  on.exit(dev.off(), add = TRUE)

  p_raw <- plot(fit, type = "sn_fit")
  p_log <- plot(fit, type = "sn_fit_log")
  expect_s3_class(p_raw, "ggplot")
  expect_s3_class(p_log, "ggplot")

  # The log panel plots log10 of the density, so the two panels must carry
  # different values, and the log one must reach further down.
  expect_false(identical(p_raw$data$value, p_log$data$value))
  expect_lt(min(p_log$data$value), min(p_raw$data$value))
})
