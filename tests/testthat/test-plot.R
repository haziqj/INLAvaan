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
