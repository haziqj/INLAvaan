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
