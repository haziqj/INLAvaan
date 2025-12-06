dat <- lavaan::HolzingerSwineford1939
mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
NSAMP <- 3

test_that("Predict method works", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP)
  prd <- predict(fit, nsamp = NSAMP)
  prd_summ <- summary(prd)
  expect_no_error(out <- capture.output(print(prd)))
  expect_no_error(out <- capture.output(print(prd_summ)))
  expect_equal(length(prd), NSAMP)
  expect_equal(nrow(prd_summ$Mean), nrow(dat))
})

test_that("Multigroup predict method works", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP, group = "school")
  prd <- predict(fit, nsamp = NSAMP)
  prd_summ <- summary(prd)
  expect_no_error(out <- capture.output(print(prd)))
  expect_no_error(out <- capture.output(print(prd_summ)))
  expect_equal(length(prd), NSAMP)
  expect_equal(nrow(prd_summ$Mean), nrow(dat))
})
