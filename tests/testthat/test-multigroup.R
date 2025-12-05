dat <- lavaan::HolzingerSwineford1939
mod <- "
    visual  =~ x1 + x2 # + x3
    textual =~ x4 + x5 # + x6
    # speed   =~ x7 + x8 + x9
  "
NSAMP <- 3
STDLV <- TRUE

test_that("Multigroup fitting and testing", {
  # Configural invariance
  expect_no_error({
    fit1 <- acfa(
      mod,
      dat,
      verbose = FALSE,
      test = FALSE,
      std.lv = STDLV,
      group = "school"
    )
  })
  expect_no_error(out <- capture.output(summary(fit1)))
  expect_equal(fit1@optim$dx, rep(0, length(fit1@optim$dx)), tolerance = 1e-3)

  # Weak invariance
  expect_no_error({
    fit2 <- acfa(
      mod,
      dat,
      verbose = FALSE,
      test = FALSE,
      std.lv = STDLV,
      group = "school",
      group.equal = "loadings"
    )
  })
  expect_no_error(out <- capture.output(summary(fit2)))
  expect_equal(fit2@optim$dx, rep(0, length(fit2@optim$dx)), tolerance = 1e-3)

  # Weak invariance
  expect_no_error({
    fit3 <- acfa(
      mod,
      dat,
      verbose = FALSE,
      test = FALSE,
      std.lv = STDLV,
      group = "school",
      group.equal = c("intercepts", "loadings")
    )
  })
  expect_no_error(out <- capture.output(summary(fit3)))
  expect_equal(fit3@optim$dx, rep(0, length(fit3@optim$dx)), tolerance = 1e-3)

  # Comparison
  expect_no_error({
    compare(fit1, fit2, fit3)
  })
})

test_that("Gradients are correct (Finite Difference Check)", {
  suppressMessages(
    tmp <- capture.output(fit <- acfa(mod, dat, test = NA, debug = TRUE))
  )
  test_df <- read.table(text = tmp, skip = 1)[, -1]
  colnames(test_df) <- c("fd", "analytic", "diff")

  expect_equal(
    as.numeric(test_df$fd),
    as.numeric(test_df$diff),
    tolerance = 1e-3
  )
  expect_equal(
    as.numeric(test_df$diff),
    rep(0, nrow(test_df)),
    tolerance = 1e-3
  )
})
