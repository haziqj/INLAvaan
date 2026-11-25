test_that("Results do not depend on the order of the latent variables", {
  dat <- lavaan::HolzingerSwineford1939
  mod1 <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "
  mod2 <- "
    speed   =~ x7 + x8 + x9
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
  # lavaan names a covariance by the order of its two latent variables
  rename <- c(
    "speed~~visual" = "visual~~speed",
    "speed~~textual" = "textual~~speed"
  )

  for (scan in c("linear", "plain")) {
    fits <- lapply(list(mod1, mod2), function(mod) {
      acfa(
        mod,
        dat,
        std.lv = TRUE,
        marginal_scan = scan,
        test = "none",
        nsamp = 3,
        silent = TRUE
      )
    })
    coefs <- lapply(fits, function(fit) {
      x <- unclass(coef(fit))
      hit <- names(x) %in% names(rename)
      names(x)[hit] <- rename[names(x)[hit]]
      x[order(names(x))]
    })
    expect_equal(coefs[[1]], coefs[[2]], tolerance = 1e-4, label = scan)
    mloglik <- vapply(
      fits,
      function(fit) get_inlavaan_internal(fit)$mloglik,
      numeric(1)
    )
    expect_equal(mloglik[1], mloglik[2], tolerance = 1e-6, label = scan)
  }
})
