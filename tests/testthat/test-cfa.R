library(lavaan)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9'

# Maximum likelihood
fit_lav  <- cfa(HS.model, HolzingerSwineford1939, std.lv = TRUE)
fit_inlv <- inlavaan(HS.model, HolzingerSwineford1939, "cfa", std.lv = TRUE,
                     add_priors = FALSE)

test_that("Correct ML estimates", {
  lav_coefs <- coef(fit_lav)
  expect_equal(as.numeric(lav_coefs), as.numeric(fit_inlv$theta_star_trans),
               tolerance = 1e-4)
})

test_that("CFA works", {
  fit <- inlavaan(HS.model, HolzingerSwineford1939, "cfa", std.lv = TRUE)
  expect_s3_class(fit, "inlavaan_internal")
})
