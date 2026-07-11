dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"
fit <- acfa(mod, dat, verbose = FALSE, nsamp = 3, test = "none", 
            marginal_method = "marggaus", vb_correction = FALSE)

test_that("anova() on a single INLAvaan fit redirects to compare()", {
  expect_error(anova(fit), "compare")
})

test_that("anova() on two INLAvaan fits redirects to compare()", {
  expect_error(anova(fit, fit), "compare")
})

test_that("plain lavaan fits are unaffected by the INLAvaan anova() override", {
  fit_lav <- lavaan::cfa(mod, dat)
  expect_no_error(anova(fit_lav))
})
