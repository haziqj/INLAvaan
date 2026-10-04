# PML fits are slow
skip_on_cran()

dat <- lavaan::HolzingerSwineford1939
xs <- c("x1", "x2", "x3")
dat_int <- dat
for (v in xs) {
  dat_int[[v]] <- as.integer(cut(dat[[v]], 3))
}
dat_fac <- dat_int
for (v in xs) {
  dat_fac[[v]] <- ordered(dat_int[[v]])
}
mod <- "visual =~ x1 + x2 + x3"
# The fit diagnostics flag this coarse one-factor model, and lavaan notes
# ordered = names it cannot find, neither of which matters here
fit_ord <- function(data, ...) {
  set.seed(1)
  suppressWarnings(
    acfa(mod, data, verbose = FALSE, nsamp = 3, test = "none", ...)
  )
}

test_that("Ordered-factor columns are fitted as ordinal data", {
  fit_fac <- fit_ord(dat_fac)
  fit_int <- fit_ord(dat_int, ordered = TRUE)
  expect_equal(fit_fac@Model@estimator, "PML")
  expect_equal(coef(fit_fac), coef(fit_int))
})

test_that("ordered = names only the ordinal variables in the model", {
  expect_equal(
    coef(fit_ord(dat_int, ordered = names(dat_int))),
    coef(fit_ord(dat_int, ordered = xs))
  )
  expect_error(fit_ord(dat, ordered = "zz"), "estimator")
})
