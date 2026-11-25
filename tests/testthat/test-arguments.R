# The arguments of inlavaan() and its wrappers, and how update() refits

hs <- lavaan::HolzingerSwineford1939
mod <- "visual =~ x1 + x2 + x3"
fit_quick <- function(..., silent = TRUE) {
  acfa(
    mod,
    ...,
    silent = silent,
    nsamp = 3,
    test = "none",
    marginal_method = "gaussian"
  )
}

## ----- data ------------------------------------------------------------------

test_that("a model fits from sample.cov and sample.nobs alone", {
  for (fn in list(inlavaan, acfa, asem, agrowth)) {
    expect_null(formals(fn)$data)
  }
  S <- stats::cov(hs[, c("x1", "x2", "x3")])
  fit <- fit_quick(sample.cov = S, sample.nobs = 301)
  expect_s4_class(fit, "INLAvaan")
  # lavaan rescales sample.cov to the divisor n of the data
  expect_equal(
    get_inlavaan_internal(fit, "theta_star_novbc"),
    get_inlavaan_internal(fit_quick(hs), "theta_star_novbc"),
    tolerance = 1e-6
  )
})
