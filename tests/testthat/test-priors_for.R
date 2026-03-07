test_that("priors_for returns named character vector with all defaults", {
  res <- priors_for()
  expect_type(res, "character")
  expect_named(res)
  expect_true(all(c("nu", "alpha", "lambda", "beta", "theta", "psi", "rho",
                     "tau") %in% names(res)))
})

test_that("priors_for default values are correct", {
  res <- priors_for()
  expect_equal(res[["nu"]],     "normal(0,32)")
  expect_equal(res[["lambda"]], "normal(0,10)")
  expect_equal(res[["theta"]],  "gamma(1,.5)[sd]")
  expect_equal(res[["rho"]],    "beta(1,1)")
})

test_that("priors_for overrides specific defaults", {
  res <- priors_for(lambda = "normal(0,1)", rho = "beta(3,3)")
  expect_equal(res[["lambda"]], "normal(0,1)")
  expect_equal(res[["rho"]],    "beta(3,3)")
  # Non-overridden defaults should remain

  expect_equal(res[["nu"]],     "normal(0,32)")
})

test_that("priors_for allows adding new (non-default) names", {
  res <- priors_for(custom = "normal(0,5)")
  expect_equal(res[["custom"]], "normal(0,5)")
})

test_that("priors_for errors on unnamed arguments", {
  expect_error(priors_for("normal(0,1)"), "named")
})
