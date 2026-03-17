## Validation: Permutation invariance of marginal posteriors
## Verifies that reordering latent variables in the model syntax produces
## identical parameter estimates (up to numerical tolerance).

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

## --- shortcut (Q2, Schur correction) ----------------------------------------
fit1 <- acfa(mod1, dat, std.lv = TRUE)
fit2 <- acfa(mod2, dat, std.lv = TRUE)

coef1 <- coef(fit1)
class(coef1) <- "numeric"
parnames <- names(coef1)

# lavaan may flip the order of covariance names
parnames[parnames == "visual~~speed"]   <- "speed~~visual"
parnames[parnames == "textual~~speed"]  <- "speed~~textual"
coef2 <- coef(fit2)[parnames]
class(coef2) <- "numeric"

cat("=== shortcut (Q2) ===\n")
cat(sprintf("Max |diff|: %.2e\n", max(abs(coef1 - coef2))))
tinytest::expect_equal(coef1, coef2, tolerance = 1e-4)

## --- shortcut_fd (Q2, forward difference) ------------------------------------
fit3 <- acfa(mod1, dat, std.lv = TRUE, marginal_correction = "shortcut_fd")
fit4 <- acfa(mod2, dat, std.lv = TRUE, marginal_correction = "shortcut_fd")

coef3 <- coef(fit3)
class(coef3) <- "numeric"
coef4 <- coef(fit4)[parnames]
class(coef4) <- "numeric"

cat("\n=== shortcut_fd (Q2, forward diff) ===\n")
cat(sprintf("Max |diff|: %.2e\n", max(abs(coef3 - coef4))))
tinytest::expect_equal(coef3, coef4, tolerance = 1e-4)

## --- Compare shortcut vs shortcut_fd -----------------------------------------
cat("\n=== shortcut vs shortcut_fd (same model) ===\n")
cat(sprintf("Max |diff|: %.2e\n", max(abs(coef1 - coef3))))
