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

fit1 <- acfa(mod1, dat, std.lv = TRUE)
fit2 <- acfa(mod2, dat, std.lv = TRUE)

coef1 <- coef(fit1)
class(coef1) <- "numeric"
parnames <- names(coef1)
parnames[parnames == "visual~~speed"] <- "speed~~visual"
parnames[parnames == "textual~~speed"] <- "speed~~textual"
coef2 <- coef(fit2)[parnames]

tinytest::expect_equal(coef1, coef2, tolerance = 0.001)


which((coef1 - coef2) > 1e-3)
