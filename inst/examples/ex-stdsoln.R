HS.model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"
utils::data("HolzingerSwineford1939", package = "lavaan")

# Fit a CFA model with standardised latent variables
fit <- acfa(
  HS.model,
  data = HolzingerSwineford1939,
  test = "none",
  nsamp = 10,
  vb_correction = "none",
  silent = TRUE
)
standardisedsolution(fit, nsamp = 10, se = FALSE, ci = FALSE)
