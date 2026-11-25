# A finer scan of each skew-normal marginal
inlavaan_control(n_grid = 31)

# Print the iterations of the search for the posterior mode
inlavaan_control(nlminb = list(trace = 1))

# An iter.max in nlminb stands in for mode_maxit
inlavaan_control(nlminb = list(iter.max = 200))

\donttest{
utils::data("HolzingerSwineford1939", package = "lavaan")
fit <- acfa(
  "visual =~ x1 + x2 + x3",
  HolzingerSwineford1939,
  test = "none",
  control = inlavaan_control(n_grid = 31)
)

# The same with a plain list
fit <- acfa(
  "visual =~ x1 + x2 + x3",
  HolzingerSwineford1939,
  test = "none",
  control = list(n_grid = 31)
)
}
