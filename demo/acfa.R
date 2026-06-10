# The famous Holzinger and Swineford (1939) example
hs_mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
fit <- acfa(hs_mod, data = lavaan::HolzingerSwineford1939)
