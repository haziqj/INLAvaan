library(qrng)
set.seed(1011)
SobolOwen <- qrng::sobol(n = 100, d = 300, randomize = "Owen")
save(SobolOwen, file = "R/sysdata.rda", compress = "xz")
