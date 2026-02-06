library(qrng)
set.seed(1011)
SobolOwen <- qrng::sobol(n = 100, d = 800, randomize = "Owen")
usethis::use_data(SobolOwen, overwrite = TRUE)
