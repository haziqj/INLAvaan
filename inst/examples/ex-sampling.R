utils::data("HolzingerSwineford1939", package = "lavaan")
fit <- acfa("visual =~ x1 + x2 + x3", HolzingerSwineford1939)

# Posterior samples of lavaan-side parameters
samps <- sampling(fit, nsamp = 500)
head(samps)

# Compare copula vs Gaussian sampling
s_cop <- sampling(fit, nsamp = 500, samp_copula = TRUE)
s_gaus <- sampling(fit, nsamp = 500, samp_copula = FALSE)

# Prior predictive samples
y_prior <- sampling(fit, type = "observed", nsamp = 500, prior = TRUE)
