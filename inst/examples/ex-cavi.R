library(INLAvaan)
# pak::pak("blhansen/VI-MSFA")
library(VIMSFA)

## ----- Bayesian Factor Analysis via CAVI and SVI -----------------------------

# Simulate data
set.seed(123)
P <- 100
J <- 4
N <- 250

if (P %% J != 0) {
  stop("P must be divisible by J for equal-sized factor blocks.")
}

items_per_factor <- P / J
Lambda <- matrix(0, nrow = P, ncol = J)
block_indices <- split(seq_len(P), rep(seq_len(J), each = items_per_factor))
for (j in seq_len(J)) {
  idx <- block_indices[[j]]
  Lambda[idx, j] <- rnorm(items_per_factor)
}
Psi <- runif(P, 0.1, 1)
Sigma <- tcrossprod(Lambda) + diag(Psi)

X <- MASS::mvrnorm(N, mu = rep(0, P), Sigma = Sigma)
X_df <- data.frame(X)

extract_peak_ram <- function(peak_obj) {
  ram_col <- grep(
    "^Peak_RAM_Used_MiB$|Peak_RAM_Used",
    names(peak_obj),
    value = TRUE
  )
  if (length(ram_col) == 0) {
    stop("Could not find peak RAM column in peakRAM output.")
  }
  as.numeric(peak_obj[[ram_col[1]]][1])
}

extract_elapsed_time <- function(peak_obj) {
  time_col <- grep(
    "^Elapsed_Time_sec$|Elapsed_Time",
    names(peak_obj),
    value = TRUE
  )
  if (length(time_col) == 0) {
    stop("Could not find elapsed time column in peakRAM output.")
  }
  as.numeric(peak_obj[[time_col[1]]][1])
}

# Bayesian FA via CAVI
cavi_peak <- peakRAM::peakRAM({
  cavi_est <- cavi_fa(X, J, scale = FALSE)
})
cavi_ram_mib <- extract_peak_ram(cavi_peak)
cavi_time_sec <- extract_elapsed_time(cavi_peak)

cavi_lambda <- cavi_est$mean_lambda
cavi_psi <- cavi_est$mean_psi
cavi_sigma <- tcrossprod(cavi_lambda) + diag(cavi_psi)

rv_cavi <- MatrixCorrelation::RV(cavi_sigma, Sigma)

# Bayesian FA via SVI
svi_peak <- peakRAM::peakRAM({
  svi_est <- svi_fa(X, J, scale = FALSE)
})
svi_ram_mib <- extract_peak_ram(svi_peak)
svi_time_sec <- extract_elapsed_time(svi_peak)
svi_lambda <- svi_est$mean_lambda
svi_psi <- svi_est$mean_psi
svi_sigma <- tcrossprod(svi_lambda) + diag(svi_psi)

rv_svi <- MatrixCorrelation::RV(svi_sigma, Sigma)

# Bayesian FA via INLAvaan
factors <- paste0("f", 1:J)
items <- colnames(X_df)
item_blocks <- lapply(block_indices, function(idx) items[idx])
loading_parts <- sapply(seq_along(factors), function(j) {
  paste0(factors[j], " =~ ", paste(item_blocks[[j]], collapse = " + "))
})
mod <- paste(loading_parts, collapse = "\n")

inla_peak <- peakRAM::peakRAM({
  fit <- acfa(
    mod,
    X_df,
    std.lv = TRUE,
    orthogonal = TRUE,
    priors_for(lambda = "normal(0,0.1)"),
    marginal_method = "marggaus",
    # optim_method = "optim"
    vb_correction = FALSE,
    control = list(iter.max = 5000, eval.max = 5000)
  )
})
inla_ram_mib <- extract_peak_ram(inla_peak)
inla_time_sec <- extract_elapsed_time(inla_peak)

inla_sigma <- fitted(fit)$cov
if (is.list(inla_sigma)) {
  inla_sigma <- inla_sigma[[1]]
}

rv_inla <- MatrixCorrelation::RV(inla_sigma, Sigma)

results <- data.frame(
  method = c("CAVI", "SVI", "INLA"),
  RV = c(rv_cavi, rv_svi, rv_inla),
  elapsed_time_sec = c(cavi_time_sec, svi_time_sec, inla_time_sec),
  peak_ram_mib = c(cavi_ram_mib, svi_ram_mib, inla_ram_mib)
)

print(results)
