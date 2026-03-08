devtools::load_all(quiet = TRUE)
set.seed(42)
dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
fit <- acfa(mod, dat, verbose = FALSE)
int <- fit@external$inlavaan_internal

# Which parameter index is x9~~x9?
pt <- int$partable
idx <- which(pt$lhs == "x9" & pt$op == "~~" & pt$rhs == "x9" & pt$free > 0)
j <- pt$free[idx]
cat("Parameter index j =", j, "\n")
cat("SN params:\n")
print(int$approx_data[j, ])

# Test qsnorm_fast with these params
xi <- int$approx_data[j, "xi"]
omega <- int$approx_data[j, "omega"]
alpha <- int$approx_data[j, "alpha"]

# Comparison: qsnorm_fast vs numerical inversion
cat("\n--- Comparison: qsnorm_fast vs numerical inversion ---\n")
test_u <- c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)
q_fast_test <- qsnorm_fast(test_u, xi = xi, omega = omega, alpha = alpha)

q_numr <- sapply(test_u, function(p) {
  obj <- function(th) {
    integrate(function(x) dsnorm(x, xi, omega, alpha), -Inf, th)$value - p
  }
  uniroot(obj, interval = c(xi - 10 * omega, xi + 10 * omega))$root
})

cat(sprintf("  %-8s %-15s %-15s %-15s\n", "u", "qsnorm_fast", "numerical", "diff"))
for (i in seq_along(test_u)) {
  cat(sprintf("  %-8.2f %-15.6f %-15.6f %-15.6e\n",
              test_u[i], q_fast_test[i], q_numr[i],
              q_fast_test[i] - q_numr[i]))
}

# Check monotonicity
u_seq <- seq(0.001, 0.999, length = 5000)
q_fast <- qsnorm_fast(u_seq, xi = xi, omega = omega, alpha = alpha)
is_mono <- all(diff(q_fast) >= 0)
cat("\nMonotonically increasing:", is_mono, "\n")

# Check all parameters
cat("\n=== Max absolute error across all parameters ===\n")
for (jj in seq_len(nrow(int$approx_data))) {
  xi_j <- int$approx_data[jj, "xi"]
  omega_j <- int$approx_data[jj, "omega"]
  alpha_j <- int$approx_data[jj, "alpha"]

  q_j <- qsnorm_fast(test_u, xi = xi_j, omega = omega_j, alpha = alpha_j)
  q_nr <- sapply(test_u, function(p) {
    obj <- function(th) {
      integrate(function(x) dsnorm(x, xi_j, omega_j, alpha_j), -Inf, th)$value - p
    }
    tryCatch(
      uniroot(obj, interval = c(xi_j - 10 * omega_j, xi_j + 10 * omega_j))$root,
      error = function(e) NA
    )
  })
  max_err <- max(abs(q_j - q_nr), na.rm = TRUE)
  cat(sprintf("  Param %2d (alpha=%6.3f): max|err|=%.6f\n", jj, alpha_j, max_err))
}
