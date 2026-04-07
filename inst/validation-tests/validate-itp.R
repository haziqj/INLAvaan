## Validation: ITP (Inverse Transform Parametrisation) for correlation matrices
## Tests that the ITP engine and its integration into the INLAvaan pipeline
## produce valid results, and that CFA estimates match the standard approach.
##
## Run from the package root:
##   Rscript -e 'devtools::load_all(); source("inst/validation-tests/validate-itp.R")'

if (!isNamespaceLoaded("INLAvaan")) devtools::load_all(quiet = TRUE)

cat("=== ITP Validation Tests ===\n\n")
pass <- 0L
fail <- 0L

check <- function(desc, expr) {
  ok <- tryCatch(expr, error = function(e) FALSE)
  if (isTRUE(ok)) {
    cat("[PASS]", desc, "\n")
    pass <<- pass + 1L
  } else {
    cat("[FAIL]", desc, "\n")
    fail <<- fail + 1L
  }
}

# ============================================================================
# Part 1: Core ITP engine (itp_to_corr)
# ============================================================================
cat("--- Part 1: Core ITP engine ---\n")

# Test 1.1: theta = 0 -> identity matrix
for (p in 2:6) {
  m <- p * (p - 1L) / 2L
  lt_idx <- which(lower.tri(diag(p)))
  C <- itp_to_corr(rep(0, m), p, lt_idx, d0 = p:1)
  check(
    sprintf("theta=0 -> I_%d", p),
    max(abs(C - diag(p))) < 1e-10
  )
}

# Test 1.2: Random theta always produces valid correlation matrix
set.seed(42)
for (p in c(2, 3, 5)) {
  m <- p * (p - 1L) / 2L
  lt_idx <- which(lower.tri(diag(p)))
  for (trial in 1:20) {
    theta <- rnorm(m, sd = 2)
    C <- itp_to_corr(theta, p, lt_idx, d0 = p:1)

    check(
      sprintf("p=%d trial=%d: unit diagonal", p, trial),
      max(abs(diag(C) - 1)) < 1e-10
    )

    check(
      sprintf("p=%d trial=%d: symmetric", p, trial),
      max(abs(C - t(C))) < 1e-12
    )

    eig <- eigen(C, symmetric = TRUE, only.values = TRUE)$values
    check(
      sprintf(
        "p=%d trial=%d: positive definite (min eig = %.2e)",
        p,
        trial,
        min(eig)
      ),
      min(eig) > 0
    )

    check(
      sprintf("p=%d trial=%d: valid correlations in [-1,1]", p, trial),
      all(C >= -1 - 1e-10) && all(C <= 1 + 1e-10)
    )
  }
}

# Test 1.3: Sparse graph (not all off-diags free)
p <- 4
# Graph: 1-2, 2-3, 3-4 (tridiagonal)
# Lower triangle positions (col-major): (2,1)=2, (3,2)=7, (4,3)=12
iLtheta_sparse <- c(2L, 7L, 12L)
theta_sparse <- c(0.5, -0.3, 0.2)
C_sparse <- itp_to_corr(theta_sparse, p, iLtheta_sparse, d0 = 4:1)

check("sparse p=4: unit diagonal", max(abs(diag(C_sparse) - 1)) < 1e-10)
check("sparse p=4: symmetric", max(abs(C_sparse - t(C_sparse))) < 1e-12)
eig_sp <- eigen(C_sparse, symmetric = TRUE, only.values = TRUE)$values
check(
  sprintf("sparse p=4: positive definite (min eig = %.2e)", min(eig_sp)),
  min(eig_sp) > 0
)

# ============================================================================
# Part 2: Jacobian via central differences
# ============================================================================
cat("\n--- Part 2: Jacobian ---\n")

# Test 2.1: Jacobian at theta=0 is non-degenerate
for (p in c(2, 3, 4)) {
  m <- p * (p - 1L) / 2L
  lt_idx <- which(lower.tri(diag(p)))
  J <- itp_jac_corr(rep(0, m), p, lt_idx, d0 = p:1)
  check(
    sprintf("p=%d: Jacobian at theta=0 is %d x %d", p, nrow(J), ncol(J)),
    nrow(J) == m && ncol(J) == m
  )
  d <- abs(det(J))
  check(
    sprintf("p=%d: |det(J)| at theta=0 = %.4e (non-zero)", p, d),
    d > 1e-10
  )
}

# Test 2.2: Jacobian at random theta matches finite differences of itp_to_corr
set.seed(123)
p <- 3
m <- 3L
lt_idx <- which(lower.tri(diag(p)))
theta0 <- rnorm(m, sd = 0.5)
J <- itp_jac_corr(theta0, p, lt_idx, d0 = p:1, h = 1e-6)

# Verify against a second-order finite difference with different h
J2 <- itp_jac_corr(theta0, p, lt_idx, d0 = p:1, h = 1e-4)
check(
  sprintf(
    "p=3: Jacobian consistent across step sizes (max diff = %.2e)",
    max(abs(J - J2))
  ),
  max(abs(J - J2)) < 1e-4
)

# ============================================================================
# Part 3: ITP block extraction from parameter table
# ============================================================================
cat("\n--- Part 3: Block extraction from parameter table ---\n")

# Build a simple CFA model with correlated factors
mod_2f <- "
  F1 =~ x1 + x2 + x3
  F2 =~ x4 + x5 + x6
"
dat <- lavaan::HolzingerSwineford1939

# Fit lavaan to get the template pt
fit_lav <- lavaan::cfa(mod_2f, dat, std.lv = TRUE)
pt_std <- inlavaanify_partable(
  lavaan::parTable(fit_lav),
  lavdata = fit_lav@Data,
  lavoptions = fit_lav@Options,
  use_itp = TRUE
)

check("ITP blocks created (non-empty)", length(pt_std$itp_blocks) > 0)

# There should be exactly one psi block (factor covariances)
blk1 <- pt_std$itp_blocks[[1]]
check(
  sprintf("Block 1: p = %d, %d free params", blk1$p, length(blk1$iLtheta)),
  blk1$p == 2L && length(blk1$iLtheta) == 1L
)

# Check that ITP correlation params have identity transform
for (ci in blk1$pt_cor_idx) {
  check(
    sprintf("pt_cor_idx %d: ginv is identity", ci),
    identical(pt_std$ginv[[ci]](0.5), 0.5)
  )
  check(
    sprintf("pt_cor_idx %d: parstart is 0", ci),
    pt_std$parstart[ci] == 0
  )
}

# ============================================================================
# Part 4: Prior cache setup (trans_type = 3 for ITP params)
# ============================================================================
cat("\n--- Part 4: Prior cache ---\n")

cache <- prepare_priors_for_optim(pt_std)
itp_idx <- which(cache$trans_type == 3L)
check("ITP params have trans_type = 3", length(itp_idx) > 0)

# Non-ITP variance params should still have trans_type = 1
exp_idx <- which(cache$trans_type == 1L)
check("Variance params still have trans_type = 1", length(exp_idx) > 0)

# ============================================================================
# Part 5: Prior log-density evaluable at theta = 0
# ============================================================================
cat("\n--- Part 5: Prior log-density ---\n")

nfree <- sum(pt_std$free > 0 & !duplicated(pt_std$free))
theta0 <- rep(0, nfree)
lp <- prior_logdens_vectorized(theta0, cache)
check(
  sprintf("prior_logdens at theta=0 is finite: %.4f", lp),
  is.finite(lp)
)

# Also at a random theta
set.seed(7)
theta_rnd <- rnorm(nfree, sd = 0.3)
lp_rnd <- prior_logdens_vectorized(theta_rnd, cache)
check(
  sprintf("prior_logdens at random theta is finite: %.4f", lp_rnd),
  is.finite(lp_rnd)
)

# ============================================================================
# Part 6: Prior gradient via numerical central differences
# ============================================================================
cat("\n--- Part 6: Prior gradient ---\n")

grad <- prior_grad_vectorized(theta0, cache)
check(
  sprintf("prior_grad at theta=0: length = %d", length(grad)),
  length(grad) ==
    length(itp_idx) +
      length(exp_idx) +
      length(which(cache$trans_type == 0L)) +
      length(which(cache$trans_type == 2L))
)
check("prior_grad at theta=0: all finite", all(is.finite(grad)))

# Verify ITP gradient entries against manual central differences
h <- 1e-5
for (ii in itp_idx) {
  fid <- cache$free_id[ii]
  tp <- tm <- theta0
  tp[fid] <- theta0[fid] + h
  tm[fid] <- theta0[fid] - h
  fd_grad <- (prior_logdens_vectorized(tp, cache) -
    prior_logdens_vectorized(tm, cache)) /
    (2 * h)
  check(
    sprintf(
      "ITP grad[%d] pipeline vs manual FD: diff = %.2e",
      ii,
      abs(grad[ii] - fd_grad)
    ),
    abs(grad[ii] - fd_grad) < 1e-4
  )
}

# ============================================================================
# Part 7: pars_to_x integration
# ============================================================================
cat("\n--- Part 7: pars_to_x ---\n")

# At theta = 0, correlation params should map to identity (rho = 0)
x0 <- pars_to_x(theta0, pt_std)
xcor0 <- attr(x0, "xcor")

# Find which free params correspond to ITP correlation
itp_free_ids <- pt_std$free[blk1$pt_cor_idx]
for (fid in itp_free_ids) {
  check(
    sprintf("pars_to_x: ITP free_id %d maps to rho near 0 at theta=0", fid),
    abs(xcor0[fid]) < 1e-10
  )
}

# At random theta, ITP correlation should be in [-1, 1]
x_rnd <- pars_to_x(theta_rnd, pt_std)
xcor_rnd <- attr(x_rnd, "xcor")
for (fid in itp_free_ids) {
  check(
    sprintf("pars_to_x: ITP free_id %d -> rho in [-1,1] at random theta", fid),
    xcor_rnd[fid] >= -1 && xcor_rnd[fid] <= 1
  )
}

# ============================================================================
# Part 8: Full model fit — CFA with ITP vs standard
# ============================================================================
cat("\n--- Part 8: Full CFA model fit ---\n")

mod_hs <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"

cat("Fitting standard CFA (tanh parametrisation)...\n")
fit_std <- tryCatch(
  inlavaan(mod_hs, data = lavaan::HolzingerSwineford1939,
           std.lv = TRUE, nsamp = 100, verbose = FALSE, model.type = "cfa"),
  error = function(e) {
    cat("  Error:", conditionMessage(e), "\n")
    NULL
  }
)

cat("Fitting ITP CFA (use_itp = TRUE)...\n")
fit_itp <- tryCatch(
  inlavaan(mod_hs, data = lavaan::HolzingerSwineford1939,
           std.lv = TRUE, nsamp = 100, verbose = FALSE,
           model.type = "cfa", use_itp = TRUE),
  error = function(e) {
    cat("  Error:", conditionMessage(e), "\n")
    NULL
  }
)

if (!is.null(fit_std) && !is.null(fit_itp)) {
  # Compare parameter estimates
  est_std <- lavaan::coef(fit_std)
  est_itp <- lavaan::coef(fit_itp)

  # They should share the same parameter names
  shared <- intersect(names(est_std), names(est_itp))
  check(
    sprintf("Same parameter names: %d shared", length(shared)),
    length(shared) == length(est_std)
  )

  if (length(shared) > 0) {
    max_diff <- max(abs(est_std[shared] - est_itp[shared]))
    cat(sprintf("  Max absolute difference in estimates: %.6f\n", max_diff))
    check(
      sprintf("Estimates close (max diff = %.4f)", max_diff),
      max_diff < 0.05
    )

    # Compare log-likelihoods
    ll_std <- tryCatch(as.numeric(lavaan::logLik(fit_std)), error = function(e) NA_real_)
    ll_itp <- tryCatch(as.numeric(lavaan::logLik(fit_itp)), error = function(e) NA_real_)
    if (!is.na(ll_std) && !is.na(ll_itp)) {
      ll_diff <- abs(ll_std - ll_itp)
      cat(sprintf("  Log-likelihood diff: %.6f\n", ll_diff))
      check(
        sprintf("Log-likelihoods close (diff = %.4f)", ll_diff),
        ll_diff < 1.0
      )
    } else {
      cat(sprintf("  Log-likelihoods: std=%.4f, itp=%.4f (comparison skipped)\n",
                  ifelse(is.na(ll_std), NA, ll_std),
                  ifelse(is.na(ll_itp), NA, ll_itp)))
    }
  }
} else {
  cat("  Skipping comparison — one or both fits failed.\n")
  if (is.null(fit_std)) {
    fail <- fail + 1L
  }
  if (is.null(fit_itp)) fail <- fail + 1L
}

# ============================================================================
# Summary
# ============================================================================
cat(sprintf("\n=== Results: %d passed, %d failed ===\n", pass, fail))
if (fail > 0) {
  cat("SOME TESTS FAILED — review output above.\n")
} else {
  cat("ALL TESTS PASSED.\n")
}
