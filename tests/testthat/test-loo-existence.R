# Existence conditions for the second-order terms, checked on hand-built
# curvature rather than on a fitted model, so the algebra is pinned directly.
#
# With Sigma = I the spectrum of -Sigma H_u is just the spectrum of -H_u, so
# a diagonal H_u places the eigenvalues exactly where a case needs them.
# Two conditions read opposite ends of that spectrum:
#
#   Gate A: Sigma^-1 + H_u > 0  <=>  k_max < 1   -> log_cpo_2 (loo)
#   Gate B: Sigma^-1 - H_u > 0  <=>  k_min > -1  -> lpd_2     (waic, p_loo)

S_act <- diag(2)
S_inv <- diag(2)
R_act <- diag(2)
s_u <- c(0.3, -0.4)
l_star <- -10

# H_u whose -Sigma H_u spectrum is exactly `ev`
unit_at <- function(ev) {
  INLAvaan:::taylor_loo_unit(
    l_star,
    s_u,
    -diag(ev),
    S_act,
    S_inv,
    R_act = R_act
  )
}

test_that("the two existence gates are independent (full 2x2 table)", {
  # each row fixes (k_max, k_min) and asserts which second-order terms exist
  cases <- list(
    list(ev = c(0.5, -0.5), gate_a = TRUE, gate_b = TRUE),
    list(ev = c(1.5, 0.2), gate_a = FALSE, gate_b = TRUE),
    list(ev = c(0.2, -1.5), gate_a = TRUE, gate_b = FALSE),
    list(ev = c(1.5, -1.5), gate_a = FALSE, gate_b = FALSE)
  )
  for (cs in cases) {
    u <- unit_at(cs$ev)
    lab <- paste0("ev = (", paste(cs$ev, collapse = ", "), ")")
    expect_equal(u$k_max, max(cs$ev), info = lab)
    expect_equal(u$k_min, min(cs$ev), info = lab)
    # Gate A governs the case-deletion term, Gate B the lpd term
    expect_identical(u$ok, cs$gate_a, info = lab)
    expect_identical(!is.na(u$lpd_2), cs$gate_b, info = lab)
    expect_identical(!is.na(u$log_cpo_2), cs$gate_a, info = lab)
  }
})

test_that("p_waic carries no existence condition of its own", {
  for (ev in list(c(0.5, -0.5), c(1.5, 0.2), c(0.2, -1.5), c(1.5, -1.5))) {
    u <- unit_at(ev)
    # p_waic = s' Sigma s + tr[(H Sigma)^2] / 2, assembled from lpd_1 and
    # k_ssq; a polynomial in the moments, so finite wherever the gates fail
    p_waic <- 2 * (u$lpd_1 - l_star) + 0.5 * u$k_ssq
    expect_true(is.finite(p_waic))
    expect_gt(p_waic, 0)
  }
})

test_that("the closed-form p_waic matches a direct trace computation", {
  H_u <- matrix(c(-0.6, 0.15, 0.15, -0.35), 2L)
  u <- INLAvaan:::taylor_loo_unit(
    l_star,
    s_u,
    H_u,
    S_act,
    S_inv,
    R_act = R_act
  )
  HS <- H_u %*% S_act
  expect_equal(
    2 * (u$lpd_1 - l_star) + 0.5 * u$k_ssq,
    as.numeric(crossprod(s_u, S_act %*% s_u)) + 0.5 * sum(diag(HS %*% HS)),
    tolerance = 1e-12
  )
})

test_that("first-order terms need no gate and reproduce log_cpo_1", {
  # no eigendecomposition, no Cholesky: the quadratic form s' Sigma s is all
  # that is needed, so every second-order field stays NA
  u <- INLAvaan:::taylor_loo_unit(
    l_star,
    s_u,
    NULL,
    S_act,
    S_inv,
    R_act = R_act,
    second_order = FALSE
  )
  expect_true(all(is.na(c(
    u$log_cpo_2,
    u$lpd_2,
    u$k_max,
    u$k_min,
    u$k_sum,
    u$k_ssq
  ))))
  quad <- as.numeric(crossprod(s_u, S_act %*% s_u))
  expect_equal(u$lpd_1, l_star + 0.5 * quad)
  expect_equal(u$log_cpo_1, l_star - 0.5 * quad)
  # elpd_waic = lpd - p_waic collapses to log_cpo_1 at first order
  expect_equal(u$lpd_1 - quad, u$log_cpo_1)
})

# A minimal inlavaan_loo stand-in carrying only what waic_from_taylor reads
fake_loo <- function(lpd_2) {
  per_unit <- data.frame(
    unit = seq_along(lpd_2),
    nobs = 1L,
    l_star = c(-10, -11, -12),
    lpd_1 = c(-9.5, -10.4, -11.6),
    lpd_2 = lpd_2,
    k_ssq = c(0.2, 0.3, 0.1)
  )
  list(
    per_unit = per_unit,
    n_units = length(lpd_2),
    second_order = TRUE,
    type = "loso",
    flavour = "joint",
    n_groups = 1L
  )
}

test_that("elpd_waic ignores the log CPO gate", {
  # Gate A failing is invisible here: the WAIC reads no case-deletion term,
  # which is why inlav_waic() muffles loo()'s reversion warning
  res <- fake_loo(c(-9.4, -10.3, -11.5))
  w <- INLAvaan:::waic_from_taylor(res)
  expect_true(w$use_second)
  expect_equal(w$n_lpd_ok, 3L)
  expect_silent(INLAvaan:::waic_from_taylor(res))
})

test_that("a failed lpd gate sends every WAIC estimate to first order", {
  res <- fake_loo(c(NA_real_, -10.3, -11.5))
  expect_warning(
    w <- INLAvaan:::waic_from_taylor(res),
    class = "inlavaan_waic_first_order"
  )
  expect_false(w$use_second)
  expect_true(w$second_order) # requested, but not reachable
  expect_equal(w$n_lpd_ok, 2L)

  # first order over *all* units, not a per-unit substitution: every
  # contribution is lpd_1 - quad, including the units that kept their term
  pu <- res$per_unit
  quad <- 2 * (pu$lpd_1 - pu$l_star)
  expect_equal(w$per_unit$lpd, pu$lpd_1)
  expect_equal(w$per_unit$p_waic, quad)
  expect_equal(w$per_unit$elpd_waic, pu$lpd_1 - quad)
})
