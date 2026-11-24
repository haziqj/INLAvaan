test_that("Correct qsn_fast results", {
  skip_if_not_installed("sn")
  expect_equal(
    sn::qsn(c(0.025, 0.5, 0.975), xi = 2.09, omega = 0.19, alpha = 0.96),
    qsnorm_fast(c(0.025, 0.5, 0.975), xi = 2.09, omega = 0.19, alpha = 0.96),
    tolerance = 1e-3
  )
})

test_that("qsnorm_fast is correct for negative alpha", {
  skip_if_not_installed("sn")
  probs <- c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)

  # Test with negative alpha (previously broken)
  q_fast <- qsnorm_fast(probs, xi = -0.44, omega = 0.25, alpha = -2.0)
  q_sn <- sn::qsn(probs, xi = -0.44, omega = 0.25, alpha = -2.0)
  expect_equal(q_fast, q_sn, tolerance = 1e-3)

  # Verify symmetry: Q(-alpha, u) == -Q(alpha, 1-u)
  q_pos <- qsnorm_fast(1 - probs, xi = 0, omega = 1, alpha = 2.0)
  q_neg <- qsnorm_fast(probs, xi = 0, omega = 1, alpha = -2.0)
  expect_equal(q_neg, -q_pos, tolerance = 1e-6)
})

test_that("qsnorm_fast is monotonically non-decreasing", {
  u <- seq(0.01, 0.99, length = 500)
  # Positive alpha
  q1 <- qsnorm_fast(u, xi = 0, omega = 1, alpha = 1.5)
  expect_true(all(diff(q1) >= -1e-10))
  # Negative alpha
  q2 <- qsnorm_fast(u, xi = 0, omega = 1, alpha = -1.5)
  expect_true(all(diff(q2) >= -1e-10))
})

test_that("qsnorm_fast() is exact at alpha 0 and 1 when alpha varies", {
  p <- c(0.3, 0.3, 0.3)
  q <- qsnorm_fast(p, alpha = c(0, 1, 2))
  expect_equal(q[1:2], c(stats::qnorm(0.3), stats::qnorm(sqrt(0.3))))
  expect_identical(
    qsnorm_fast(c(0, 1, 0.3), alpha = c(-2, -2, 2))[1:2],
    c(-Inf, Inf)
  )
})

# library(microbenchmark)
# microbenchmark(
#   sn = sn::qsn(c(0.025, 0.5, 0.975), xi = 2.09, omega = 0.19, alpha = 0.96),
#   INLAvaan = qsnorm_fast(
#     c(0.025, 0.5, 0.975),
#     xi = 2.09,
#     omega = 0.19,
#     alpha = 0.96
#   ),
#   times = 5000
# )
