test_that("Correct qsn_fast results", {
  expect_equal(
    sn::qsn(c(0.025, 0.5, 0.975), xi = 2.09, omega = 0.19, alpha = 0.96),
    qsnorm_fast(c(0.025, 0.5, 0.975), xi = 2.09, omega = 0.19, alpha = 0.96),
    tolerance = 1e-3
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
