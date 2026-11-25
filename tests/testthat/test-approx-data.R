dat <- lavaan::HolzingerSwineford1939
mod <- "
  visual  =~ x1 + l2*x2 + l3*x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
  ratio := l2 / l3
"
sn_cols <- c("xi", "omega", "alpha", "logC", "nmad", "gamma1")

# With meanstructure = TRUE the intercepts take the saturated-means fast path,
# the three factor covariances are free, and ratio is a := row.
fit_ad <- function(...) {
  acfa(
    mod,
    dat,
    meanstructure = TRUE,
    silent = TRUE,
    nsamp = 50,
    test = "none",
    ...
  )
}

test_that("approx_data_row() takes fields by name", {
  # Engine fits have 5 fields and 0.3.x fits 7, and both give the same row
  engine <- list(xi = 1, omega = 2, alpha = 3, logC = 4, nmad = 0.01)
  old <- c(engine[1:4], k = 1, rmse = 1e-6, nmad = 0.01)
  rows <- approx_data_rows(list(
    a = c(engine, gamma1 = 0.1),
    b = c(old, gamma1 = 0.1),
    c = list(xi = 5, omega = 6, alpha = 7)
  ))
  expect_identical(colnames(rows), sn_cols)
  expect_identical(rownames(rows), c("a", "b", "c"))
  expect_equal(rows["a", ], rows["b", ])
  expect_equal(unname(rows["c", ]), c(5, 6, 7, NA, NA, NA))
  expect_true(all(is.na(approx_data_row(NULL))))
})

test_that("approx_data has one layout with and without sn_fit_sample", {
  for (sn_fit_sample in c(TRUE, FALSE)) {
    set.seed(1)
    fit <- fit_ad(sn_fit_sample = sn_fit_sample)
    int <- get_inlavaan_internal(fit)
    ad <- int$approx_data
    m <- length(int$theta_star)
    expect_identical(colnames(ad), sn_cols)

    # Marginal rows: the fast path for the intercepts, a scan for the rest
    fp <- grep("~1$", rownames(ad)[seq_len(m)])
    scan <- setdiff(seq_len(m), fp)
    expect_length(fp, 9)
    expect_equal(unname(ad[fp, "xi"]), unname(int$theta_star_novbc[fp]))
    expect_equal(
      unname(ad[fp, "omega"]),
      unname(sqrt(diag(int$Sigma_theta))[fp])
    )
    # The fast-path rows are Gaussian marginals with no scan
    expect_true(all(ad[fp, "alpha"] == 0))
    expect_true(all(is.na(ad[fp, c("logC", "nmad", "gamma1")])))
    expect_true(all(is.finite(ad[scan, ])))
    expect_true(all(ad[scan, "omega"] > 0))
    expect_true(all(ad[scan, "nmad"] >= 0 & ad[scan, "nmad"] < 0.1))

    # Rows fitted to the draws: three covariances and the := parameter
    if (sn_fit_sample) {
      extra <- ad[-seq_len(m), , drop = FALSE]
      expect_identical(
        rownames(extra),
        c("visual~~textual", "visual~~speed", "textual~~speed", "ratio")
      )
      expect_true(all(is.finite(extra[, c("xi", "omega", "alpha")])))
      expect_true(all(is.na(extra[, c("logC", "nmad", "gamma1")])))
    } else {
      expect_equal(nrow(ad), m)
    }

    # The readers
    diag_g <- diagnostics(fit)
    diag_p <- diagnostics(fit, type = "param")
    expect_equal(
      diag_g[["nmad_max"]],
      max(ad[seq_len(m), "nmad"], na.rm = TRUE)
    )
    # The fast-path rows have no scan, so their scan diagnostics are NA
    expect_equal(diag_p$alpha[scan], unname(ad[scan, "alpha"]))
    expect_true(all(is.na(diag_p[fp, c("alpha", "nmad", "unscanned_mass")])))
    expect_true(all(is.finite(diag_p$unscanned_mass[scan])))
    expect_equal(diag_g[["nmad_mean"]], mean(ad[scan, "nmad"]))
    out <- capture.output(summary(fit, nmad = TRUE))
    expect_true(any(grepl("NMAD", out)))
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      expect_s3_class(plot(fit, type = "sn_fit", params = "l2"), "ggplot")
      expect_s3_class(plot(fit, params = "ratio"), "ggplot")
    }
  }
})

test_that("a free covariance with no fast path fits with sn_fit_sample", {
  set.seed(1)
  fit <- acfa(
    mod,
    dat,
    silent = TRUE,
    nsamp = 50,
    test = "none",
    sn_fit_sample = TRUE
  )
  ad <- get_inlavaan_internal(fit)$approx_data
  expect_identical(colnames(ad), sn_cols)
  expect_equal(nrow(ad), length(coef(fit)) + 4L)
})
