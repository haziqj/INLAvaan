dat <- lavaan::HolzingerSwineford1939
mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "
NSAMP <- 3

test_that("Predict method works (type = 'lv')", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP)
  prd <- predict(fit, nsamp = NSAMP)
  prd_summ <- summary(prd)
  expect_no_error(out <- capture.output(print(prd)))
  expect_no_error(out <- capture.output(print(prd_summ)))
  expect_equal(length(prd), NSAMP)
  expect_equal(nrow(prd_summ$Mean), nrow(dat))
})

test_that("Multigroup predict method works", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP, group = "school")
  prd <- predict(fit, nsamp = NSAMP)
  prd_summ <- summary(prd)
  expect_no_error(out <- capture.output(print(prd)))
  expect_no_error(out <- capture.output(print(prd_summ)))
  expect_equal(length(prd), NSAMP)
  expect_equal(nrow(prd_summ$Mean), nrow(dat))
})

test_that("Predict method works for SEM model (B-matrix path)", {
  sem_mod <- "
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem60 ~ ind60
  "
  sem_dat <- lavaan::PoliticalDemocracy
  fit <- asem(sem_mod, sem_dat, verbose = FALSE, nsamp = NSAMP)
  prd <- predict(fit, nsamp = NSAMP)
  prd_summ <- summary(prd)
  expect_equal(length(prd), NSAMP)
  expect_equal(nrow(prd_summ$Mean), nrow(sem_dat))
})

# -- type = "yhat" and alias "ov" ------------------------------------------

test_that("type = 'yhat' returns fitted means (no noise)", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP)
  prd <- predict(fit, type = "yhat", nsamp = NSAMP)
  expect_equal(length(prd), NSAMP)
  # Columns should match observed variable names
  expect_equal(ncol(prd[[1]]), 6L)
  # Rows should match training data
  expect_equal(nrow(prd[[1]]), nrow(dat))
})

test_that("type = 'ov' is an alias for 'yhat'", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP)
  # Both types should give same-dimensioned output
  prd_yhat <- predict(fit, type = "yhat", nsamp = NSAMP)
  prd_ov   <- predict(fit, type = "ov",   nsamp = NSAMP)
  expect_equal(dim(prd_yhat[[1]]), dim(prd_ov[[1]]))
})

# -- type = "ypred" and alias "ydist" --------------------------------------

test_that("type = 'ypred' returns predicted values with noise", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP)
  prd <- predict(fit, type = "ypred", nsamp = NSAMP)
  expect_equal(length(prd), NSAMP)
  expect_equal(ncol(prd[[1]]), 6L)
  expect_equal(nrow(prd[[1]]), nrow(dat))
})

test_that("type = 'ydist' is an alias for 'ypred'", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP)
  prd_ypred <- predict(fit, type = "ypred", nsamp = NSAMP)
  prd_ydist <- predict(fit, type = "ydist", nsamp = NSAMP)
  expect_equal(dim(prd_ypred[[1]]), dim(prd_ydist[[1]]))
})

# -- type = "yhat" / "ypred" for SEM (B-matrix) ----------------------------

test_that("type = 'yhat' works with SEM model", {
  sem_mod <- "
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem60 ~ ind60
  "
  sem_dat <- lavaan::PoliticalDemocracy
  fit <- asem(sem_mod, sem_dat, verbose = FALSE, nsamp = NSAMP)
  prd <- predict(fit, type = "yhat", nsamp = NSAMP)
  expect_equal(length(prd), NSAMP)
  expect_equal(nrow(prd[[1]]), nrow(sem_dat))
  expect_equal(ncol(prd[[1]]), 7L)
})

test_that("type = 'ypred' works with SEM model", {
  sem_mod <- "
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem60 ~ ind60
  "
  sem_dat <- lavaan::PoliticalDemocracy
  fit <- asem(sem_mod, sem_dat, verbose = FALSE, nsamp = NSAMP)
  prd <- predict(fit, type = "ypred", nsamp = NSAMP)
  expect_equal(length(prd), NSAMP)
  expect_equal(nrow(prd[[1]]), nrow(sem_dat))
})

# -- newdata ---------------------------------------------------------------

test_that("newdata works with type = 'lv'", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP)
  newdat <- dat[1:10, ]
  prd <- predict(fit, type = "lv", newdata = newdat, nsamp = NSAMP)
  expect_equal(length(prd), NSAMP)
  expect_equal(nrow(prd[[1]]), 10L)
})

test_that("newdata works with type = 'yhat'", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP)
  newdat <- dat[1:5, ]
  prd <- predict(fit, type = "yhat", newdata = newdat, nsamp = NSAMP)
  expect_equal(length(prd), NSAMP)
  expect_equal(nrow(prd[[1]]), 5L)
})

test_that("newdata works with type = 'ypred'", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP)
  newdat <- dat[1:5, ]
  prd <- predict(fit, type = "ypred", newdata = newdat, nsamp = NSAMP)
  expect_equal(length(prd), NSAMP)
  expect_equal(nrow(prd[[1]]), 5L)
})

test_that("newdata with multigroup predict works", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP, group = "school")
  # Subset: 3 from Pasteur, 2 from Grant-White
  newdat <- rbind(
    dat[dat$school == "Pasteur", ][1:3, ],
    dat[dat$school == "Grant-White", ][1:2, ]
  )
  prd <- predict(fit, type = "lv", newdata = newdat, nsamp = NSAMP)
  prd_summ <- summary(prd)
  expect_equal(length(prd), NSAMP)
  expect_equal(nrow(prd_summ$Mean), 5L)
})

test_that("newdata errors for type = 'ymis'", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP)
  expect_error(
    predict(fit, type = "ymis", newdata = dat[1:5, ], nsamp = NSAMP),
    "ymis.*newdata"
  )
})

# -- multigroup yhat/ypred ------------------------------------------------

test_that("type = 'yhat' works with multigroup model", {
  fit <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP, group = "school")
  prd <- predict(fit, type = "yhat", nsamp = NSAMP)
  prd_summ <- summary(prd)
  expect_equal(length(prd), NSAMP)
  expect_equal(nrow(prd_summ$Mean), nrow(dat))
})
