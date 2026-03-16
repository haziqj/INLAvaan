dat     <- lavaan::HolzingerSwineford1939
sem_dat <- lavaan::PoliticalDemocracy
NSAMP   <- 3

mod <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
"
sem_mod <- "
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem60 ~ ind60
"

# Fit once; reused across all tests below
fit_cfa <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP)
fit_mg  <- acfa(mod, dat, verbose = FALSE, nsamp = NSAMP, group = "school")
fit_sem <- asem(sem_mod, sem_dat, verbose = FALSE, nsamp = NSAMP)

# ---- CFA: type = "lv" (default) -----------------------------------------

test_that("type = 'lv' returns correctly-shaped latent predictions", {
  prd  <- predict(fit_cfa, nsamp = NSAMP)
  summ <- summary(prd)
  expect_no_error(capture.output(print(prd)))
  expect_no_error(capture.output(print(summ)))
  expect_equal(length(prd),     NSAMP)
  expect_equal(nrow(summ$Mean), nrow(dat))
})

# ---- CFA: type = "yhat" / "ov" and "ypred" / "ydist" -------------------

test_that("type = 'yhat' and alias 'ov' return n x p fitted-mean matrices", {
  prd_yhat <- predict(fit_cfa, type = "yhat", nsamp = NSAMP)
  prd_ov   <- predict(fit_cfa, type = "ov",   nsamp = NSAMP)
  expect_equal(length(prd_yhat),    NSAMP)
  expect_equal(nrow(prd_yhat[[1]]), nrow(dat))
  expect_equal(ncol(prd_yhat[[1]]), 6L)
  expect_equal(dim(prd_ov[[1]]),    dim(prd_yhat[[1]]))
})

test_that("type = 'ypred' and alias 'ydist' return n x p predicted matrices", {
  prd_ypred <- predict(fit_cfa, type = "ypred", nsamp = NSAMP)
  prd_ydist <- predict(fit_cfa, type = "ydist", nsamp = NSAMP)
  expect_equal(length(prd_ypred),    NSAMP)
  expect_equal(nrow(prd_ypred[[1]]), nrow(dat))
  expect_equal(ncol(prd_ypred[[1]]), 6L)
  expect_equal(dim(prd_ydist[[1]]),  dim(prd_ypred[[1]]))
})

# ---- SEM (B-matrix path): lv, yhat, ypred --------------------------------

test_that("SEM predict covers lv, yhat, and ypred types", {
  prd_lv    <- predict(fit_sem, nsamp = NSAMP)
  prd_yhat  <- predict(fit_sem, type = "yhat",  nsamp = NSAMP)
  prd_ypred <- predict(fit_sem, type = "ypred", nsamp = NSAMP)
  n <- nrow(sem_dat)
  expect_equal(nrow(summary(prd_lv)$Mean), n)
  expect_equal(nrow(prd_yhat[[1]]),         n)
  expect_equal(ncol(prd_yhat[[1]]),         7L)
  expect_equal(nrow(prd_ypred[[1]]),        n)
})

# ---- newdata -------------------------------------------------------------

test_that("newdata works for lv, yhat, and ypred types", {
  newdat <- dat[1:5, ]
  expect_equal(nrow(predict(fit_cfa, type = "lv",    newdata = newdat, nsamp = NSAMP)[[1]]), 5L)
  expect_equal(nrow(predict(fit_cfa, type = "yhat",  newdata = newdat, nsamp = NSAMP)[[1]]), 5L)
  expect_equal(nrow(predict(fit_cfa, type = "ypred", newdata = newdat, nsamp = NSAMP)[[1]]), 5L)
})

test_that("type = 'ymis' with newdata throws an error", {
  expect_error(
    predict(fit_cfa, type = "ymis", newdata = dat[1:5, ], nsamp = NSAMP),
    "ymis.*newdata"
  )
})

# ---- Multigroup: lv, yhat, newdata ---------------------------------------

test_that("multigroup predict works for lv, yhat, and newdata", {
  prd      <- predict(fit_mg, nsamp = NSAMP)
  prd_yhat <- predict(fit_mg, type = "yhat", nsamp = NSAMP)
  newdat   <- rbind(
    dat[dat$school == "Pasteur",     ][1:3, ],
    dat[dat$school == "Grant-White", ][1:2, ]
  )
  prd_new  <- predict(fit_mg, type = "lv", newdata = newdat, nsamp = NSAMP)
  expect_equal(nrow(summary(prd)$Mean),      nrow(dat))
  expect_equal(nrow(summary(prd_yhat)$Mean), nrow(dat))
  expect_equal(nrow(summary(prd_new)$Mean),  5L)
})
