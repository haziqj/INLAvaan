# pars_to_x() maps theta to lavaan's x through a parameter map that is built
# once per model. The reference below is the definition of that map, one row of
# the parameter table at a time: each row goes through its own ginv, then each
# covariance is scaled by the standard deviations of its two ends. Every fit
# goes through pars_to_x(), so it must agree with the definition to the last
# digit.

pars_to_x_rowwise <- function(theta, pt) {
  group <- if ("level" %in% names(pt)) partable_level_index(pt) else pt$group
  free <- pt$free > 0L
  pars <- pt$parstart
  pars[free] <- theta
  x <- mapply(function(f, z) f(z), pt$ginv, pars)
  xcor <- x
  sd1sd2 <- rep(1, length(x))
  thidx <- integer(length(x))
  thidx[free] <- seq_len(sum(free))
  owner <- seq_along(x)
  is_copy <- free & duplicated(pt$free)
  owner[is_copy] <- match(pt$free[is_copy], pt$free)
  jcb_mat <- NULL
  for (j in which(grepl("cov", pt$mat))) {
    k <- owner[j]
    var_row <- function(v) {
      which(pt$lhs == v & pt$op == "~~" & pt$rhs == v & group == group[k])
    }
    v1 <- var_row(pt$lhs[k])
    v2 <- var_row(pt$rhs[k])
    sd1 <- sqrt(x[v1])
    sd2 <- sqrt(x[v2])
    rho <- x[j]
    x[j] <- rho * sd1 * sd2
    sd1sd2[j] <- sd1 * sd2
    jcb_mat <- rbind(
      jcb_mat,
      c(thidx[v1], thidx[j], 0.5 * rho * sd1 * sd2),
      c(thidx[v2], thidx[j], 0.5 * rho * sd1 * sd2)
    )
  }
  if (!is.null(jcb_mat)) {
    jcb_mat <- jcb_mat[jcb_mat[, 1] != 0 & jcb_mat[, 2] != 0, , drop = FALSE]
  }
  keep <- free & !duplicated(pt$free)
  out <- x[keep]
  attr(out, "xcor") <- xcor[keep]
  attr(out, "sd1sd2") <- sd1sd2[free]
  attr(out, "jcb_mat") <- jcb_mat
  out
}

chain_rule_rowwise <- function(gll, theta, x, pt) {
  jcb <- mapply(function(f, z) f(z), pt$ginv_prime[pt$free > 0L], theta)
  out <- jcb * attr(x, "sd1sd2") * gll
  jcb_mat <- attr(x, "jcb_mat")
  for (k in seq_len(NROW(jcb_mat))) {
    i <- jcb_mat[k, 1]
    out[i] <- out[i] + jcb_mat[k, 3] * gll[jcb_mat[k, 2]]
  }
  out
}

partable_of <- function(model, data, ...) {
  fit0 <- lavaan::sem(model, data, do.fit = FALSE, ceq.simple = TRUE, ...)
  inlavaanify_partable(fit0@ParTable, priors_for(), fit0@Data, fit0@Options)
}

hs <- lavaan::HolzingerSwineford1939
hs_model <- "
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
"
tables <- list(
  # Free covariances that share variances, so a variance has several terms
  covariances = partable_of(
    "
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
    ",
    lavaan::PoliticalDemocracy
  ),
  # Covariances and variances held equal over groups
  multigroup = partable_of(
    hs_model,
    hs,
    group = "school",
    group.equal = c("loadings", "lv.variances", "lv.covariances")
  ),
  # Correlations under std.lv next to a residual covariance
  std_lv = partable_of(paste(hs_model, "x1 ~~ x4"), hs, std.lv = TRUE),
  # A fixed covariance and a fixed variance
  fixed = partable_of(
    paste(hs_model, "x1 ~~ 0.1*x2 \n x3 ~~ 0.5*x3 \n x3 ~~ x6"),
    hs
  ),
  two_level = partable_of(
    "
    level: 1
      fw =~ y1 + y2 + y3
      y1 ~~ y2
    level: 2
      fb =~ y1 + y2 + y3
      y2 ~~ y3
    ",
    lavaan::Demo.twolevel,
    cluster = "cluster"
  )
)
# A row with a transform other than the identity, exp and tanh
other <- tables$covariances
row <- which(other$op == "=~" & other$free > 0L)[1L]
other$ginv[[row]] <- function(x) x^3
other$ginv_prime[[row]] <- function(x) 3 * x^2
tables$other_transform <- other

test_that("pars_to_x() with a parameter map equals the row-by-row definition", {
  set.seed(1)
  for (name in names(tables)) {
    pt <- tables[[name]]
    map <- pars_to_x_map(pt)
    free_id <- pt$free[pt$free > 0L]
    for (scale in c(0, 0.1, 0.5)) {
      # Rows held equal carry one value
      packed <- pt$parstart[pt$free > 0L & !duplicated(pt$free)]
      theta <- (packed + scale * rnorm(length(packed)))[free_id]
      ref <- pars_to_x_rowwise(theta, pt)
      expect_identical(pars_to_x(theta, pt, map), ref, info = name)
      expect_identical(pars_to_x(theta, pt), ref, info = name)

      gll <- rnorm(length(theta))
      expect_identical(
        pars_chain_rule(gll, theta, ref, pt, map),
        chain_rule_rowwise(gll, theta, ref, pt),
        info = name
      )
    }
  }
})

test_that("The parameter map finds the rows of each covariance", {
  pt <- tables$covariances
  map <- pars_to_x_map(pt)
  expect_identical(map$cov_rows, which(pt$mat == "theta_cov"))
  expect_true(all(pt$lhs[map$cov_var1] == pt$lhs[map$cov_rows]))
  expect_true(all(pt$rhs[map$cov_var2] == pt$rhs[map$cov_rows]))
  expect_true(all(pt$op[c(map$cov_var1, map$cov_var2)] == "~~"))
  # y2, y4, y6 and y8 are each in two covariances
  expect_length(map$jcb_rounds, 2L)

  # A variance in level 2 scales the covariance of level 2 only
  pt <- tables$two_level
  map <- pars_to_x_map(pt)
  level <- partable_level_index(pt)
  expect_identical(level[map$cov_var1], level[map$cov_rows])
  expect_identical(level[map$cov_var2], level[map$cov_rows])

  # Without a free covariance there are no off-diagonal terms
  map <- pars_to_x_map(partable_of(hs_model, hs, std.lv = TRUE))
  expect_false(map$has_cov)
  expect_null(map$jcb_rounds)
})
