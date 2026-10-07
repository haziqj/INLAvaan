# Extended LOO suite pinned to reference values. It runs in CI, and
# test-loo-loso.R covers the core LOO on CRAN.
skip_on_cran()

# Composite LOO. lavaan fixes the (co)variances T of the composite indicators
# at their sample values, so each unit is scored with T recomputed without it
# (the T-deletion term). Otherwise the LOO is optimistic by about one nat per
# fixed moment.

dat <- lavaan::HolzingerSwineford1939
mod_comp <- "
  C <~ x1 + x2 + x3
  x4 ~ C
  x5 ~ C
  x4 ~~ x5
"
mod_two <- "
  C1 <~ x1 + x2 + x3
  C2 <~ x4 + x5 + x6
  x7 ~ C1 + C2
  x8 ~ C1 + C2
"
# The same model as mod_comp with the indicator moments estimated, which for
# one composite of observed indicators is an exact reparametrisation
mod_phantom <- "
  C =~ 0
  C ~ 1*x1 + x2 + x3
  C ~~ 0*C
  x4 ~ C
  x5 ~ C
  x4 ~~ x5
"
fit_args <- list(
  verbose = FALSE,
  nsamp = 3,
  test = "none",
  vb_correction = FALSE,
  marginal_method = "marggaus",
  marginal_correction = "none"
)
fit_comp <- do.call(asem, c(list(mod_comp, dat), fit_args))
res_comp <- loo(fit_comp)

# The T-deletion term of the given units at the fit's own summary, with T taken
# from lavaan's setup of the data without each unit
exact_t_delta <- function(fit, model, data, units, ...) {
  int <- get_inlavaan_internal(fit)
  lavmodel <- int$lavmodel
  lavdata <- int$lavdata
  t_fixed <- composite_fixed_t(lavmodel, int$partable, lavdata)
  x <- pars_to_x(int$theta_star, int$partable)
  dv <- loso_data_view(lavmodel, lavdata, x_idx = int$lavsamplestats@x.idx)
  mom <- loo_implied_moments(lavaan::lav_model_set_parameters(lavmodel, x))
  vapply(
    units,
    function(u) {
      g <- which(vapply(lavdata@case.idx, function(i) u %in% i, logical(1)))
      r <- match(u, lavdata@case.idx[[g]])
      e <- t_fixed[[g]]
      fit_u <- lavaan::sem(model, data[-u, ], do.fit = FALSE, ...)
      e_u <- composite_fixed_t(fit_u@Model, fit_u@ParTable, fit_u@Data)[[g]]
      lavmodel_u <- lavmodel
      lavmodel_u@GLIST[[e$mm]][e$pos] <- fit_u@Model@GLIST[[e_u$mm]][e_u$pos]
      lavmodel_u <- lavaan::lav_model_set_parameters(lavmodel_u, x)
      y_u <- dv$Y[[g]][r, , drop = FALSE]
      loso_loglik_all(y_u, mom[[g]]) -
        loso_loglik_all(y_u, loo_implied_moments(lavmodel_u)[[g]])
    },
    numeric(1)
  )
}

# The units with the largest terms, where an error would show most
top_units <- function(pu, k = 3L) {
  pu$unit[order(-pu$t_delta)][seq_len(k)]
}

test_that("The T-deletion term re-fixes the indicator block without the unit", {
  pu <- res_comp$per_unit
  expect_true("t_delta" %in% names(pu))
  units <- top_units(pu)
  expect_equal(
    pu$t_delta[match(units, pu$unit)],
    exact_t_delta(fit_comp, mod_comp, dat, units),
    tolerance = 1e-8
  )
  # About one nat per fixed moment (three variances, three covariances)
  expect_gt(sum(pu$t_delta), 6)
  expect_lt(sum(pu$t_delta), 7)

  # Two composites, whose likelihood does not factorise into an indicator
  # block and the rest
  fit_two <- do.call(asem, c(list(mod_two, dat), fit_args))
  res_two <- loo(fit_two)
  units <- top_units(res_two$per_unit)
  expect_equal(
    res_two$per_unit$t_delta[match(units, res_two$per_unit$unit)],
    exact_t_delta(fit_two, mod_two, dat, units),
    tolerance = 1e-8
  )
  expect_gt(sum(res_two$per_unit$t_delta), 12)
  expect_lt(sum(res_two$per_unit$t_delta), 14)
})

test_that("The term enters the log CPO, p_loo and p_waic, not the lpd", {
  pu <- res_comp$per_unit
  res_1 <- loo(fit_comp, second_order = FALSE)
  waic_1 <- waic(fit_comp, second_order = FALSE)
  # First-order WAIC is still the first-order LOO
  expect_equal(waic_1$per_unit$elpd_waic, res_1$per_unit$log_cpo_1)
  expect_equal(
    res_1$per_unit$lpd_1 - res_1$per_unit$log_cpo_1,
    2 * (res_1$per_unit$lpd_1 - res_1$per_unit$l_star) + pu$t_delta
  )
  expect_equal(
    res_comp$estimates["p_loo", "Estimate"],
    sum(pu$lpd_2 - pu$log_cpo_2)
  )
  expect_gt(waic(fit_comp)$estimates["p_waic", "Estimate"], sum(pu$t_delta))

  # A subset of units carries the same terms
  units <- c(268, 5, 100)
  res_sub <- loo(fit_comp, units = units)
  expect_equal(res_sub$per_unit$t_delta, pu$t_delta[match(units, pu$unit)])
})

test_that("The LOO matches the model that estimates the indicator moments", {
  fit_phantom <- do.call(
    asem,
    c(list(mod_phantom, dat, fixed.x = FALSE), fit_args)
  )
  # Without indicator (co)variances fixed at sample values there is no term
  expect_null(loo(fit_phantom)$per_unit$t_delta)
  cmp <- compare(fit_comp, fit_phantom, loo = TRUE)
  expect_lt(abs(cmp$elpd_diff[2]), 2 * cmp$se_diff[2])
  # Without the term the composite would win by about six nats
  expect_gt(sum(res_comp$per_unit$t_delta), 3 * cmp$se_diff[2])
})

test_that("Every route to the LOO and WAIC carries the term", {
  fit_full <- do.call(
    asem,
    c(list(mod_comp, dat), utils::modifyList(fit_args, list(test = "full")))
  )
  stored <- get_inlavaan_internal(fit_full, "loo")
  expect_equal(stored$per_unit$t_delta, res_comp$per_unit$t_delta)
  expect_equal(stored$elpd_2, res_comp$elpd_2)
  expect_identical(
    get_inlavaan_internal(fit_full, "waic"),
    waic_from_taylor(stored)
  )
  fit_added <- add_loo(fit_comp)
  expect_equal(get_inlavaan_internal(fit_added, "loo"), res_comp)
  expect_equal(waic(fit_added), waic(fit_comp))
  fm <- fitMeasures(fit_added, c("elpd_loo", "p_loo", "p_waic"))
  expect_equal(unname(fm["p_loo"]), res_comp$estimates["p_loo", "Estimate"])
})

test_that("Each group's indicator block is re-fixed without its own units", {
  fit_mg <- do.call(asem, c(list(mod_comp, dat, group = "school"), fit_args))
  # One unit of the weakly identified second group has no second-order term,
  # which does not bear on the T-deletion term
  res_mg <- suppressWarnings(
    loo(fit_mg),
    classes = "inlavaan_loo_first_order"
  )
  pu <- res_mg$per_unit
  # Six fixed moments per group
  sums <- tapply(pu$t_delta, pu$group, sum)
  expect_true(all(sums > 6 & sums < 7.5))
  units <- unname(unlist(lapply(split(pu, pu$group), top_units, k = 2L)))
  expect_equal(
    pu$t_delta[match(units, pu$unit)],
    exact_t_delta(fit_mg, mod_comp, dat, units, group = "school"),
    tolerance = 1e-8
  )
})

test_that("Under missing data the term is close to the exact re-fix", {
  dat_mis <- dat[, paste0("x", 1:5)]
  set.seed(42)
  for (v in names(dat_mis)) {
    dat_mis[[v]][runif(nrow(dat_mis)) < 0.1] <- NA
  }
  fit_mis <- do.call(
    asem,
    c(list(mod_comp, dat_mis, missing = "ml"), fit_args)
  )
  res_mis <- loo(fit_mis)
  pu <- res_mis$per_unit
  expect_true(all(is.finite(pu$t_delta)))
  expect_gt(sum(pu$t_delta), 5.5)
  expect_lt(sum(pu$t_delta), 7)
  units <- top_units(pu)
  expect_equal(
    pu$t_delta[match(units, pu$unit)],
    exact_t_delta(fit_mis, mod_comp, dat_mis, units, missing = "ml"),
    tolerance = 0.05
  )
})
