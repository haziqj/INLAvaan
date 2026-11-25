# inlavaan_control() and the `control` argument of inlavaan(). capture_engine()
# fits and records what inlavaan() gives vanilla_core().

capture_engine <- function(expr) {
  seen <- new.env()
  local_mocked_bindings(
    engine_call = function(...) {
      seen$args <- list(...)
      seen$core <- vanilla_core(...)
      seen$core
    }
  )
  seen$fit <- expr
  seen
}

hs <- lavaan::HolzingerSwineford1939
fit_with <- function(..., silent = TRUE) {
  capture_engine(acfa(
    "visual =~ x1 + x2 + x3",
    hs,
    silent = silent,
    nsamp = 3,
    test = "none",
    vb_correction = "none",
    marginal_method = "gaussian",
    ...
  ))
}

# Every warning of `expr`, muffled
collect_warnings <- function(expr) {
  ws <- list()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      ws[[length(ws) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = ws)
}

forget_deprecations <- function(ids) {
  rm(list = intersect(ids, ls(deprecation_seen)), envir = deprecation_seen)
}

## ----- The constructor ---------------------------------------------------------

test_that("inlavaan_control() returns the entries given, checked", {
  expect_identical(inlavaan_control(), list())
  expect_identical(inlavaan_control(n_grid = 31), list(n_grid = 31L))
  # In the order of the arguments
  expect_identical(
    inlavaan_control(vb_tol = 1e-4, mode_maxit = 100.4),
    list(mode_maxit = 100L, vb_tol = 1e-4)
  )
  expect_identical(
    inlavaan_control(add_priors = FALSE, hessian = "forward", debug = TRUE),
    list(hessian = "forward", debug = TRUE, add_priors = FALSE)
  )
  expect_identical(
    inlavaan_control(nlminb = list(trace = 1)),
    list(nlminb = list(trace = 1))
  )
  # The result passes the same check again unchanged
  ctrl <- inlavaan_control(mode_tol = 1e-8, log_threshold = -5, debug = TRUE)
  expect_identical(do.call(inlavaan_control, ctrl), ctrl)
})

test_that("the defaults are those of the engine and of inlavaan()", {
  fmls <- as.list(formals(inlavaan_control))
  engine <- as.list(formals(vanilla_control))
  expect_identical(fmls[names(engine)], engine)
  expect_identical(fmls$hessian, quote(c("central", "forward")))
  expect_false(fmls$debug)
  expect_true(fmls$add_priors)
  expect_identical(
    names(fmls),
    c(names(engine), "hessian", "debug", "add_priors")
  )
})

test_that("a twin in nlminb stands in for its entry, and keeps its value", {
  values <- list(mode_maxit = 10, mode_maxeval = 20, mode_tol = 1e-8)
  for (entry in names(nlminb_twins)) {
    twin <- nlminb_twins[[entry]]
    ctrl <- inlavaan_control(
      nlminb = stats::setNames(list(values[[entry]], 1), c(twin, "trace"))
    )
    expected <- if (entry == "mode_tol") 1e-8 else as.integer(values[[entry]])
    expect_identical(ctrl[[entry]], expected)
    expect_identical(ctrl$nlminb, list(trace = 1))
    # Checked again, by INLAvaan or by the engine, it keeps its value
    expect_identical(do.call(inlavaan_control, ctrl), ctrl)
    expect_identical(check_control(ctrl)[[entry]], expected)
    # The entry and its twin together stop
    both <- stats::setNames(list(values[[entry]]), entry)
    both$nlminb <- stats::setNames(list(values[[entry]]), twin)
    expect_error(do.call(inlavaan_control, both), "not both")
  }
  ce <- fit_with(control = inlavaan_control(nlminb = list(iter.max = 3)))
  expect_identical(ce$args$control$mode_maxit, 3L)
  expect_identical(check_control(ce$args$control)$mode_maxit, 3L)
})

test_that("a bad entry stops with the name of the entry", {
  for (entry in c("mode_maxit", "mode_maxeval", "vb_maxit")) {
    for (bad in list(-1, 0, NA, Inf, "10", c(10, 20))) {
      expect_error(
        do.call(inlavaan_control, stats::setNames(list(bad), entry)),
        entry,
        fixed = TRUE
      )
    }
  }
  for (entry in c("mode_tol", "vb_tol")) {
    for (bad in list(-1, 0, NA, "a")) {
      expect_error(
        do.call(inlavaan_control, stats::setNames(list(bad), entry)),
        entry,
        fixed = TRUE
      )
    }
  }
  expect_error(inlavaan_control(n_grid = 6), "n_grid")
  expect_error(inlavaan_control(log_threshold = 0), "log_threshold")
  expect_error(inlavaan_control(nlminb = "a"), "nlminb")
  expect_error(inlavaan_control(nlminb = list(1)), "nlminb")
  expect_error(inlavaan_control(hessian = "exact"), "hessian")
  expect_error(inlavaan_control(debug = NA), "debug")
  expect_error(inlavaan_control(add_priors = "yes"), "add_priors")
})

## ----- The control of inlavaan() -----------------------------------------------

test_that("control takes inlavaan_control() or a plain list", {
  ce <- fit_with(control = inlavaan_control(n_grid = 31, vb_maxit = 10))
  expect_identical(ce$args$control$n_grid, 31L)
  expect_identical(ce$args$control$vb_maxit, 10L)
  ce <- fit_with(control = list(n_grid = 31, log_threshold = -5))
  expect_identical(ce$args$control$n_grid, 31L)
  expect_identical(ce$args$control$log_threshold, -5)
  # The deprecated argument of inlavaan() gives the grid too
  ce <- fit_with(sn_fit_ngrid = 9)
  expect_identical(ce$args$control$n_grid, 9L)
  for (bad in list("a", list(1), list(n_grid = 31, 9))) {
    expect_error(fit_with(control = bad), "control")
  }
  err <- expect_error(fit_with(control = list(n_gird = 31)), "Unknown")
  expect_match(conditionMessage(err), "n_gird", fixed = TRUE)
  expect_match(conditionMessage(err), "inlavaan_control", fixed = TRUE)
})

test_that("an entry does what the argument of inlavaan() does, but not both", {
  # hessian: "forward" gives the engine a Hessian at the mode
  expect_null(fit_with()$args$hess_mode)
  ce <- fit_with(control = inlavaan_control(hessian = "forward"))
  expect_true(is.function(ce$args$hess_mode))
  # debug returns the internal list
  int <- acfa(
    "visual =~ x1 + x2 + x3",
    hs,
    silent = TRUE,
    nsamp = 3,
    test = "none",
    control = list(debug = TRUE)
  )
  expect_s3_class(int, "inlavaan_internal")
  # add_priors = FALSE gives the mode of the argument
  mode_of <- function(...) {
    get_inlavaan_internal(fit_with(...)$fit, "theta_star_novbc")
  }
  expect_identical(
    mode_of(control = list(add_priors = FALSE)),
    mode_of(add_priors = FALSE)
  )
  # Each entry and its argument together stop, before the model is built
  pairs <- list(
    list("n_grid", 31, "sn_fit_ngrid", 31),
    list("log_threshold", -5, "sn_fit_logthresh", -5),
    list("hessian", "forward", "hessian_method", "forward"),
    list("debug", TRUE, "debug", TRUE),
    list("add_priors", FALSE, "add_priors", FALSE)
  )
  for (p in pairs) {
    args <- list(control = stats::setNames(list(p[[2]]), p[[1]]))
    args[[p[[3]]]] <- p[[4]]
    err <- expect_error(do.call(fit_with, args), "not both")
    expect_match(conditionMessage(err), p[[3]], fixed = TRUE)
  }
})

test_that("the controls of nlminb() still work in control, with a warning", {
  ids <- paste0("control$", c("iter.max", "trace", "eval.max", "rel.tol"))
  forget_deprecations(ids)
  res <- collect_warnings(suppressMessages(fit_with(
    control = list(iter.max = 500, trace = 0),
    silent = FALSE
  )))
  ce <- res$value
  expect_identical(ce$args$control$mode_maxit, 500L)
  expect_identical(ce$args$control$nlminb, list(trace = 0))
  dep <- Filter(function(w) inherits(w, "inlavaan_deprecated"), res$warnings)
  expect_length(dep, 2L)
  msgs <- cli::ansi_strip(vapply(dep, conditionMessage, ""))
  expect_match(msgs[[1]], "control$iter.max", fixed = TRUE)
  expect_match(msgs[[1]], "inlavaan_control(mode_maxit = )", fixed = TRUE)
  expect_match(
    msgs[[2]],
    "inlavaan_control(nlminb = list(trace = ))",
    fixed = TRUE
  )
  # Once per session
  expect_no_warning(
    suppressMessages(fit_with(control = list(iter.max = 500), silent = FALSE)),
    class = "inlavaan_deprecated"
  )
  # An error with options(inlavaan.deprecation = "error")
  old_opt <- options(inlavaan.deprecation = "error")
  expect_error(
    fit_with(control = list(iter.max = 500)),
    class = "inlavaan_deprecated"
  )
  options(old_opt)
  # A silent fit records the warning but does not mark it as seen
  fit <- fit_with(control = list(eval.max = 500))$fit
  expect_match(
    get_inlavaan_internal(fit, "warnings"),
    "control$eval.max",
    fixed = TRUE,
    all = FALSE
  )
  expect_null(deprecation_seen[["control$eval.max"]])
  # The old name and the new one, or its twin in nlminb, together stop
  expect_error(
    fit_with(control = list(rel.tol = 1e-8, nlminb = list(rel.tol = 1e-8))),
    "not both"
  )
  expect_error(
    fit_with(control = list(rel.tol = 1e-8, mode_tol = 1e-8)),
    "not both"
  )
  forget_deprecations(ids)
})

test_that("an old name and the entry it sets stop, in the names given", {
  for (old in nlminb_twins) {
    entry <- names(nlminb_twins)[nlminb_twins == old]
    ctrl <- stats::setNames(list(10, 10), c(old, entry))
    # Before the move, so with no deprecation warning
    expect_no_warning(err <- expect_error(resolve_control(ctrl), "not both"))
    msg <- cli::ansi_strip(conditionMessage(err))
    expect_match(msg, paste0("control$", old), fixed = TRUE)
    expect_match(msg, paste0("control$", entry), fixed = TRUE)
    expect_no_match(msg, "nlminb", fixed = TRUE)
  }
  # A bad value of an old name says which old name it was
  err <- expect_error(
    suppressWarnings(resolve_control(list(iter.max = -1), silent = TRUE)),
    "mode_maxit"
  )
  expect_match(
    cli::ansi_strip(conditionMessage(err)),
    "control$iter.max",
    fixed = TRUE
  )
  # Other bad values keep the error of inlavaan_control()
  expect_error(
    suppressWarnings(resolve_control(
      list(trace = 1, n_grid = 3),
      silent = TRUE
    )),
    "n_grid"
  )
})
