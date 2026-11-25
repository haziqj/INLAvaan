# The arguments and values of inlavaan() that INLAvaan 0.4.0 deprecates (see
# R/inlavaan-args.R). Each warns once per session, stops with the option
# inlavaan.deprecation = "error", and gives the fit of what replaces it. Each
# test forgets the deprecations it raises, so that the record of this session
# does not leak into other tests.

forget_deprecations <- function(ids) {
  rm(list = intersect(ids, ls(deprecation_seen)), envir = deprecation_seen)
}

hs <- lavaan::HolzingerSwineford1939
mod <- "visual =~ x1 + x2 + x3"
fit_hs <- function(..., silent = TRUE) {
  acfa(mod, hs, silent = silent, nsamp = 3, test = "none", ...)
}

# resolve_inlavaan_args() with the deprecated arguments in `...`
resolve_with <- function(
  ...,
  vb_correction = "onestep",
  marginal_method = "skewnorm",
  silent = FALSE
) {
  resolve_inlavaan_args(
    old = list(...),
    vb_correction = vb_correction,
    marginal_method = marginal_method,
    marginal_scan = "linear",
    scan_given = FALSE,
    control = list(),
    silent = silent
  )
}

# Each deprecated argument and value, by the id of its warning
old_calls <- list(
  n_qmc = list(n_qmc = 64L),
  vb_method = list(vb_method = "gauss_hermite"),
  samp_copula = list(samp_copula = FALSE),
  samp_norta = list(samp_norta = TRUE),
  sn_fit_temp = list(sn_fit_temp = 1),
  optim_method = list(optim_method = "ucminf"),
  numerical_grad = list(numerical_grad = TRUE),
  sn_fit_ngrid = list(sn_fit_ngrid = 31),
  sn_fit_logthresh = list(sn_fit_logthresh = -5),
  hessian_method = list(hessian_method = "forward"),
  debug = list(debug = TRUE),
  add_priors = list(add_priors = FALSE),
  `marginal_correction = "none"` = list(marginal_correction = "none"),
  `marginal_correction = "shortcut"` = list(marginal_correction = "shortcut"),
  `marginal_correction = "shortcut_fd"` = list(
    marginal_correction = "shortcut_fd"
  ),
  `marginal_correction = "hessian"` = list(marginal_correction = "hessian"),
  `marginal_method = "marggaus"` = list(marginal_method = "marggaus"),
  `marginal_method = "asymgaus"` = list(marginal_method = "asymgaus")
)

## ----- The warning -----------------------------------------------------------

test_that("each deprecated argument and value warns once per session", {
  for (id in names(old_calls)) {
    forget_deprecations(id)
    w <- expect_warning(
      do.call(resolve_with, old_calls[[id]]),
      class = "inlavaan_deprecated"
    )
    msg <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(w)))
    expect_match(msg, "stops from INLAvaan 0.5.0", fixed = TRUE)
    expect_true(isTRUE(deprecation_seen[[id]]), label = id)
    expect_no_warning(do.call(resolve_with, old_calls[[id]]))
    forget_deprecations(id)
  }
  # FALSE is marginal_correction = "none"
  id <- "marginal_correction = \"none\""
  forget_deprecations(id)
  expect_warning(
    resolve_with(marginal_correction = FALSE),
    class = "inlavaan_deprecated"
  )
  expect_true(isTRUE(deprecation_seen[[id]]))
  forget_deprecations(id)
})

test_that("the warning names what to use instead", {
  message_of <- function(id) {
    forget_deprecations(id)
    w <- expect_warning(do.call(resolve_with, old_calls[[id]]))
    forget_deprecations(id)
    gsub("\\s+", " ", cli::ansi_strip(conditionMessage(w)))
  }
  msg <- message_of("sn_fit_ngrid")
  expect_match(msg, "control = inlavaan_control(n_grid = )", fixed = TRUE)
  expect_match(msg, "gives the same fit", fixed = TRUE)
  msg <- message_of("hessian_method")
  expect_match(msg, "inlavaan_control(hessian = )", fixed = TRUE)
  msg <- message_of("n_qmc")
  expect_match(msg, "has no effect", fixed = TRUE)
  expect_match(msg, "INLAvaan 0.3.5 has the old method", fixed = TRUE)
  msg <- message_of("samp_copula")
  expect_match(msg, "marginal_method = \"gaussian\"", fixed = TRUE)
  msg <- message_of("marginal_correction = \"none\"")
  expect_match(msg, "marginal_scan = \"plain\"", fixed = TRUE)
  expect_match(msg, "gives the same fit", fixed = TRUE)
  msg <- message_of("marginal_correction = \"hessian\"")
  expect_match(msg, "marginal_scan = \"linear\"", fixed = TRUE)
  msg <- message_of("marginal_method = \"marggaus\"")
  expect_match(msg, "marginal_method = \"gaussian\"", fixed = TRUE)
  expect_match(msg, "gives the same fit", fixed = TRUE)
  msg <- message_of("marginal_method = \"asymgaus\"")
  expect_match(msg, "now runs `marginal_method = \"skewnorm\"`", fixed = TRUE)
})

test_that("with the option inlavaan.deprecation = \"error\" they stop", {
  old_opt <- options(inlavaan.deprecation = "error")
  for (id in names(old_calls)) {
    forget_deprecations(id)
    for (i in 1:2) {
      expect_error(
        do.call(resolve_with, old_calls[[id]]),
        class = "inlavaan_deprecated"
      )
    }
    expect_null(deprecation_seen[[id]])
  }
  # Also in a fit, silent or not
  expect_error(fit_hs(n_qmc = 64), class = "inlavaan_deprecated")
  expect_error(
    fit_hs(marginal_method = "marggaus", silent = FALSE),
    class = "inlavaan_deprecated"
  )
  options(old_opt)
})

test_that("a deprecation in a silent fit is recorded, not shown or marked", {
  forget_deprecations("n_qmc")
  expect_silent(fit <- fit_hs(n_qmc = 64, marginal_method = "gaussian"))
  expect_match(
    get_inlavaan_internal(fit, "warnings"),
    "`n_qmc` is deprecated",
    fixed = TRUE,
    all = FALSE
  )
  expect_null(deprecation_seen[["n_qmc"]])
  # The next fit that is not silent shows it
  expect_warning(
    suppressMessages(fit_hs(
      n_qmc = 64,
      marginal_method = "gaussian",
      silent = FALSE
    )),
    class = "inlavaan_deprecated"
  )
  expect_true(deprecation_seen[["n_qmc"]])
  forget_deprecations("n_qmc")
})

## ----- What the old calls give -------------------------------------------------

test_that("an old call gives the fit of the new call it maps to", {
  expect_same_fit <- function(old_args, new_args) {
    set.seed(1)
    a <- do.call(fit_hs, old_args)
    set.seed(1)
    b <- do.call(fit_hs, new_args)
    ia <- get_inlavaan_internal(a)
    ib <- get_inlavaan_internal(b)
    for (field in c("coefficients", "summary", "approx_data", "Omega")) {
      expect_identical(ia[[field]], ib[[field]], label = field)
    }
    expect_identical(ia$marginal_method, ib$marginal_method)
  }
  # Moved into control
  expect_same_fit(list(sn_fit_ngrid = 31), list(control = list(n_grid = 31)))
  expect_same_fit(
    list(sn_fit_logthresh = -5),
    list(control = inlavaan_control(log_threshold = -5))
  )
  expect_same_fit(
    list(hessian_method = "forward"),
    list(control = list(hessian = "forward"))
  )
  expect_same_fit(
    list(add_priors = FALSE, marginal_method = "gaussian"),
    list(control = list(add_priors = FALSE), marginal_method = "gaussian")
  )
  int <- fit_hs(debug = TRUE, marginal_method = "gaussian")
  expect_s3_class(int, "inlavaan_internal")
  # Old values
  expect_same_fit(
    list(marginal_correction = "none"),
    list(marginal_scan = "plain")
  )
  expect_same_fit(
    list(marginal_correction = FALSE),
    list(marginal_scan = "plain")
  )
  expect_same_fit(list(marginal_correction = "hessian"), list())
  expect_same_fit(
    list(marginal_method = "marggaus"),
    list(marginal_method = "gaussian")
  )
  expect_same_fit(list(marginal_method = "asymgaus"), list())
  # Ended arguments have no effect
  expect_same_fit(
    list(
      n_qmc = 32,
      vb_method = "sobol",
      samp_copula = FALSE,
      samp_norta = TRUE,
      sn_fit_temp = 2,
      optim_method = "optim",
      numerical_grad = TRUE
    ),
    list()
  )
  int <- get_inlavaan_internal(fit_hs(numerical_grad = TRUE))
  expect_true(int$engine$has_grad)
  expect_null(int$R_star)
  expect_identical(int$optim_method, "nlminb")
})

test_that("TRUE, FALSE and \"skewnormal\" are silent aliases", {
  args <- function(vb_correction, marginal_method) {
    resolve_with(
      vb_correction = vb_correction,
      marginal_method = marginal_method
    )
  }
  expect_no_warning(a <- args(TRUE, "skewnormal"))
  expect_identical(a$vb_correction, "onestep")
  expect_identical(a$marginal_method, "skewnorm")
  expect_no_warning(a <- args(FALSE, "Gauss"))
  expect_identical(a$vb_correction, "none")
  expect_identical(a$marginal_method, "gaussian")
  expect_identical(args("full", "skew")$vb_correction, "full")
  # The defaults are the first of each
  a <- resolve_inlavaan_args(
    old = list(),
    vb_correction = c("onestep", "full", "none"),
    marginal_method = c("skewnorm", "gaussian"),
    marginal_scan = c("linear", "exact", "laplace", "plain"),
    scan_given = FALSE,
    control = list()
  )
  expect_identical(
    a[c("vb_correction", "marginal_method", "marginal_scan")],
    list(
      vb_correction = "onestep",
      marginal_method = "skewnorm",
      marginal_scan = "linear"
    )
  )
  expect_identical(a$control, list())
  # An abbreviation of an old value is the old value
  forget_deprecations("marginal_method = \"marggaus\"")
  expect_warning(a <- args(TRUE, "marg"), class = "inlavaan_deprecated")
  expect_identical(a$marginal_method, "gaussian")
  forget_deprecations("marginal_method = \"marggaus\"")
  # Errors name the argument of inlavaan()
  expect_error(args(TRUE, "foo"), "marginal_method")
  expect_error(args(NA, "skewnorm"), "vb_correction")
  expect_error(fit_hs(marginal_scan = "wide"), "marginal_scan")
})

test_that("marginal_method = \"sampling\" is not available", {
  err <- expect_error(
    fit_hs(marginal_method = "sampling"),
    class = "inlavaan_unavailable"
  )
  msg <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err)))
  expect_match(msg, "under development", fixed = TRUE)
  expect_match(msg, "marginal_method = \"gaussian\"", fixed = TRUE)
  expect_error(fit_hs(marginal_method = "samp"), class = "inlavaan_unavailable")
})

test_that("an old argument and what replaces it stop together", {
  err <- expect_error(
    fit_hs(sn_fit_ngrid = 31, control = list(n_grid = 31)),
    "not both"
  )
  expect_match(conditionMessage(err), "sn_fit_ngrid", fixed = TRUE)
  expect_error(
    fit_hs(marginal_correction = "none", marginal_scan = "plain"),
    "not both"
  )
  # marginal_scan given at its default counts too
  expect_error(
    fit_hs(marginal_correction = "none", marginal_scan = "linear"),
    "not both"
  )
  # A bad value names the old argument
  expect_error(fit_hs(sn_fit_ngrid = 3), "sn_fit_ngrid")
  expect_error(fit_hs(sn_fit_logthresh = 0), "sn_fit_logthresh")
  expect_error(fit_hs(hessian_method = "backward"), "hessian_method")
  expect_error(fit_hs(add_priors = NA), "add_priors")
  expect_error(fit_hs(marginal_correction = "exact"), "marginal_correction")
})

## ----- The functions after the fit ---------------------------------------------

test_that("samp_copula after the fit warns and has no effect", {
  fit <- acfa(
    "visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6",
    hs,
    silent = TRUE,
    nsamp = 5,
    test = "dic",
    marginal_method = "gaussian",
    vb_correction = "none"
  )
  forget_deprecations("samp_copula")
  set.seed(1)
  expect_warning(
    a <- sampling(fit, nsamp = 5, samp_copula = FALSE),
    class = "inlavaan_deprecated"
  )
  set.seed(1)
  expect_identical(a, sampling(fit, nsamp = 5))
  forget_deprecations("samp_copula")
  expect_warning(
    sampling(get_inlavaan_internal(fit), nsamp = 3, samp_copula = TRUE),
    class = "inlavaan_deprecated"
  )
  forget_deprecations("samp_copula")
  expect_warning(
    simulate(fit, nsim = 1, samp_copula = FALSE),
    class = "inlavaan_deprecated"
  )
  forget_deprecations("samp_copula")
  expect_warning(
    bfit_indices(fit, baseline.model = FALSE, samp_copula = FALSE),
    class = "inlavaan_deprecated"
  )
  forget_deprecations("samp_copula")
})

## ----- The wrappers and the help -----------------------------------------------

test_that("the wrappers stop when given model.type", {
  for (fn in c("acfa", "asem", "agrowth")) {
    f <- get(fn)
    err <- expect_error(
      f(mod, hs, model.type = "growth"),
      class = "inlavaan_wrapper_model_type"
    )
    expect_match(conditionMessage(err), fn, fixed = TRUE)
    expect_error(
      f(mod, hs, model.t = "growth"),
      class = "inlavaan_wrapper_model_type"
    )
  }
  # model = is not model.type
  fit <- acfa(
    model = mod,
    data = hs,
    silent = TRUE,
    nsamp = 3,
    test = "none",
    marginal_method = "gaussian"
  )
  expect_identical(get_inlavaan_internal(fit, "call")$model.type, "cfa")
  # The wrappers take the arguments of inlavaan() but model.type and start
  fmls <- as.list(formals(inlavaan))
  wrapper_fmls <- fmls[setdiff(names(fmls), c("model.type", "start"))]
  expect_identical(as.list(formals(acfa)), wrapper_fmls)
  expect_identical(as.list(formals(asem)), wrapper_fmls)
  expect_identical(as.list(formals(agrowth)), wrapper_fmls)
})

test_that("the help marks each deprecated argument with the badge", {
  rd_items <- function(topic) {
    path <- test_path("..", "..", "man", paste0(topic, ".Rd"))
    skip_if_not(file.exists(path), "needs the source tree")
    rd <- tools::parse_Rd(path)
    tags <- vapply(rd, function(x) attr(x, "Rd_tag"), "")
    args <- rd[[which(tags == "\\arguments")]]
    items <- Filter(function(x) identical(attr(x, "Rd_tag"), "\\item"), args)
    text <- vapply(
      items,
      function(it) paste(unlist(it[[2]]), collapse = ""),
      ""
    )
    names(text) <- vapply(
      items,
      function(it) paste(unlist(it[[1]]), collapse = ""),
      ""
    )
    text
  }
  badge <- "lifecycle-deprecated.svg"
  text <- rd_items("inlavaan")
  deprecated <- c(
    names(inlavaan_moved_args),
    names(inlavaan_ended_args),
    "marginal_correction"
  )
  expect_length(deprecated, 13L)
  for (arg in deprecated) {
    expect_match(text[[arg]], badge, fixed = TRUE, label = arg)
  }
  # The old values of marginal_method
  expect_match(text[["marginal_method"]], badge, fixed = TRUE)
  for (arg in c("vb_correction", "marginal_scan", "silent", "control")) {
    expect_no_match(text[[arg]], badge, fixed = TRUE)
  }
  for (topic in c("sampling", "simulate", "bfit_indices")) {
    expect_match(rd_items(topic)[["samp_copula"]], badge, fixed = TRUE)
  }
})
