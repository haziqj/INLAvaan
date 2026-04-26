test_that("Native LISREL backend matches lavaan implied covariance and loglik", {
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "

  fit0 <- INLAvaan:::with_safe_detectCores(
    lavaan::cfa(mod, dat, do.fit = FALSE, parser = "old", test = "none", verbose = FALSE)
  )
  backend <- INLAvaan:::native_lisrel_backend_extract(fit0)
  x_free <- lavaan:::lav_model_get_parameters(fit0@Model, type = "free")

  sigma_native <- INLAvaan:::cpp_lisrel_implied_cov(backend, x_free)
  sigma_lavaan <- lavaan:::lav_model_implied(fit0@Model)$cov[[1]]
  expect_equal(sigma_native, sigma_lavaan, tolerance = 1e-10)

  ll_native <- INLAvaan:::cpp_lisrel_loglik(backend, x_free)
  ll_ref <- INLAvaan:::inlav_model_loglik(
    x_free,
    fit0@Model,
    fit0@SampleStats,
    fit0@Data,
    fit0@Options,
    fit0@Cache
  )
  expect_equal(ll_native, ll_ref, tolerance = 1e-8)
})

test_that("Native LISREL gradient matches finite differences", {
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "

  fit0 <- INLAvaan:::with_safe_detectCores(
    lavaan::cfa(mod, dat, do.fit = FALSE, parser = "old", test = "none", verbose = FALSE)
  )
  backend <- INLAvaan:::native_lisrel_backend_extract(fit0)
  x_free <- lavaan:::lav_model_get_parameters(fit0@Model, type = "free")

  grad_native <- INLAvaan:::cpp_lisrel_grad(backend, x_free)
  grad_fd <- INLAvaan:::cpp_fast_grad(function(x) INLAvaan:::cpp_lisrel_loglik(backend, x), x_free)

  expect_equal(grad_native, grad_fd, tolerance = 1e-5)
})

test_that("Native theta-space Hessian matches finite-difference gradient Jacobian", {
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "

  int <- acfa(
    mod,
    dat,
    std.lv = TRUE,
    verbose = FALSE,
    test = "none",
    nsamp = 5,
    vb_correction = FALSE,
    marginal_method = "marggaus",
    add_priors = FALSE,
    debug = TRUE
  )

  pt <- int$partable
  backend <- int$native_backend
  ptfreeidx <- which(pt$free > 0L & !duplicated(pt$free))
  transforms <- INLAvaan:::native_lisrel_theta_transforms(pt, ptfreeidx)
  theta_star <- int$theta_star_novbc

  neg_grad_lik <- function(pars) {
    x <- INLAvaan:::pars_to_x(pars, pt)
    jcb_vec <- mapply(function(f, x) f(x), pt$ginv_prime[pt$free > 0], pars)
    gll <- INLAvaan:::native_model_grad(x, backend)
    sd1sd2 <- attr(x, "sd1sd2")
    jcb_mat <- attr(x, "jcb_mat")
    gll_th <- jcb_vec * sd1sd2 * gll
    if (!is.null(jcb_mat) && NROW(jcb_mat) > 0) {
      for (k in seq_len(nrow(jcb_mat))) {
        gll_th[jcb_mat[k, 1]] <- gll_th[jcb_mat[k, 1]] + jcb_mat[k, 3] * gll[jcb_mat[k, 2]]
      }
    }
    -1 * as.numeric(gll_th)
  }

  H_cpp <- INLAvaan:::cpp_lisrel_hessian_theta(
    backend,
    theta_star,
    transforms,
    int$cov_var_idx1,
    int$cov_var_idx2
  )
  H_fd <- INLAvaan:::fast_jacobian(neg_grad_lik, theta_star)

  expect_equal(H_cpp, H_fd, tolerance = 1e-2)
})

test_that("Partable-built native backend matches lavModel-built backend", {
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "

  fit0 <- INLAvaan:::with_safe_detectCores(
    lavaan::cfa(mod, dat, do.fit = FALSE, parser = "old", test = "none", verbose = FALSE)
  )
  backend_model <- INLAvaan:::native_lisrel_backend_extract(fit0)
  backend_pt <- INLAvaan:::native_lisrel_backend_extract_partable(
    lavpartable = fit0@ParTable,
    lavsamplestats = fit0@SampleStats,
    lavdata = fit0@Data,
    lavoptions = fit0@Options
  )
  x_free <- lavaan:::lav_model_get_parameters(fit0@Model, type = "free")

  expect_equal(backend_pt$x_free_start, backend_model$x_free_start, tolerance = 1e-10)
  expect_equal(
    INLAvaan:::cpp_lisrel_loglik(backend_pt, x_free),
    INLAvaan:::cpp_lisrel_loglik(backend_model, x_free),
    tolerance = 1e-10
  )
  expect_equal(
    INLAvaan:::cpp_lisrel_grad(backend_pt, x_free),
    INLAvaan:::cpp_lisrel_grad(backend_model, x_free),
    tolerance = 1e-10
  )
})

test_that("Native LISREL mean structure matches lavaan", {
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "

  fit0 <- INLAvaan:::with_safe_detectCores(
    lavaan::cfa(
      mod, dat,
      meanstructure = TRUE,
      do.fit = FALSE, parser = "old", test = "none", verbose = FALSE
    )
  )
  backend <- INLAvaan:::native_lisrel_backend_extract(fit0)
  x_free <- lavaan:::lav_model_get_parameters(fit0@Model, type = "free")

  mu_native <- INLAvaan:::cpp_lisrel_implied_mean(backend, x_free)
  mu_lavaan <- lavaan:::lav_model_implied(fit0@Model)$mean[[1]]
  expect_equal(as.numeric(mu_native), as.numeric(mu_lavaan), tolerance = 1e-10)

  ll_native <- INLAvaan:::cpp_lisrel_loglik(backend, x_free)
  ll_ref <- INLAvaan:::inlav_model_loglik(
    x_free,
    fit0@Model,
    fit0@SampleStats,
    fit0@Data,
    fit0@Options,
    fit0@Cache
  )
  expect_equal(ll_native, ll_ref, tolerance = 1e-8)

  grad_native <- INLAvaan:::cpp_lisrel_grad(backend, x_free)
  grad_fd <- INLAvaan:::cpp_fast_grad(
    function(x) INLAvaan:::cpp_lisrel_loglik(backend, x),
    x_free
  )
  expect_equal(grad_native, grad_fd, tolerance = 1e-5)
})

test_that("Native latent posterior matches the R single-level formula", {
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "

  fit <- acfa(
    mod,
    dat,
    verbose = FALSE,
    test = "none",
    nsamp = 5,
    vb_correction = FALSE,
    marginal_method = "marggaus"
  )

  int <- fit@external$inlavaan_internal
  backend <- int$native_backend
  backend$groups[[1L]]$Y <- as.matrix(int$lavdata@X[[1L]])

  lat_post_cpp <- INLAvaan:::cpp_lisrel_latent_posterior(
    backend,
    as.numeric(int$theta_star)
  )

  glist <- INLAvaan:::get_SEM_param_matrix(
    as.numeric(int$theta_star),
    "all",
    int$lavmodel
  )[[1L]]
  Lambda <- glist$lambda
  Psi <- glist$psi
  Theta <- glist$theta
  B <- glist$beta
  alpha <- glist$alpha
  if (is.null(alpha)) {
    alpha <- 0
  }

  if (is.null(B)) {
    Phi <- Psi
    front <- Lambda
  } else {
    IminB_inv <- solve(diag(nrow(B)) - B)
    Phi <- IminB_inv %*% Psi %*% t(IminB_inv)
    front <- Lambda %*% IminB_inv
  }

  Sigmay_inv <- solve(front %*% Psi %*% t(front) + Theta)
  PhiLtSinv <- Phi %*% t(Lambda) %*% Sigmay_inv
  mu_eta_r <- t(as.numeric(alpha) + PhiLtSinv %*% t(int$lavdata@X[[1L]]))
  V_eta_r <- Phi - PhiLtSinv %*% Lambda %*% Phi

  expect_equal(
    unname(lat_post_cpp[[1L]]$eta_mean),
    unname(mu_eta_r),
    tolerance = 1e-8
  )
  expect_equal(
    unname(lat_post_cpp[[1L]]$eta_cov[[1L]]),
    unname(V_eta_r),
    tolerance = 1e-8
  )
})

test_that("Native single-level yhat matches the R formula for fixed RNG seed", {
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "

  fit <- acfa(
    mod,
    dat,
    verbose = FALSE,
    test = "none",
    nsamp = 5,
    vb_correction = FALSE,
    marginal_method = "marggaus"
  )

  int <- fit@external$inlavaan_internal
  backend <- int$native_backend
  backend$groups[[1L]]$Y <- as.matrix(int$lavdata@X[[1L]])

  glist <- INLAvaan:::get_SEM_param_matrix(
    as.numeric(int$theta_star),
    "all",
    int$lavmodel
  )[[1L]]
  Lambda <- glist$lambda
  Psi <- glist$psi
  Theta <- glist$theta
  B <- glist$beta
  alpha <- glist$alpha
  nu <- glist$nu
  if (is.null(alpha)) alpha <- rep(0, ncol(Lambda))
  if (is.null(nu)) nu <- rep(0, nrow(Lambda))

  if (is.null(B)) {
    Phi <- Psi
    front <- Lambda
  } else {
    IminB_inv <- solve(diag(nrow(B)) - B)
    Phi <- IminB_inv %*% Psi %*% t(IminB_inv)
    front <- Lambda %*% IminB_inv
  }

  Sigmay_inv <- solve(front %*% Psi %*% t(front) + Theta)
  PhiLtSinv <- Phi %*% t(Lambda) %*% Sigmay_inv
  mu_eta <- t(as.numeric(alpha) + PhiLtSinv %*% t(int$lavdata@X[[1L]]))
  V_eta <- Phi - PhiLtSinv %*% Lambda %*% Phi
  chol_V <- tryCatch(
    t(chol(V_eta)),
    error = function(e) diag(sqrt(pmax(diag(V_eta), 0)), ncol(V_eta))
  )
  n_obs <- nrow(mu_eta)
  nlv <- ncol(mu_eta)

  set.seed(123)
  Z <- matrix(rnorm(n_obs * nlv), nrow = nlv, ncol = n_obs)
  eta_draw <- mu_eta + t(chol_V %*% Z)
  yhat_r <- if (is.null(B)) {
    sweep(tcrossprod(eta_draw, Lambda), 2, as.numeric(nu), "+")
  } else {
    sweep(
      tcrossprod(eta_draw %*% t(IminB_inv), Lambda),
      2,
      as.numeric(nu),
      "+"
    )
  }

  set.seed(123)
  yhat_cpp <- INLAvaan:::cpp_lisrel_predict_y(
    backend,
    as.numeric(int$theta_star),
    add_noise = FALSE
  )[[1L]]

  expect_equal(unname(yhat_cpp), unname(yhat_r), tolerance = 1e-8)
})

test_that("Native single-level missing imputation matches the R formula for fixed RNG seed", {
  dat <- lavaan::HolzingerSwineford1939
  set.seed(42)
  dat$x1[sample(nrow(dat), 12)] <- NA
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "

  fit <- acfa(
    mod,
    dat,
    verbose = FALSE,
    test = "none",
    nsamp = 5,
    vb_correction = FALSE,
    marginal_method = "marggaus"
  )

  int <- fit@external$inlavaan_internal
  backend <- int$native_backend
  backend$groups[[1L]]$Y <- as.matrix(int$lavdata@X[[1L]])

  Sigma_y <- INLAvaan:::cpp_lisrel_implied_cov(
    list(type = backend$type, groups = list(backend$groups[[1L]]), x_free_start = backend$x_free_start),
    as.numeric(int$theta_star)
  )
  mu_y <- as.numeric(INLAvaan:::cpp_lisrel_implied_mean(
    list(type = backend$type, groups = list(backend$groups[[1L]]), x_free_start = backend$x_free_start),
    as.numeric(int$theta_star)
  ))
  yg <- int$lavdata@X[[1L]]
  out_r <- yg
  na_mat <- is.na(yg)
  patterns <- apply(na_mat, 1, function(r) paste(which(r), collapse = ","))
  unique_patterns <- unique(patterns[patterns != ""])

  set.seed(123)
  for (pat in unique_patterns) {
    mis_idx <- as.integer(strsplit(pat, ",")[[1]])
    obs_idx <- setdiff(seq_len(ncol(yg)), mis_idx)
    case_rows <- which(patterns == pat)
    Sigma_oo <- Sigma_y[obs_idx, obs_idx, drop = FALSE]
    Sigma_mo <- Sigma_y[mis_idx, obs_idx, drop = FALSE]
    Sigma_mm <- Sigma_y[mis_idx, mis_idx, drop = FALSE]
    A <- Sigma_mo %*% solve(Sigma_oo)
    Sigma_cond <- Sigma_mm - A %*% t(Sigma_mo)
    Sigma_cond <- (Sigma_cond + t(Sigma_cond)) / 2
    chol_cond <- t(chol(Sigma_cond))
    y_obs_centred <- yg[case_rows, obs_idx, drop = FALSE] -
      matrix(mu_y[obs_idx], nrow = length(case_rows), ncol = length(obs_idx), byrow = TRUE)
    mu_cond <- matrix(
      mu_y[mis_idx],
      nrow = length(case_rows),
      ncol = length(mis_idx),
      byrow = TRUE
    ) + y_obs_centred %*% t(A)
    Z <- matrix(rnorm(length(case_rows) * length(mis_idx)), nrow = length(mis_idx), ncol = length(case_rows))
    out_r[case_rows, mis_idx] <- mu_cond + t(chol_cond %*% Z)
  }

  set.seed(123)
  out_cpp <- INLAvaan:::cpp_lisrel_impute_missing(
    backend,
    as.numeric(int$theta_star)
  )[[1L]]

  expect_equal(out_cpp, out_r, tolerance = 1e-8)
})

test_that("Native multilevel missing imputation matches the R predict path", {
  dat <- lavaan::Demo.twolevel
  dat <- dat[dat$cluster <= 10, ]
  mod <- "
    level: 1
      fw =~ y1 + y2 + y3
      xw =~ x1 + x2 + x3
    level: 2
      fb =~ y1 + y2 + y3
      xb =~ x1 + x2 + x3
      fb ~ w1 + w2
      xb ~ w1 + w2
  "

  fit <- asem(
    mod,
    dat,
    cluster = "cluster",
    verbose = FALSE,
    test = "none",
    nsamp = 3,
    vb_correction = FALSE,
    marginal_correction = "none"
  )

  fit_native <- fit
  fit_r <- fit

  int_native <- fit_native@external$inlavaan_internal
  int_r <- fit_r@external$inlavaan_internal

  j_y1 <- match("y1", int_native$lavdata@ov.names[[1L]])
  int_native$lavdata@X[[1L]][1:10, j_y1] <- NA_real_
  int_r$lavdata@X[[1L]][1:10, j_y1] <- NA_real_
  int_r$native_backend <- NULL

  fit_native@external$inlavaan_internal <- int_native
  fit_r@external$inlavaan_internal <- int_r

  set.seed(123)
  pred_native <- predict(fit_native, type = "ymis", nsamp = 1)
  set.seed(123)
  pred_r <- predict(fit_r, type = "ymis", nsamp = 1)

  expect_equal(pred_native[[1L]], pred_r[[1L]], tolerance = 1e-8)
})

test_that("Native skew-normal sampling matches explicit R sampling for ceq.simple models", {
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "

  fit <- acfa(
    mod,
    dat,
    group = "school",
    group.equal = "loadings",
    verbose = FALSE,
    test = "none",
    nsamp = 5,
    vb_correction = FALSE
  )

  int <- fit@external$inlavaan_internal
  expect_equal(length(int$native_theta_transforms), length(int$theta_star))
  expect_equal(length(int$cov_var_idx1), length(int$theta_star))
  expect_equal(length(int$cov_var_idx2), length(int$theta_star))

  set.seed(1)
  samp_native <- INLAvaan:::sample_params(
    theta_star = int$theta_star,
    Sigma_theta = int$Sigma_theta,
    method = int$marginal_method,
    approx_data = int$approx_data,
    pt = int$partable,
    lavmodel = int$lavmodel,
    nsamp = 200L,
    R_star = int$R_star,
    integration_data = int$inla_integration %||% NULL,
    native_theta_transforms = int$native_theta_transforms,
    cov_var_idx1 = int$cov_var_idx1,
    cov_var_idx2 = int$cov_var_idx2,
    nthreads = 1L
  )

  old_backend <- getOption("inlavaan.backend")
  old_force_r <- getOption("inlavaan.force_r_path")
  on.exit(
    options(inlavaan.backend = old_backend, inlavaan.force_r_path = old_force_r),
    add = TRUE
  )
  options(inlavaan.backend = "r", inlavaan.force_r_path = TRUE)

  set.seed(1)
  samp_r <- INLAvaan:::sample_params(
    theta_star = int$theta_star,
    Sigma_theta = int$Sigma_theta,
    method = int$marginal_method,
    approx_data = int$approx_data,
    pt = int$partable,
    lavmodel = int$lavmodel,
    nsamp = 200L,
    R_star = int$R_star,
    integration_data = int$inla_integration %||% NULL,
    native_theta_transforms = NULL,
    cov_var_idx1 = NULL,
    cov_var_idx2 = NULL,
    nthreads = 1L
  )

  expect_equal(dim(samp_native$theta_samp), dim(samp_r$theta_samp))
  expect_equal(dim(samp_native$x_samp), dim(samp_r$x_samp))
  expect_lt(mean(abs(samp_native$x_samp - samp_r$x_samp)), 1e-3)
})

test_that("Native LISREL multigroup mean structure matches lavaan", {
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "

  fit0 <- INLAvaan:::with_safe_detectCores(
    lavaan::cfa(
      mod, dat,
      group = "school",
      meanstructure = TRUE,
      do.fit = FALSE, parser = "old", test = "none", verbose = FALSE
    )
  )
  backend <- INLAvaan:::native_lisrel_backend_extract(fit0)
  x_free <- lavaan:::lav_model_get_parameters(fit0@Model, type = "free")

  ll_native <- INLAvaan:::cpp_lisrel_loglik(backend, x_free)
  ll_ref <- INLAvaan:::inlav_model_loglik(
    x_free,
    fit0@Model,
    fit0@SampleStats,
    fit0@Data,
    fit0@Options,
    fit0@Cache
  )
  expect_equal(ll_native, ll_ref, tolerance = 1e-8)

  grad_native <- INLAvaan:::cpp_lisrel_grad(backend, x_free)
  grad_fd <- INLAvaan:::cpp_fast_grad(
    function(x) INLAvaan:::cpp_lisrel_loglik(backend, x),
    x_free
  )
  expect_equal(grad_native, grad_fd, tolerance = 1e-5)
})

test_that("Native LISREL missing-data loglik matches lavaan", {
  dat <- lavaan::HolzingerSwineford1939
  set.seed(123)
  datmiss <- dat
  ov_names <- paste0("x", 1:9)
  mis <- matrix(rbinom(nrow(dat) * length(ov_names), 1, 0.9), nrow(dat), length(ov_names))
  datmiss[ov_names] <- datmiss[ov_names] * mis
  datmiss[ov_names][datmiss[ov_names] == 0] <- NA

  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "

  fit0 <- INLAvaan:::with_safe_detectCores(
    lavaan::cfa(
      mod, datmiss,
      meanstructure = TRUE,
      missing = "ML",
      do.fit = FALSE, parser = "old", test = "none", verbose = FALSE
    )
  )
  backend <- INLAvaan:::native_lisrel_backend_extract(fit0)
  x_free <- lavaan:::lav_model_get_parameters(fit0@Model, type = "free")

  ll_native <- INLAvaan:::cpp_lisrel_loglik(backend, x_free)
  ll_ref <- INLAvaan:::inlav_model_loglik(
    x_free,
    fit0@Model,
    fit0@SampleStats,
    fit0@Data,
    fit0@Options,
    fit0@Cache
  )
  expect_equal(ll_native, ll_ref, tolerance = 1e-8)

  grad_native <- INLAvaan:::cpp_lisrel_grad(backend, x_free)
  grad_fd <- INLAvaan:::cpp_fast_grad(
    function(x) INLAvaan:::cpp_lisrel_loglik(backend, x),
    x_free
  )
  expect_equal(grad_native, grad_fd, tolerance = 1e-5)
})

test_that("Native ceq.simple theta gradient matches finite differences", {
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "

  int <- acfa(
    mod,
    dat,
    group = "school",
    group.equal = "loadings",
    std.lv = TRUE,
    verbose = FALSE,
    test = "none",
    nsamp = 5,
    vb_correction = FALSE,
    marginal_method = "marggaus",
    add_priors = FALSE,
    debug = TRUE
  )

  pt <- int$partable
  backend <- int$native_backend
  ceq.K <- int$lavmodel@ceq.simple.K
  theta <- int$theta_star_novbc
  ptfree_all <- pt$free[pt$free > 0L]
  ptfree_unique <- ptfree_all[!duplicated(ptfree_all)]
  expand_idx <- match(ptfree_all, ptfree_unique)
  expand_wt <- 1 / as.numeric(table(ptfree_all)[as.character(ptfree_all)])

  grad_theta_native <- function(pars) {
    pars_unpacked <- as.numeric(ceq.K %*% pars)
    x <- INLAvaan:::pars_to_x(pars_unpacked, pt)
    jcb_vec <- mapply(
      function(f, x) f(x),
      pt$ginv_prime[pt$free > 0],
      pars_unpacked
    )
    gll <- INLAvaan:::native_model_grad(x, backend)[expand_idx] * expand_wt
    sd1sd2 <- attr(x, "sd1sd2")
    jcb_mat <- attr(x, "jcb_mat")
    gll_th <- jcb_vec * sd1sd2 * gll
    if (!is.null(jcb_mat) && NROW(jcb_mat) > 0) {
      for (k in seq_len(nrow(jcb_mat))) {
        gll_th[jcb_mat[k, 1]] <- gll_th[jcb_mat[k, 1]] + jcb_mat[k, 3] * gll[jcb_mat[k, 2]]
      }
    }
    as.numeric(gll_th %*% ceq.K)
  }

  obj_theta_native <- function(pars) {
    pars_unpacked <- as.numeric(ceq.K %*% pars)
    x <- INLAvaan:::pars_to_x(pars_unpacked, pt, compute_jac = FALSE)
    INLAvaan:::native_model_loglik(x, backend)
  }

  grad_fd <- INLAvaan:::cpp_fast_grad(obj_theta_native, theta)

  expect_equal(grad_theta_native(theta), grad_fd, tolerance = 1e-4)
})

test_that("Native-supported models use the lightweight native initialiser", {
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "

  fit <- acfa(
    mod,
    dat,
    verbose = FALSE,
    nsamp = 5,
    vb_correction = FALSE,
    marginal_method = "marggaus"
  )

  expect_true(
    fit@external$inlavaan_internal$init_method %in% c("native_light", "lavaan_full")
  )
  expect_false(is.null(fit@external$inlavaan_internal$native_backend))
})

test_that("Missing-data ML models keep a native backend and native loglik matches R", {
  mod <- "
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
  "
  dat <- lavaan::PoliticalDemocracy
  set.seed(221)
  mis <- matrix(rbinom(prod(dim(dat)), 1, 0.99), nrow(dat), ncol(dat))
  dat <- dat * mis
  dat[dat == 0] <- NA

  fit <- asem(
    mod,
    dat,
    missing = "ML",
    meanstructure = TRUE,
    verbose = FALSE,
    test = "none",
    marginal_method = "marggaus",
    vb_correction = FALSE,
    nsamp = 5
  )

  int <- fit@external$inlavaan_internal
  expect_true(int$lavsamplestats@missing.flag)
  expect_false(is.null(int$native_backend))

  x_free <- lavaan:::lav_model_get_parameters(int$lavmodel, type = "free")
  ll_native <- INLAvaan:::cpp_lisrel_loglik(int$native_backend, x_free)
  ll_r <- INLAvaan:::inlav_model_loglik(
    x_free,
    int$lavmodel,
    int$lavsamplestats,
    int$lavdata,
    int$lavoptions,
    int$lavcache
  )
  expect_equal(ll_native, ll_r, tolerance = 1e-5)
})

test_that("Native lightweight initialiser matches lavaan parameter count without mean structure", {
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  "

  fit <- acfa(
    mod,
    dat,
    std.lv = TRUE,
    verbose = FALSE,
    nsamp = 5,
    vb_correction = FALSE,
    marginal_method = "marggaus",
    test = "none"
  )
  fit0 <- INLAvaan:::with_safe_detectCores(
    lavaan::cfa(
      mod,
      dat,
      std.lv = TRUE,
      do.fit = FALSE,
      parser = "old",
      test = "none",
      verbose = FALSE
    )
  )

  expect_equal(fit@Fit@npar, fit0@Fit@npar)
  expect_equal(length(fit@external$inlavaan_internal$theta_star), fit0@Fit@npar)
  expect_false(any(grepl("~1", names(fit@external$inlavaan_internal$coefficients), fixed = TRUE)))
})

test_that("Native partable parser matches ceq.simple multigroup equality packing", {
  dat <- lavaan::HolzingerSwineford1939
  mod <- "
    visual  =~ x1 + x2
    textual =~ x4 + x5
  "

  fit_load <- INLAvaan:::with_safe_detectCores(
    lavaan::cfa(
      mod, dat,
      std.lv = TRUE,
      group = "school",
      group.equal = "loadings",
      ceq.simple = TRUE,
      do.fit = FALSE, parser = "old", test = "none", verbose = FALSE
    )
  )
  pt_load_native <- INLAvaan:::native_lisrel_build_partable(list(
    model = mod,
    data = dat,
    model.type = "cfa",
    std.lv = TRUE,
    int.ov.free = TRUE,
    int.lv.free = FALSE,
    auto.fix.first = FALSE,
    auto.fix.single = TRUE,
    auto.var = TRUE,
    auto.cov.lv.x = TRUE,
    auto.efa = TRUE,
    auto.th = TRUE,
    auto.delta = TRUE,
    auto.cov.y = TRUE,
    group = "school",
    group.equal = "loadings"
  ))
  expect_equal(pt_load_native$free, as.data.frame(fit_load@ParTable)$free)

  fit_both <- INLAvaan:::with_safe_detectCores(
    lavaan::cfa(
      mod, dat,
      std.lv = TRUE,
      group = "school",
      group.equal = c("loadings", "intercepts"),
      ceq.simple = TRUE,
      do.fit = FALSE, parser = "old", test = "none", verbose = FALSE
    )
  )
  pt_both_native <- INLAvaan:::native_lisrel_build_partable(list(
    model = mod,
    data = dat,
    model.type = "cfa",
    std.lv = TRUE,
    int.ov.free = TRUE,
    int.lv.free = FALSE,
    auto.fix.first = FALSE,
    auto.fix.single = TRUE,
    auto.var = TRUE,
    auto.cov.lv.x = TRUE,
    auto.efa = TRUE,
    auto.th = TRUE,
    auto.delta = TRUE,
    auto.cov.y = TRUE,
    group = "school",
    group.equal = c("loadings", "intercepts")
  ))
  expect_equal(pt_both_native$free, as.data.frame(fit_both@ParTable)$free)
})
