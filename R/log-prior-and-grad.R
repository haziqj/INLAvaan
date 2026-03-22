prepare_priors_for_optim <- function(pt) {
  # 1. Filter for relevant rows: Free parameters that are not duplicates
  # We only calculate gradients for unique free parameters.
  idx <- which(
    !is.na(pt$prior) & pt$prior != "" & pt$free > 0 & !duplicated(pt$free)
  )

  # Initialize storage vectors (length of RELEVANT priors only)
  n <- length(idx)

  # A. Transformation Codes (0=Identity, 1=Exp/Log, 2=Tanh/Atanh)
  # Derived from your partable_transform_funcs logic
  trans_type <- integer(n) # Default 0 (Identity)

  # Check the 'mat' column for transformation types
  # (theta_var/psi_var -> Exp; theta_cor/psi_cor -> Tanh)
  mat_vals <- pt$mat[idx]
  trans_type[grepl("theta_var|psi_var", mat_vals)] <- 1L
  trans_type[grepl("theta_cor|theta_cov|psi_cor|psi_cov", mat_vals)] <- 2L

  # B. Prior Codes and Hyperparameters
  prior_type <- integer(n) # 0=None, 1=Normal, 2=Gamma, 3=Beta
  p1 <- numeric(n) # mu / shape / a
  p2 <- numeric(n) # sd / rate / b
  is_sd_prior <- logical(n) # Special flag for [sd] qualifier
  is_prec_prior <- logical(n) # Special flag for [prec] qualifier

  raw_priors <- pt$prior[idx]

  # Detect [prec] qualifier (valid for any distribution)
  is_prec_prior <- grepl("\\[prec\\]", raw_priors)

  # Strip ALL qualifiers before numeric parsing
  clean_priors <- gsub("\\[sd\\]|\\[prec\\]", "", raw_priors)

  # Parse Normal
  is_norm <- grepl("normal", clean_priors)
  if (any(is_norm)) {
    prior_type[is_norm] <- 1L
    # Parse string "normal(0,1)" -> c(0, 1)
    # Using efficient vectorized parsing approach
    clean <- gsub("normal\\(|\\)", "", clean_priors[is_norm])
    splits <- strsplit(clean, ",")
    p1[is_norm] <- as.numeric(sapply(splits, `[`, 1))
    p2[is_norm] <- as.numeric(sapply(splits, `[`, 2))
  }

  # Parse Gamma
  is_gamma <- grepl("gamma", clean_priors)
  if (any(is_gamma)) {
    prior_type[is_gamma] <- 2L
    is_sd <- grepl("\\[sd\\]", raw_priors[is_gamma]) & !is_prec_prior[is_gamma]
    is_sd_prior[is_gamma] <- is_sd

    clean <- gsub("gamma\\(|\\)", "", clean_priors[is_gamma])
    splits <- strsplit(clean, ",")
    p1[is_gamma] <- as.numeric(sapply(splits, `[`, 1))
    p2[is_gamma] <- as.numeric(sapply(splits, `[`, 2))
  }

  # Parse Beta
  is_beta <- grepl("beta", clean_priors)
  if (any(is_beta)) {
    prior_type[is_beta] <- 3L
    clean <- gsub("beta\\(|\\)", "", clean_priors[is_beta])
    splits <- strsplit(clean, ",")
    p1[is_beta] <- as.numeric(sapply(splits, `[`, 1))
    p2[is_beta] <- as.numeric(sapply(splits, `[`, 2))
  }

  # Return a lightweight list optimized for the gradient function
  list(
    parname = pt$names[idx], # Just for names(theta)
    idx_in_pt = idx, # Indices in original pt
    free_id = pt$free[idx], # Which theta index this corresponds to
    trans_type = trans_type, # 0=Id, 1=Exp, 2=Tanh
    prior_type = prior_type, # 1=Norm, 2=Gam, 3=Beta
    p1 = p1,
    p2 = p2,
    is_sd_prior = is_sd_prior,
    is_prec_prior = is_prec_prior,
    prior_names = raw_priors # Just for names(grad)
  )
}

prior_logdens_vectorized <- function(theta, cache, debug = FALSE) {
  # 1. Map theta to relevant parameters (vectorized)
  th <- theta[cache$free_id]
  n <- length(th)

  # 2. Apply Transformations & Jacobian of Transformation
  # Initialize vectors
  xval <- th
  dx_dth <- rep(1, n)

  # A. Exp (Log-link) - e.g., Variances
  idx_exp <- which(cache$trans_type == 1L)
  if (length(idx_exp) > 0) {
    ex <- exp(th[idx_exp])
    xval[idx_exp] <- ex
    dx_dth[idx_exp] <- ex
  }

  # B. Tanh (Atanh-link) - e.g., Correlations
  idx_tanh <- which(cache$trans_type == 2L)
  if (length(idx_tanh) > 0) {
    # Replicating your safe_tanh logic roughly, or use standard tanh
    # Note: d/dx tanh(x) = 1 - tanh^2(x)
    eps <- 1e-6
    safe_scale <- (1 - eps)

    t_val <- tanh(th[idx_tanh])
    xval[idx_tanh] <- safe_scale * t_val
    dx_dth[idx_tanh] <- safe_scale * (1 - t_val^2)
  }

  # 3. Calculate Log-Densities (Vectorized by Type)
  lp <- numeric(n)

  # Type 1: Normal
  idx_norm <- which(cache$prior_type == 1L)
  if (length(idx_norm) > 0) {
    # dnorm is fully vectorized
    lp[idx_norm] <- dnorm(
      xval[idx_norm],
      mean = cache$p1[idx_norm],
      sd = cache$p2[idx_norm],
      log = TRUE
    )
  }

  # Type 2: Gamma
  idx_gam <- which(cache$prior_type == 2L)
  if (length(idx_gam) > 0) {
    # Separate standard Gamma vs Gamma[sd]
    # We can do this with masking to keep it vectorized
    sub_sd <- cache$is_sd_prior[idx_gam]

    # 1. Standard Gamma Logic (applied to all first)
    # Note: Gamma check for x > 0 is handled by dgamma returning -Inf/NaN if invalid
    lp[idx_gam] <- dgamma(
      xval[idx_gam],
      shape = cache$p1[idx_gam],
      rate = cache$p2[idx_gam],
      log = TRUE
    )

    # 2. Fix up Gamma[sd] cases
    if (any(sub_sd)) {
      # Indices of the [sd] items within the global vectors
      global_sd_idx <- idx_gam[sub_sd]

      # Get current xval (which is variance)
      vars <- xval[global_sd_idx]
      sds <- sqrt(vars)

      # Re-calculate density for s = sqrt(x)
      lp[global_sd_idx] <- dgamma(
        sds,
        shape = cache$p1[global_sd_idx],
        rate = cache$p2[global_sd_idx],
        log = TRUE
      )

      # Apply Jacobian correction for [sd]:
      # The gradient code divided by 2*sqrt(x).
      # Here we adjust the Jacobian term dx_dth directly.
      # Original dx_dth is d(xval)/d(theta).
      # We need d(sd)/d(theta) = d(sd)/d(xval) * d(xval)/d(theta)
      #                        = (1 / 2*sqrt(xval)) * dx_dth
      dx_dth[global_sd_idx] <- dx_dth[global_sd_idx] / (2 * sds)
    }
  }

  # Type 3: Beta (Boxed on -1, 1)
  idx_beta <- which(cache$prior_type == 3L)
  if (length(idx_beta) > 0) {
    # Inline dbeta_box logic for speed:
    # dbeta_box(x, a, b, min=-1, max=1) = dbeta((x - min)/(max-min), a, b) / (max-min)
    # Scaled to [0,1]: y = (x + 1) / 2
    # Jacobian of scale: 1/2

    xv <- xval[idx_beta]
    # Shift to [0,1]
    y <- (xv + 1) / 2

    # Calculate density
    # log(dbeta(y) * 1/2) = log(dbeta(y)) - log(2)
    val <- dbeta(
      y,
      shape1 = cache$p1[idx_beta],
      shape2 = cache$p2[idx_beta],
      log = TRUE
    ) -
      0.69314718 # log(2)

    lp[idx_beta] <- val
  }

  # 4. Handle [prec] qualifier: prior is on precision (1/x) rather than x
  # Re-evaluate lp at 1/xval and apply Jacobian |d(1/x)/dx| = 1/x^2
  idx_prec <- which(cache$is_prec_prior %||% logical(n))
  if (length(idx_prec) > 0) {
    ip_norm  <- idx_prec[cache$prior_type[idx_prec] == 1L]
    ip_gamma <- idx_prec[cache$prior_type[idx_prec] == 2L]
    ip_beta  <- idx_prec[cache$prior_type[idx_prec] == 3L]

    if (length(ip_norm) > 0) {
      lp[ip_norm] <- dnorm(
        1 / xval[ip_norm], cache$p1[ip_norm], cache$p2[ip_norm], log = TRUE
      )
    }
    if (length(ip_gamma) > 0) {
      lp[ip_gamma] <- dgamma(
        1 / xval[ip_gamma], cache$p1[ip_gamma], cache$p2[ip_gamma], log = TRUE
      )
    }
    if (length(ip_beta) > 0) {
      y <- (1 / xval[ip_beta] + 1) / 2
      lp[ip_beta] <- dbeta(
        y, shape1 = cache$p1[ip_beta], shape2 = cache$p2[ip_beta], log = TRUE
      ) - 0.69314718
    }

    # Jacobian: |d(1/x)/dx| = 1/x^2 — multiply dx_dth by 1/x^2
    dx_dth[idx_prec] <- dx_dth[idx_prec] / (xval[idx_prec]^2)
  }

  # 5. Final Summation
  # log|J| + log(dens)
  ljcb <- log(abs(dx_dth))

  if (debug) {
    # Reconstruct names for debugging if needed
    names(lp) <- cache$prior_names
    return(list(theta = theta, lp = lp, ljcb = ljcb))
  }

  total_log_dens <- sum(lp + ljcb)

  # Safety check (vectorized finite check is faster than is.finite on sum if sum is NA)
  if (!is.finite(total_log_dens)) {
    return(-1e40)
  }

  return(total_log_dens)
}

prior_grad_vectorized <- function(theta, cache) {
  # 'cache' is the object returned by prepare_priors_for_optim()

  # 1. Map theta to relevant parameters
  th <- theta[cache$free_id]
  n <- length(th)

  # 2. Apply Transformations (Vectorized)
  # Calculate xval, dx_dth, ddx_dth2 based on trans_type
  xval <- th # Default Identity
  dx_dth <- rep(1, n)
  ddx_dth2 <- rep(0, n)

  # Type 1: Exp (Log-link) -> for Variances
  idx_exp <- which(cache$trans_type == 1L)
  if (length(idx_exp) > 0) {
    ex <- exp(th[idx_exp])
    xval[idx_exp] <- ex
    dx_dth[idx_exp] <- ex
    ddx_dth2[idx_exp] <- ex
  }

  # Type 2: Tanh (Atanh-link) -> for Correlations
  idx_tanh <- which(cache$trans_type == 2L)
  if (length(idx_tanh) > 0) {
    # Using your safe_tanh logic: (1 - eps) * tanh(x)
    # Note: Your original code applied safe_tanh on the INVERSE (ginv).
    # Since theta is unrestricted, theta -> tanh(theta) maps R -> (-1, 1)

    # Calculate tanh(theta)
    th_t <- th[idx_tanh]
    t_val <- tanh(th_t)

    # Apply safe scaling (eps hardcoded here or passed in env)
    eps <- 1e-6
    safe_scale <- (1 - eps)

    xval[idx_tanh] <- safe_scale * t_val

    # Derivatives
    # d/dx tanh(x) = 1 - tanh^2(x)
    dt_dth <- 1 - t_val^2
    dx_dth[idx_tanh] <- safe_scale * dt_dth

    # Second derivative: -2 * tanh * (1-tanh^2) * scale
    # Note: Your original code had: -2 * safe_tanh(x) * (1 - safe_tanh(x)^2)
    # But strictly: d2/dth2 (scale*tanh) = scale * (-2*tanh * sech^2)
    # We will stick to the strict derivative of your ginv:
    ddx_dth2[idx_tanh] <- safe_scale * (-2 * t_val * dt_dth)
  }

  # 3. Calculate Prior Gradients (dlp_dx)
  dlp_dx <- numeric(n)
  jac_extra <- numeric(n) # Defaults to 0

  # A. Normal (Type 1)
  idx_norm <- which(cache$prior_type == 1L)
  if (length(idx_norm) > 0) {
    # -(x - mu) / sd^2
    dlp_dx[idx_norm] <- -(xval[idx_norm] - cache$p1[idx_norm]) /
      (cache$p2[idx_norm]^2)
  }

  # B. Gamma (Type 2)
  idx_gam <- which(cache$prior_type == 2L)
  if (length(idx_gam) > 0) {
    # Split into standard Gamma vs Gamma[sd]
    # We use logical masking within the gamma group
    sub_sd <- cache$is_sd_prior[idx_gam]

    # Parameters for all gammas
    shape <- cache$p1[idx_gam]
    rate <- cache$p2[idx_gam]
    xv <- xval[idx_gam]

    # Calculation for Standard Gamma
    # (shape - 1)/x - rate
    # We calculate this for everyone first
    res_gam <- (shape - 1) / xv - rate

    # Correction for Gamma[sd]
    if (any(sub_sd)) {
      # Indices within the gamma subset that are [sd]
      # Re-implementing:
      # s = sqrt(x); dlp_ds = (shape-1)/s - rate; dlp_dx = dlp_ds / (2s)
      # jac_extra = -dx_dth / (2x)

      # We just overwrite the values for the [sd] rows
      xv_sd <- xv[sub_sd]
      s_val <- sqrt(xv_sd)
      dlp_ds <- (shape[sub_sd] - 1) / s_val - rate[sub_sd]

      # Update dlp_dx for these specific rows
      res_gam[sub_sd] <- dlp_ds / (2 * s_val)

      # Add Jacobian extra
      # We need the dx_dth specific to these rows
      dx_dth_gam <- dx_dth[idx_gam]
      jac_extra[idx_gam[sub_sd]] <- -dx_dth_gam[sub_sd] / (2 * xv_sd)
    }

    dlp_dx[idx_gam] <- res_gam
  }

  # C. Beta (Type 3)
  idx_beta <- which(cache$prior_type == 3L)
  if (length(idx_beta) > 0) {
    a <- cache$p1[idx_beta]
    b <- cache$p2[idx_beta]
    xv <- xval[idx_beta]
    # d/dx log p_X(x) = d/dy log dbeta(y,a,b) * dy/dx
    #   = [(a-1)/y - (b-1)/(1-y)] * 1/2, with y = (x+1)/2
    #   = [(a-1)/((x+1)/2) - (b-1)/((1-x)/2)] * 1/2
    #   = (a-1)/(x+1) - (b-1)/(1-x)
    dlp_dx[idx_beta] <- (a - 1) / (xv + 1) - (b - 1) / (1 - xv)
  }

  # Handle [prec] qualifier: prior is on precision (1/x)
  # Chain rule: dlp_dx = (d log p(tau) / d tau)|_{tau=1/x} * (-1/x^2)
  # Jacobian extra: d/d(theta) log|d(1/x)/dx| = -2 * x'(theta) / x
  idx_prec <- which(cache$is_prec_prior %||% logical(n))
  if (length(idx_prec) > 0) {
    pv <- 1 / xval[idx_prec] # precision values

    ip_norm  <- which(cache$prior_type[idx_prec] == 1L)
    ip_gamma <- which(cache$prior_type[idx_prec] == 2L)
    ip_beta  <- which(cache$prior_type[idx_prec] == 3L)

    raw_dlp_dprec <- numeric(length(idx_prec))

    if (length(ip_norm) > 0) {
      raw_dlp_dprec[ip_norm] <- -(pv[ip_norm] - cache$p1[idx_prec[ip_norm]]) /
        (cache$p2[idx_prec[ip_norm]]^2)
    }
    if (length(ip_gamma) > 0) {
      raw_dlp_dprec[ip_gamma] <- (cache$p1[idx_prec[ip_gamma]] - 1) / pv[ip_gamma] -
        cache$p2[idx_prec[ip_gamma]]
    }
    if (length(ip_beta) > 0) {
      pv_b <- pv[ip_beta]
      a_b <- cache$p1[idx_prec[ip_beta]]
      b_b <- cache$p2[idx_prec[ip_beta]]
      raw_dlp_dprec[ip_beta] <- (a_b - 1) / (pv_b + 1) - (b_b - 1) / (1 - pv_b)
    }

    # Chain: dlp_dx = dlp_dprec * d(1/x)/dx = dlp_dprec * (-1/x^2)
    dlp_dx[idx_prec] <- raw_dlp_dprec * (-1 / xval[idx_prec]^2)

    # Jacobian: d/d(theta) log(1/x^2) = -2 * x'/x
    jac_extra[idx_prec] <- -2 * dx_dth[idx_prec] / xval[idx_prec]
  }

  # 4. Chain Rule Assembly
  grad <- dlp_dx * dx_dth + ddx_dth2 / dx_dth + jac_extra

  names(grad) <- cache$prior_names
  return(grad)
}
