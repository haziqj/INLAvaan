generic_obj <- function(x, lavmodel, lavsamplestats, lavdata, lavcache) {
  # 1. Update parameters
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)

  # 2. Get the lavaan objective
  # Note: For Multilevel, this returns Sum(-LogLik) / N_total
  #       For Single-level, this returns Sum(N * F) / N_total  (roughly)
  fx <- lavaan:::lav_model_objective(
    lavmodel = lavmodel_x,
    GLIST = NULL,
    lavsamplestats = lavsamplestats,
    lavdata = lavdata,
    lavcache = lavcache
  )

  # 3. Recover the Kernel (The data-dependent part)
  # In ALL ML cases (Single or Multilevel), multiplying by -1 * Ntotal
  # recovers the sum of log-likelihoods (minus constants).
  ntotal <- lavsamplestats@ntotal
  ll_kernel <- -1 * ntotal * fx

  # 4. Add the Correct Constants
  ll_constant <- 0
  log2pi <- log(2 * pi)

  # --- CASE 1: MULTILEVEL (Cluster) ---
  if (lavdata@nlevels > 1L) {
    # lav_model_objective_2l computes the direct likelihood (not a discrepancy).
    # The only thing missing is the -0.5 * log(2pi) term for every data point.

    for (g in 1:lavsamplestats@ngroups) {
      # In Multilevel, N is the number of Level-1 observations
      n_obs_g <- lavsamplestats@nobs[[g]]

      # P is the number of observed variables at Level 1
      # (Check lavmodel@nvar per group)
      p_g <- if (is.list(lavmodel@nvar)) {
        lavmodel@nvar[[g]]
      } else {
        lavmodel@nvar[g]
      }

      # The constant is: -0.5 * N * P * log(2pi)
      ll_constant <- ll_constant + (-0.5 * n_obs_g * p_g * log2pi)
    }

    # Note: If you have missing data in Multilevel (FIML), 'n_obs_g * p_g'
    # overestimates the constant. You would strictly need to sum the actual
    # observed values per case. For MAP, this constant is irrelevant.
  } else if (lavmodel@estimator == "ML" && !lavsamplestats@missing.flag) {
    # --- CASE 2: SINGLE LEVEL (Standard ML) ---
    # Here we MUST add the Saturated Model likelihood back because lavaan
    # minimized a discrepancy (F = LL_sat - LL_mod).

    for (g in 1:lavsamplestats@ngroups) {
      n_obs_g <- lavsamplestats@nobs[[g]]
      p_g <- if (is.list(lavmodel@nvar)) {
        lavmodel@nvar[[g]]
      } else {
        lavmodel@nvar[g]
      }
      log_det_S <- lavsamplestats@cov.log.det[[g]]

      # Saturated Constant: -N/2 * ( p*log(2pi) + log|S| + p )
      const_g <- -0.5 * n_obs_g * (p_g * log2pi + log_det_S + p_g)
      ll_constant <- ll_constant + const_g
    }
  } else if (lavmodel@estimator == "ML" && lavsamplestats@missing.flag) {
    # --- CASE 3: SINGLE LEVEL FIML (Missing Data) ---
    # lavaan calculates direct likelihood (Sum -2LL) but with log2pi=FALSE.
    # We recover the missing 2pi terms based on observed patterns.

    for (g in 1:lavsamplestats@ngroups) {
      # Iterate over missing patterns to count total observed values
      total_observed_values <- sum(sapply(
        lavsamplestats@missing[[g]],
        function(p) {
          p$freq * sum(p$var.idx) # frequency of pattern * num vars in pattern
        }
      ))

      ll_constant <- ll_constant + (-0.5 * log2pi * total_observed_values)
    }
  }

  return(ll_kernel + ll_constant)
}

generic_grd <- function(x, lavmodel, lavsamplestats, lavdata, lavcache) {
  # 1. Update the lavmodel with parameters 'x'
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)

  # 2. Get the raw lavaan gradient
  # Note: This is usually the gradient of the *minimization* objective
  dx <- lavaan:::lav_model_gradient(
    lavmodel = lavmodel_x,
    GLIST = NULL,
    lavsamplestats = lavsamplestats,
    lavdata = lavdata,
    lavcache = lavcache,
    type = "free"
  )

  # 3. Apply the correct Scaling Factor to convert to 'Gradient of LogLik'
  # We need to map from [Gradient of Obj] -> [Gradient of LogLik]
  # Basic rule: LogLik = - (N * Multiplier) * Obj
  # So: Grad_LogLik = - (N * Multiplier) * Grad_Obj

  ntotal <- lavsamplestats@ntotal
  scale_factor <- 0

  # --- CASE 1: Multilevel ML ---
  if (lavdata@nlevels > 1L) {
    # In lav_model_gradient.R (line ~390), the code divides by (2 * ntotal).
    # The input to that division was the gradient of (-2 * LogLik).
    # So lavaan returns: (1 / 2N) * Grad(-2LL) = -1/N * Grad(LL).
    # To recover Grad(LL), we multiply by -N.
    scale_factor <- -1 * ntotal
  } else if (lavmodel@estimator == "ML" && lavmodel@conditional.x) {
    # --- CASE 2: ML with Conditional X ---
    # In lav_model_gradient.R (line ~318), the code explicitly does: group.dx / 2.
    # It returns the weighted average gradient: (1/N) * Sum(N_g * 0.5 * Grad(F)).
    # We know LogLik ~ -0.5 * N * F.
    # To recover Grad(LL), we need to multiply by -N.
    # (Because the /2 in LogLik formula cancels with the /2 in lavaan's gradient code).
    scale_factor <- -1 * ntotal
  } else if (lavmodel@estimator == "ML") {
    # --- CASE 3: Standard ML & FIML (Missing) ---
    # In lav_model_gradient.R (lines 104-165), the code computes Omega (Grad F).
    # It does NOT divide by 2 (unlike the conditional.x case).
    # It returns the weighted average: (1/N) * Sum(N_g * Grad(F)).
    # Since LogLik = -0.5 * N * F
    # Grad(LogLik) = -0.5 * N * Grad(F)
    # We must multiply by -N/2.
    scale_factor <- -0.5 * ntotal
  } else if (lavmodel@estimator == "PML") {
    # --- CASE 4: PML (Pairwise ML) ---
    # lavaan calculates 'group.w * group.dx'.
    # This results in the Average LogLik gradient.
    # We need the Sum LogLik gradient.
    scale_factor <- -1 * ntotal
  }

  # Apply scaling
  grad_ll <- dx * scale_factor

  return(grad_ll)
}


# Multivariate normal likelihood (from sample statistics)
mvnorm_loglik_samplestats <- function(
  x,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavoptions,
  lavcache
) {
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)
  lavimplied <- lavaan::lav_model_implied(lavmodel_x)
  nG <- lavsamplestats@ngroups
  res <- vector("numeric", length = nG)

  for (g in seq_len(nG)) {
    Sigma <- lavimplied$cov[[g]]
    if (check_mat(Sigma)) {
      return(-1e40)
    }

    Mu <-
      if (lavmodel@meanstructure) {
        lavimplied$mean[[g]]
      } else {
        lavsamplestats@mean[[g]]
      }

    res[g] <- lavaan___lav_mvnorm_loglik_samplestats(
      sample.mean = lavsamplestats@mean[[g]],
      sample.cov = lavsamplestats@cov[[g]],
      sample.nobs = lavsamplestats@nobs[[g]],
      Mu = Mu,
      Sigma = Sigma,
      x.idx = lavsamplestats@x.idx[[g]],
      x.mean = lavsamplestats@mean.x[[g]],
      x.cov = lavsamplestats@cov.x[[g]],
      Sinv.method = "eigen",
      Sigma.inv = NULL
    )
  }

  sum(res)
}

mvnorm_loglik_grad <- function(
  x,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavoptions,
  lavcache
) {
  # Gradient of fit function F_ML (not loglik yet)
  grad_F <- lavaan___lav_model_gradient(
    lavmodel = lavaan::lav_model_set_parameters(lavmodel, x),
    lavsamplestats = lavsamplestats,
    lavdata = lavdata
  )
  # Rescale so we get gradient of loglik
  out <- -1 * lavsamplestats@ntotal * grad_F
  out
}

# nocov start

# Pairwise likelihood
pl_fn <- function(x, lavmodel, lavsamplestats, lavdata, lavoptions, lavcache) {
  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)
  lavimplied <- lavaan::lav_model_implied(lavmodel_x)
  Sigma <- lavimplied$cov[[1]]

  no_ord <- length(fit@Data@ordered)
  kappa <- 1 / sqrt(no_ord) # scaling factor for PML

  if (check_mat(Sigma)) {
    return(-1e40)
  }

  pml_objval <- lavaan___lav_model_objective_pml(
    Sigma.hat = Sigma,
    Mu.hat = lavimplied$mean[[1]],
    TH = lavimplied$th[[1]],
    PI = NULL,
    th.idx = lavmodel_x@th.idx[[1]],
    num.idx = NULL,
    X = NULL,
    eXo = NULL,
    wt = NULL,
    lavcache = lavcache[[1]],
    missing = lavdata@missing
  )
  out <- attr(pml_objval, "logl")
  out * kappa
}

pl_grad <- function(
  x,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavcache
) {
  no_ord <- length(fit@Data@ordered)
  kappa <- 1 / sqrt(no_ord) # scaling factor for PML

  # 1. Update the model parameters
  # This updates GLIST inside lavmodel so the gradient uses the new 'x'
  lavmodel <- lavaan::lav_model_set_parameters(lavmodel, x)

  # 2. Compute Gradient of the Objective Function
  # For PML, lavaan minimizes the Negative Pairwise Log-Likelihood.
  # unlike ML, the PML gradient in lav_model_gradient is NOT divided by N.
  grad_Obj <- lavaan___lav_model_gradient(
    lavmodel = lavmodel,
    GLIST = NULL,
    lavsamplestats = lavsamplestats,
    lavdata = lavdata,
    lavcache = lavcache,
    type = "free"
  )

  # 3. Convert to Gradient of Log-Likelihood
  # grad_Obj = Gradient(-1 * Sum(log_lik))
  # We want Gradient(Sum(log_lik))
  # So we just flip the sign. No * ntotal needed.
  out <- -1 * grad_Obj

  out * kappa
}

# WLS fit function
wls_fn <- function(
  x,
  lavmodel,
  lavsamplestats,
  lavdata,
  lavoptions,
  lavcache,
  PT
) {
  # Here, only optimise non-thresholds
  fullx <- PT$start[PT$free > 0]
  fulltype <- PT$mat[PT$free > 0]
  fullx[fulltype != "tau"] <- x
  ntau <- sum(fulltype == "tau")

  lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, fullx)
  lavimplied <- lavaan::lav_model_implied(lavmodel_x)
  Sigma <- lavimplied$cov[[1]]
  Sigma <- with(lavmodel_x@GLIST, {
    LPLt <- lambda %*% psi %*% t(lambda)
    Theta <- matrix(0, nrow = nrow(LPLt), ncol = nrow(LPLt))
    diag(Theta) <- 1 - diag(LPLt)
    LPLt + Theta
  })

  if (check_mat(Sigma)) {
    return(1e40)
    # Sigma <- force_pd(Sigma)
  }

  # Sample and model implied correlations
  s <- lavsamplestats@WLS.obs[[1]]
  s <- s[-(1:ntau)]
  Sigma_cor <- cov2cor(Sigma)
  sigma <- lavaan::lav_matrix_vech(Sigma_cor, diag = FALSE)

  if (is.null(lavsamplestats@WLS.V[[1]])) {
    # DWLS
    # FIXME: I don't know why dividing by n makes it better?
    w <- lavsamplestats@WLS.VD[[1]] / lavsamplestats@nobs[[1]]
    w <- w[-(1:ntau)]
    out <- -2 * sum((s - sigma)^2 / sqrt(w))
    # out <- sum(dnorm(s, mean = sigma, sd = sqrt(w), log = TRUE))
  } else {
    # WLS
    Gamma <- lavsamplestats@WLS.V[[1]]
    out <- -1 *
      t(s - sigma) %*% solve(Gamma[-(1:ntau), -(1:ntau)]) %*% (s - sigma)
  }
  as.numeric(out)
}
# nocov end
