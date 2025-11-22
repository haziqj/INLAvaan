partable_classify_sem_matrix <- function(lhs, op, rhs, ov.names, std.ov, std.lv) {
  lhs_is_ov <- lhs %in% ov.names
  rhs_is_ov <- rhs %in% ov.names

  if (op == "=~") {
    return("lambda")
  }

  if (op == "~~") {
    if (lhs_is_ov & rhs_is_ov) {
      if (lhs == rhs) {
        return("theta_var")
      } else {
        if (isTRUE(std.ov)) {
          return("theta_cor")
        } else {
          return("theta_cov")
        }
      }
    } else {
      (!lhs_is_ov & !rhs_is_ov)
    }
    if (lhs == rhs) {
      return("psi_var")
    } else {
      if (isTRUE(std.lv)) {
        return("psi_cor")
      } else {
        return("psi_cov")
      }
    }
  }

  if (op == "~*~") {
    return("delta")
  }

  if (op == "~") {
    if (!lhs_is_ov) {
      return("beta")
    }
  }

  if (op == "~1") {
    if (lhs_is_ov) {
      return("nu")
    } else if (!lhs_is_ov) {
      return("alpha")
    }
  }

  if (op == "|") {
    return("tau")
  }
  if (op == ":=") {
    return("defined")
  }
  if (op %in% c("==", "<", ">")) {
    return("constraint")
  }
  if (op == "@") {
    return("fixed")
  }

  return(NA_character_)
}

partable_prior_from_row <- function(matrix, lhs, rhs, op, dp = blavaan::dpriors()) {
  is_var <- grepl("_var", matrix)

  if (matrix == "nu") {
    return(dp[["nu"]])
  }
  if (matrix == "alpha") {
    return(dp[["alpha"]])
  }
  if (matrix == "lambda") {
    return(dp[["lambda"]])
  }
  if (matrix == "beta") {
    return(dp[["beta"]])
  }

  if (grepl("theta", matrix)) {
    return(if (is_var) dp[["theta"]] else dp[["rho"]])
  }
  if (grepl("psi", matrix)) {
    return(if (is_var) dp[["psi"]] else dp[["rho"]])
  }

  if (matrix == "tau") {
    return(dp[["tau"]])
  }

  return(NA_character_)
}

safe_tanh <- function(x, eps = 1e-6) {
  (1 - eps) * tanh(x)
}

partable_transform_funcs <- function(matrix) {
  g <- identity
  ginv <- identity
  ginv_prime <- function(x) 1

  if (grepl("theta_var|psi_var", matrix)) {
    g <- log
    ginv <- exp
    ginv_prime <- exp
  }

  if (grepl("theta_cor|theta_cov|psi_cor|psi_cov", matrix)) {
    g <- atanh
    ginv <- safe_tanh
    ginv_prime <- \(x) 1 - safe_tanh(x) ^ 2
  }

  return(list(g = g, ginv = ginv, ginv_prime = ginv_prime))
}

inlavaanify_partable <- function(pt, dp = blavaan::dpriors(), lavdata, lavoptions) {
  ngroups <- lavdata@ngroups
  std_ov <- lavoptions$std.ov
  std_lv <- lavoptions$std.lv
  pt$mat <- NA


  for (g in seq_len(ngroups)) {
    # Identify stuff
    ov.names <- lavdata@ov.names[[g]]
    pt$mat[pt$group == g] <- mapply(
      partable_classify_sem_matrix,
      lhs = pt$lhs[pt$group == g],
      op = pt$op[pt$group == g],
      rhs = pt$rhs[pt$group == g],
      MoreArgs = list(ov.names = ov.names, std.ov = std_ov, std.lv = std_lv),
      SIMPLIFY = TRUE
    )
  }

  # Add priors
  pt$prior <- mapply(
    partable_prior_from_row,
    matrix = pt$mat,
    lhs = pt$lhs,
    rhs = pt$rhs,
    op = pt$op,
    MoreArgs = list(dp = dp),
    USE.NAMES = FALSE
  )
  pt$prior[pt$free == 0] <- NA_character_

  # Add transformations to unrestricted parameter space
  tmp <- lapply(pt$mat, partable_transform_funcs)
  pt$g <- lapply(tmp, `[[`, "g")
  pt$ginv <- lapply(tmp, `[[`, "ginv")
  pt$ginv_prime <- lapply(tmp, `[[`, "ginv_prime")

  # Compute starting values in unrestricted space
  pt$parstart <- mapply(
    function(fun, val) fun(val), pt$g, pt$start,
    USE.NAMES = FALSE
  )

  # Add names
  pt$names <- mapply(paste0, pt$lhs, pt$op, pt$rhs)

  # FIXME: Perhaps add a 'inlavaan_partable' class to this object
  as.list(pt)
}
