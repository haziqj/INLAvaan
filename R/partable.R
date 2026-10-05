partable_classify_sem_matrix <- function(
  lhs,
  op,
  rhs,
  ov.names,
  std.ov,
  std.lv
) {
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
    # if (!lhs_is_ov) {
    return("beta")
    # }
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

partable_prior_from_row <- function(matrix, lhs, rhs, op, dp) {
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
  g_prime <- function(x) 1
  ginv <- identity
  ginv_prime <- function(x) 1
  ginv_prime2 <- function(x) 0

  if (grepl("theta_var|psi_var", matrix)) {
    g <- log
    g_prime <- function(x) 1 / x
    ginv <- exp
    ginv_prime <- exp
    ginv_prime2 <- exp
  }

  if (grepl("theta_cor|theta_cov|psi_cor|psi_cov", matrix)) {
    g <- atanh
    g_prime <- function(x) 1 / (1 - x^2)
    ginv <- tanh
    ginv_prime <- function(x) 1 - tanh(x)^2
    ginv_prime2 <- function(x) -2 * safe_tanh(x) * (1 - safe_tanh(x)^2)
  }

  return(list(
    g = g,
    g_prime = g_prime,
    ginv = ginv,
    ginv_prime = ginv_prime,
    ginv_prime2 = ginv_prime2
  ))
}

# Level of each row of a two-level parameter table as an index (1, 2), with 0
# for rows outside the levels such as := rows. lavaan keeps the level as the
# syntax writes it, a number ("level: 1") or a name ("level: within").
partable_level_index <- function(pt) {
  lvl <- pt$level
  in_level <- if (is.character(lvl)) nzchar(lvl) else lvl > 0L
  in_level <- in_level & !pt$op %in% c("==", "<", ">", ":=")
  match(lvl, unique(lvl[in_level]), nomatch = 0L)
}

# Level names of a two-level parameter table, in level order
partable_level_labels <- function(pt) {
  idx <- partable_level_index(pt)
  pt$level[match(seq_len(max(idx)), idx)]
}

inlavaanify_partable <- function(
  pt,
  dp = priors_for(),
  lavdata,
  lavoptions
) {
  nlevels <- lavdata@nlevels
  is_multilvl <- nlevels > 1
  if (is_multilvl && lavdata@ngroups > 1) {
    cli_abort("Multigroup two-level models are not supported.")
  }
  if (is_multilvl) {
    pt$group <- partable_level_index(pt)
  }
  ngroups <- max(pt$group)
  std_ov <- lavoptions$std.ov
  std_lv <- lavoptions$std.lv
  pt$mat <- NA

  for (g in c(0, seq_len(ngroups))) {
    # Identify stuff
    if (is_multilvl) {
      ov.names <- lavdata@ov.names[[1]]
    } else {
      ov.names <- if (g == 0) NULL else lavdata@ov.names[[g]]
    }
    pt$mat[pt$group == g] <- mapply(
      partable_classify_sem_matrix,
      lhs = pt$lhs[pt$group == g],
      op = pt$op[pt$group == g],
      rhs = pt$rhs[pt$group == g],
      MoreArgs = list(ov.names = ov.names, std.ov = std_ov, std.lv = std_lv),
      SIMPLIFY = TRUE
    )
  }
  pt$mat <- unlist(pt$mat)

  # Add priors
  # Note: Possible to add own non-standard priors, but the evaluation of
  # prior_logdens() will return an error.
  user_prior <- pt$prior
  pt$prior <- mapply(
    partable_prior_from_row,
    matrix = pt$mat,
    lhs = pt$lhs,
    rhs = pt$rhs,
    op = pt$op,
    MoreArgs = list(dp = dp),
    USE.NAMES = FALSE
  )
  pt$prior[pt$free == 0L | duplicated(pt$free)] <- NA_character_
  if (!is.null(user_prior)) {
    where_user_prior <- user_prior != ""
    pt$prior[where_user_prior] <- user_prior[where_user_prior]

    # Parameters held equal use the prior of their first row, so a prior written
    # on another row of the class moves there.
    shared <- unique(pt$free[pt$free > 0L & duplicated(pt$free)])
    for (f in shared) {
      rows <- which(pt$free == f)
      priors <- unique(user_prior[rows][user_prior[rows] != ""])
      if (length(priors) > 1L) {
        cli_abort(c(
          "Parameters held equal have different priors.",
          "x" = "{.code {partable_row_name(pt, rows)}}: {.code {priors}}.",
          "i" = "Give a prior on one of them only."
        ))
      }
      if (length(priors) == 1L) {
        pt$prior[rows] <- NA_character_
        pt$prior[rows[1L]] <- priors
      }
    }
  }

  # Add transformations to unrestricted parameter space
  tmp <- lapply(pt$mat, partable_transform_funcs)
  pt$g <- lapply(tmp, `[[`, "g")
  pt$g_prime <- lapply(tmp, `[[`, "g_prime")
  pt$ginv <- lapply(tmp, `[[`, "ginv")
  pt$ginv_prime <- lapply(tmp, `[[`, "ginv_prime")
  pt$ginv_prime2 <- lapply(tmp, `[[`, "ginv_prime2")

  # Compute starting values in unrestricted space. A covariance is held as a
  # correlation there, so its start is lavaan's start covariance over the start
  # standard deviations.
  start <- pt$start
  grp <- if ("level" %in% names(pt)) partable_level_index(pt) else pt$group
  for (j in which(grepl("cov", pt$mat))) {
    var_start <- vapply(
      c(pt$lhs[j], pt$rhs[j]),
      function(v) {
        pt$start[pt$lhs == v & pt$op == "~~" & pt$rhs == v & grp == grp[j]][1L]
      },
      numeric(1)
    )
    rho <- start[j] / sqrt(prod(var_start))
    start[j] <- if (is.finite(rho)) max(min(rho, 0.99), -0.99) else 0
  }
  pt$parstart <- mapply(
    function(fun, val) fun(val),
    pt$g,
    start,
    USE.NAMES = FALSE
  )

  # Names as coef() gives them: the label if any, else lhs op rhs with a group
  # (".g2") or level (".l2", or ".lbetween" for a named level) suffix after the
  # first. pt$group holds the level index in a two-level model, and := rows sit
  # in group 0.
  pt$names <- paste0(pt$lhs, pt$op, pt$rhs)
  later <- pt$group > 1
  suffix <- if (is_multilvl) {
    paste0(".l", pt$level)
  } else {
    paste0(".g", pt$group)
  }
  pt$names[later] <- paste0(pt$names[later], suffix[later])
  where_label <- pt$label != ""
  pt$names[where_label] <- pt$label[where_label]

  is_def <- pt$op == ":="
  clash <- intersect(pt$lhs[is_def], pt$label[!is_def])
  if (length(clash) > 0) {
    cli_abort(
      "Defined parameter{?s} {.code {clash}} reuse{?s/} a parameter label."
    )
  }

  # Remove pt$group if multilevel (interacts with lav_partable_labels())
  if (is_multilvl) {
    pt$group <- NULL
  }

  # FIXME: Perhaps add a 'inlavaan_partable' class to this object
  as.list(pt)
}
