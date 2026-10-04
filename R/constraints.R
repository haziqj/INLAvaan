# INLAvaan holds parameters equal only by giving them one free index (lavaan's
# ceq.simple packing). lavaan turns that packing off for the whole table once
# the model has an explicit constraint, so shared labels and group.equal then
# also arrive as `.pN. == .pM.` rows. pack_constraints() rewrites what it can
# honour: `a == b` becomes a shared free index and `a == 0.5` (or `a` equal to a
# fixed parameter) a fixed value. Bounds that the parameterisation already
# guarantees (a variance above zero) are dropped, and anything else is refused.
# It returns NULL when the table has no constraint rows, else a parameter table
# for lavaan to rebuild the model from.
pack_constraints <- function(pt, effect_coding = "") {
  if (any(nzchar(effect_coding))) {
    cli_abort(c(
      "{.arg effect.coding} is not supported.",
      "i" = "Use marker-variable scaling (the default) or {.code std.lv = TRUE}."
    ))
  }

  con_idx <- which(pt$op %in% c("==", "<", ">"))
  if (length(con_idx) == 0L) {
    return(NULL)
  }

  # Each `==` either joins two free parameters or fixes one at a value
  bad <- character()
  pairs <- matrix(integer(), 0L, 2L)
  fixes <- list()
  for (i in con_idx) {
    txt <- paste(pt$lhs[i], pt$op[i], pt$rhs[i])
    lhs <- constraint_side(pt, pt$lhs[i])
    rhs <- constraint_side(pt, pt$rhs[i])
    types <- c(lhs$type, rhs$type)
    if (pt$op[i] != "==") {
      if (!implied_bound(pt, pt$op[i], lhs, rhs)) {
        bad[txt] <- "inequality constraints are not supported"
      }
      next
    }
    if (all(types == "free")) {
      pairs <- rbind(pairs, c(pt$free[lhs$row], pt$free[rhs$row]))
    } else if (setequal(types, c("free", "value"))) {
      side <- if (lhs$type == "free") lhs else rhs
      value <- if (lhs$type == "value") lhs$value else rhs$value
      fixes[[length(fixes) + 1L]] <- list(
        free = pt$free[side$row],
        value = value,
        txt = txt
      )
    } else if (all(types == "value")) {
      if (!isTRUE(all.equal(lhs$value, rhs$value))) {
        bad[txt] <- "both sides are fixed, at different values"
      }
    } else if ("defined" %in% types) {
      bad[txt] <- "constraints on defined parameters are not supported"
    } else {
      bad[txt] <- "each side must be a single parameter or a number"
    }
  }

  # Join the free indices of each pair, then fix whole classes at a value
  root <- seq_len(max(pt$free))
  find <- function(f) {
    while (root[f] != f) {
      f <- root[f]
    }
    f
  }
  for (k in seq_len(nrow(pairs))) {
    a <- find(pairs[k, 1L])
    b <- find(pairs[k, 2L])
    root[max(a, b)] <- min(a, b)
  }
  root <- vapply(seq_along(root), find, integer(1))
  fixed_at <- rep(NA_real_, length(root))
  fixed_by <- character(length(root))
  for (fx in fixes) {
    r <- root[fx$free]
    if (!is.na(fixed_at[r]) && !isTRUE(all.equal(fixed_at[r], fx$value))) {
      bad[paste0(fixed_by[r], "\t", fx$txt)] <- paste(
        "a parameter cannot be fixed at both",
        fixed_at[r],
        "and",
        fx$value
      )
    }
    fixed_at[r] <- fx$value
    fixed_by[r] <- fx$txt
  }

  if (length(bad) > 0L) {
    # A clash between two constraints is keyed by both, separated by a tab
    txt <- vapply(
      strsplit(names(bad), "\t", fixed = TRUE),
      function(x) paste0("{.code ", cli_escape(x), "}", collapse = " and "),
      character(1)
    )
    bullets <- paste0(txt, ": ", bad)
    names(bullets) <- rep("x", length(bullets))
    n_con <- length(unlist(strsplit(names(bad), "\t", fixed = TRUE)))
    cli_abort(c(
      "INLAvaan cannot honour {cli::qty(n_con)}{?this/these} constraint{?s}.",
      bullets,
      "i" = "Supported: {.code a == b} and {.code a == <number>}, where
             {.code a} and {.code b} are model parameters."
    ))
  }
  idx <- which(pt$free > 0L)
  cls <- root[pt$free[idx]]
  to_fix <- !is.na(fixed_at[cls])
  value <- fixed_at[cls[to_fix]]
  pt$ustart[idx[to_fix]] <- value
  pt$start[idx[to_fix]] <- value
  pt$est[idx[to_fix]] <- value
  pt$free[idx[to_fix]] <- 0L
  pt$free[idx[!to_fix]] <- match(cls[!to_fix], sort(unique(cls[!to_fix])))

  # lavaan allows only free parameters in a := definition, so a parameter fixed
  # here enters the definitions as its value.
  def_rows <- which(pt$op == ":=")
  for (k in which(to_fix)) {
    r <- idx[k]
    for (nm in setdiff(c(pt$label[r], pt$plabel[r]), "")) {
      pattern <- paste0(
        "(?<![[:alnum:]._])",
        gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", nm),
        "(?![[:alnum:]._])"
      )
      pt$rhs[def_rows] <- gsub(
        pattern,
        paste0("(", deparse(fixed_at[cls[k]]), ")"),
        pt$rhs[def_rows],
        perl = TRUE
      )
    }
  }

  # lavaan keeps shared free indices only in a table without constraint rows
  pt <- lapply(pt, `[`, -con_idx)
  pt$id <- seq_along(pt$id)
  pt
}

# What one side of a constraint refers to: a free parameter (its row), a value
# (a number, or a fixed parameter), a defined parameter, or anything else.
constraint_side <- function(pt, s) {
  s <- trimws(s)
  num <- suppressWarnings(as.numeric(s))
  if (!is.na(num)) {
    return(list(type = "value", value = num))
  }
  is_par <- !pt$op %in% c(":=", "==", "<", ">")
  rows <- which(is_par & (pt$label == s | pt$plabel == s))
  if (length(rows) > 0L) {
    r <- rows[1L]
    if (pt$free[r] > 0L) {
      return(list(type = "free", row = r))
    }
    value <- if (is.na(pt$ustart[r])) pt$start[r] else pt$ustart[r]
    return(list(type = "value", value = value))
  }
  if (s %in% pt$lhs[pt$op == ":="]) {
    return(list(type = "defined"))
  }
  list(type = "other")
}

# Whether `lhs op rhs` only says that a variance is above a value of at most
# zero, which the log transformation already guarantees.
implied_bound <- function(pt, op, lhs, rhs) {
  if (lhs$type == "free" && rhs$type == "value" && op == ">") {
    side <- lhs
    value <- rhs$value
  } else if (lhs$type == "value" && rhs$type == "free" && op == "<") {
    side <- rhs
    value <- lhs$value
  } else {
    return(FALSE)
  }
  r <- side$row
  pt$op[r] == "~~" && pt$lhs[r] == pt$rhs[r] && value <= 0
}

# Rows that share a free index share one internal coordinate, which pars_to_x()
# maps with the first row's transformation. That is exact only when every row
# has the same kind of transformation.
check_packed_kinds <- function(pt) {
  kind <- rep(NA_character_, length(pt$mat))
  kind[pt$mat %in% c("lambda", "beta", "nu", "alpha", "tau")] <- "identity"
  kind[pt$mat %in% c("theta_var", "psi_var")] <- "variance"
  kind[pt$mat %in% c("theta_cor", "psi_cor")] <- "correlation"
  kind[pt$mat %in% c("theta_cov", "psi_cov")] <- "covariance"
  shared <- unique(pt$free[pt$free > 0L & duplicated(pt$free)])
  bad <- character()
  for (f in shared) {
    rows <- which(pt$free == f)
    if (anyNA(kind[rows]) || length(unique(kind[rows])) > 1L) {
      bad <- c(bad, paste(partable_row_name(pt, rows), collapse = " = "))
    }
  }
  if (length(bad) > 0L) {
    bullets <- paste0("{.code ", cli_escape(bad), "}")
    names(bullets) <- rep("x", length(bullets))
    cli_abort(c(
      "INLAvaan cannot hold these parameters equal.",
      bullets,
      "i" = "Equal parameters must all be loadings, regressions, intercepts or
             thresholds (in any mix), or else all variances, all
             correlations or all covariances."
    ))
  }
  invisible(pt)
}

# Rows of a parameter table as text for messages (lhs op rhs), with the group or
# level when there is more than one.
partable_row_name <- function(pt, rows) {
  nm <- paste0(pt$lhs[rows], pt$op[rows], pt$rhs[rows])
  if (is.null(pt$level)) {
    block <- label <- pt$group
    unit <- "group"
  } else {
    block <- partable_level_index(pt)
    label <- pt$level
    unit <- "level"
  }
  if (max(block) > 1L) {
    nm <- paste0(nm, " (", unit, " ", label[rows], ")")
  }
  nm
}
