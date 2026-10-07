# Rows that lavaan derives from the composite weights inside
# lav_model_set_parameters(): the variance of each composite (its residual
# variance when the composite is endogenous) and its intercept. lavaan keeps
# them fixed unless the syntax frees one with NA*, which INLAvaan refuses (see
# check_composite_derived()). Their values in a parameter table built with
# do.fit = FALSE are those at the start weights.
composite_derived_rows <- function(pt, include_free = FALSE) {
  comp <- unique(pt$lhs[pt$op == "<~"])
  which(
    (include_free | pt$free == 0L) &
      pt$lhs %in% comp &
      ((pt$op == "~~" & pt$lhs == pt$rhs) | pt$op == "~1")
  )
}

# The weight rows and the indicator covariance block T of each composite, per
# group (or level), so that pars_to_x() can scale a covariance with a composite
# by Var(C) = w'Tw at the current weights. lavaan fixes T at the sample
# covariances, which are the start values of its rows, and leaves the entry of
# a pair without a row at zero.
composite_blocks <- function(pt) {
  grp <- if ("level" %in% names(pt)) partable_level_index(pt) else pt$group
  is_w <- pt$op == "<~"
  out <- list()
  for (g in unique(grp[is_w])) {
    for (cname in unique(pt$lhs[is_w & grp == g])) {
      wrows <- which(is_w & pt$lhs == cname & grp == g)
      ind <- pt$rhs[wrows]
      tmat <- matrix(0, length(ind), length(ind))
      for (a in seq_along(ind)) {
        for (b in seq_len(a)) {
          r <- which(
            pt$op == "~~" &
              grp == g &
              ((pt$lhs == ind[a] & pt$rhs == ind[b]) |
                (pt$lhs == ind[b] & pt$rhs == ind[a]))
          )
          if (length(r) > 0L) {
            tmat[a, b] <- tmat[b, a] <- pt$start[r[1L]]
          }
        }
      }
      out[[length(out) + 1L]] <- list(
        name = cname,
        group = g,
        wrows = wrows,
        tmat = tmat,
        vrow = which(
          pt$lhs == cname & pt$op == "~~" & pt$rhs == cname & grp == g
        )
      )
    }
  }
  out
}

# Composite models INLAvaan cannot fit yet. Two-level models come first,
# because lavaan estimates the indicator covariances there (composites.cov =
# "free"). Ordinal data must stop before the PML refit, whose theta
# parameterisation lavaan rejects for composites.
check_composite_scope <- function(fit0) {
  if (!any(fit0@ParTable$op == "<~")) {
    return(invisible(NULL))
  }
  if (fit0@Data@nlevels > 1L) {
    cli_abort(
      "INLAvaan does not support composites ({.code <~}) in two-level models
       yet."
    )
  }
  if (identical(fit0@Options$composites.cov, "free")) {
    cli_abort(c(
      "INLAvaan does not support {.code composites.cov = \"free\"} yet.",
      "i" = "Use the default, which fixes the covariances of each composite's
             indicators at their sample values."
    ))
  }
  if (length(lavaan::lavNames(fit0, "ov.ord")) > 0L) {
    cli_abort(
      "INLAvaan does not support composites ({.code <~}) with ordinal data
       yet."
    )
  }
  invisible(NULL)
}

# lavaan overwrites a freed composite variance or intercept with its derived
# value, so the likelihood is flat along that parameter while lavaan's gradient
# for it is not zero.
check_composite_derived <- function(pt) {
  rows <- composite_derived_rows(pt, include_free = TRUE)
  rows <- rows[pt$free[rows] > 0L]
  if (length(rows) == 0L) {
    return(invisible(NULL))
  }
  bullets <- paste0("{.code ", cli_escape(partable_row_name(pt, rows)), "}")
  names(bullets) <- rep("x", length(bullets))
  cli_abort(c(
    "The variance and intercept of a composite are set by its weights, so they
     cannot be free:",
    bullets,
    "i" = "Remove the {.code NA*} modifier."
  ))
}

# A label on a derived composite row cannot tie it to anything: lavaan then
# fixes the free rows that share the label, and a constraint that uses it would
# hold a parameter at the value of the start weights. lavaan ties rows by their
# effective label, the user's label or else lhs op rhs (with .gN in a later
# group), so equal("C~~C") on another row ties it as well.
check_composite_labels <- function(pt) {
  rows <- composite_derived_rows(pt)
  if (length(rows) == 0L) {
    return(invisible(NULL))
  }
  is_con <- pt$op %in% c("==", "<", ">", ":=")
  group <- if (is.null(pt$group)) rep(1L, length(pt$lhs)) else pt$group
  eff <- pt$label
  default <- paste0(
    pt$lhs,
    pt$op,
    pt$rhs,
    ifelse(group > 1L, paste0(".g", group), "")
  )
  eff[!nzchar(eff)] <- default[!nzchar(eff)]
  others <- setdiff(which(!is_con), rows)
  tied <- intersect(eff[rows], eff[others])
  con_names <- unlist(lapply(
    c(pt$lhs[is_con & pt$op != ":="], pt$rhs[is_con & pt$op != ":="]),
    function(e) tryCatch(all.vars(str2lang(e)), error = function(err) e)
  ))
  constrained <- intersect(c(eff[rows], pt$plabel[rows]), con_names)
  bad <- unique(c(tied, constrained))
  if (length(bad) == 0L) {
    return(invisible(NULL))
  }
  cli_abort(c(
    "The variances and intercepts of composites are set by the weights, so
     their labels cannot be shared with other parameters or used in
     constraints.",
    "x" = "{cli::qty(length(bad))}Label{?s} {.code {bad}} name{?s/}
           {?such a row/such rows}.",
    "i" = "{cli::qty(length(bad))}Remove the label{?s}, or any constraint that
           uses {?it/them}."
  ))
}

# lavaan scales a composite by a weight fixed at 1 and finds that weight by its
# value. Without one it fixes Var(C) at 1, so the composite is no longer w'x
# and the scale of its weights is not identified. A weight fixed at 0 drops its
# indicator from the composite's block but not from the covariances lavaan
# fixes for it, and lavaan also accepts an indicator shared by two composites.
# Neither is represented consistently.
check_composite_weights <- function(pt) {
  is_w <- pt$op == "<~"
  if (!any(is_w)) {
    return(invisible(NULL))
  }
  zero <- which(is_w & pt$free == 0L & pt$ustart %in% 0)
  if (length(zero) > 0L) {
    bullets <- paste0("{.code ", cli_escape(partable_row_name(pt, zero)), "}")
    names(bullets) <- rep("x", length(bullets))
    cli_abort(c(
      "A composite weight cannot be fixed at 0:",
      bullets,
      "i" = "Leave the indicator out of the composite instead."
    ))
  }
  block <- if (is.null(pt$block)) pt$group else pt$block
  comp_key <- paste(pt$lhs, block)
  marked <- comp_key[is_w & pt$free == 0L & pt$ustart %in% 1]
  unmarked <- which(is_w & !comp_key %in% marked & !duplicated(comp_key))
  if (length(unmarked) > 0L) {
    bullets <- paste0("{.code ", cli_escape(pt$lhs[unmarked]), "}")
    if (max(block) > 1L) {
      bullets <- paste0(bullets, " in group ", block[unmarked])
    }
    bullets <- paste(bullets, "has none.")
    names(bullets) <- rep("x", length(bullets))
    ind <- pt$rhs[is_w & comp_key == comp_key[unmarked[1L]]]
    example <- paste0(
      pt$lhs[unmarked[1L]],
      " <~ 1*",
      paste(ind, collapse = " + ")
    )
    cli_abort(c(
      "Each composite needs one weight fixed at 1.",
      bullets,
      "i" = "Fix one of its weights at 1, for example {.code {example}}."
    ))
  }
  ind_key <- paste(pt$rhs, block)[is_w]
  n_owner <- tapply(pt$lhs[is_w], ind_key, function(l) length(unique(l)))
  shared <- unique(pt$rhs[is_w][ind_key %in% names(n_owner)[n_owner > 1L]])
  if (length(shared) > 0L) {
    cli_abort(c(
      "An observed variable can be an indicator of one composite only.",
      "x" = "{.code {shared}} {cli::qty(length(shared))}form{?s/} more than
             one composite."
    ))
  }
  invisible(NULL)
}

# A covariance between an indicator of a composite and a variable outside that
# composite is either fixed by lavaan at its sample value (an indicator of
# another composite) or left free (an indicator of a factor). Neither keeps the
# composite a weighted sum of its indicators once INLAvaan re-expresses it for
# predict() and sampling(), so such covariances are refused.
check_composite_covariances <- function(pt) {
  is_w <- pt$op == "<~"
  if (!any(is_w)) {
    return(invisible(NULL))
  }
  block <- if (is.null(pt$block)) pt$group else pt$block
  composite_of <- stats::setNames(
    pt$lhs[is_w],
    paste(pt$rhs[is_w], block[is_w])
  )
  lhs_comp <- unname(composite_of[paste(pt$lhs, block)])
  rhs_comp <- unname(composite_of[paste(pt$rhs, block)])
  same <- !is.na(lhs_comp) & !is.na(rhs_comp) & lhs_comp == rhs_comp
  rows <- which(
    pt$op == "~~" &
      pt$lhs != pt$rhs &
      (!is.na(lhs_comp) | !is.na(rhs_comp)) &
      !same
  )
  if (length(rows) == 0L) {
    return(invisible(NULL))
  }
  bullets <- paste0("{.code ", cli_escape(partable_row_name(pt, rows)), "}")
  names(bullets) <- rep("x", length(bullets))
  cli_abort(c(
    "INLAvaan does not support covariances between an indicator of a composite
     and a variable outside that composite:",
    bullets,
    "i" = "The indicators of a composite relate to other variables through the
           composite. Remove these covariances."
  ))
}

# lavaan fixes a composite's mean at w'nu, the weighted means of its
# indicators, so the composite's intercept absorbs any latent mean above it. A
# free latent mean whose every path to the data (along =~ and ~, leaving out
# coefficients fixed at zero) runs through a composite drops out of the
# likelihood, and its posterior would be its prior. growth() and group.equal =
# "intercepts" free such means too. Means that share a free index with an
# identified mean are fine.
check_composite_means <- function(pt, lavoptions = NULL) {
  if (!any(pt$op == "<~")) {
    return(invisible(NULL))
  }
  block <- if (is.null(pt$block)) rep(1L, length(pt$lhs)) else pt$block
  flag <- logical(length(pt$lhs))
  for (b in unique(block[pt$op == "<~"])) {
    in_b <- block == b
    comps <- unique(pt$lhs[in_b & pt$op == "<~"])
    factors <- unique(pt$lhs[in_b & pt$op == "=~"])
    nonzero <- !(pt$free == 0L & pt$ustart %in% 0)
    is_mm <- in_b & pt$op == "=~" & nonzero
    is_reg <- in_b & pt$op == "~" & nonzero
    from <- c(pt$lhs[is_mm], pt$rhs[is_reg])
    to <- c(pt$rhs[is_mm], pt$lhs[is_reg])
    rows <- which(in_b & pt$op == "~1" & pt$free > 0L & pt$lhs %in% factors)
    for (i in rows) {
      reached <- character()
      frontier <- pt$lhs[i]
      while (length(frontier) > 0L) {
        nxt <- setdiff(unique(to[from %in% frontier]), reached)
        reached <- c(reached, nxt)
        frontier <- setdiff(nxt, comps) # a composite ends the path
      }
      flag[i] <- any(reached %in% comps) &&
        all(reached %in% c(comps, factors))
    }
  }
  for (i in which(flag)) {
    flag[i] <- all(flag[pt$free == pt$free[i]])
  }
  if (!any(flag)) {
    return(invisible(NULL))
  }
  rows <- which(flag)
  bullets <- paste0("{.code ", cli_escape(partable_row_name(pt, rows)), "}")
  names(bullets) <- rep("x", length(bullets))
  hint <- NULL
  if (identical(lavoptions$model.type, "growth")) {
    hint <- c(
      "i" = "{.fn agrowth} frees the means of the growth factors. Use
             {.fn asem} with {.code meanstructure = TRUE}, which fixes them at
             zero and frees the intercepts of the indicators."
    )
  } else if (
    "intercepts" %in% lavoptions$group.equal && any(pt$user[rows] == 0L)
  ) {
    hint <- c(
      "i" = "{.code group.equal = \"intercepts\"} frees the latent means of
             later groups. Add {.val means} to {.arg group.equal}."
    )
  }
  cli_abort(c(
    "INLAvaan cannot estimate {cli::qty(length(rows))}{?this latent mean/these
     latent means}, because composites absorb {?it/them}:",
    bullets,
    "i" = "A composite's mean is set by the means of its indicators, so a
           latent mean that reaches the data only through composites does not
           enter the likelihood.",
    "i" = "Fix {cli::qty(length(rows))}{?it/them} at zero, for example
           {.code {pt$lhs[rows[1L]]} ~ 0*1}.",
    hint
  ))
}

# Start values for the free weights. lavaan starts every weight at 1, and from
# there the optimiser can settle in a poor local mode when the best weights
# have signs opposite to the marker's. Under the composite model
# T^-1 Cov(x, y) = w a' for the indicators x and every other observed variable
# y, so the first left singular vector of T^-1 S_xy, scaled to the marker, is a
# moment estimate of w. A composite keeps lavaan's starts when this is not
# finite or runs beyond 100, and a weight with a start() of the user's keeps
# that value.
composite_start_weights <- function(pt, lavsamplestats, lavdata) {
  parstart <- pt$parstart
  is_w <- pt$op == "<~"
  if (!any(is_w) || lavdata@nlevels > 1L) {
    return(parstart)
  }
  for (g in seq_len(lavdata@ngroups)) {
    S <- lavsamplestats@cov[[g]]
    h1 <- lavsamplestats@missing.h1
    if (length(h1) >= g && !is.null(h1[[g]]$sigma)) {
      S <- h1[[g]]$sigma
    }
    ovn <- lavdata@ov.names[[g]]
    for (cname in unique(pt$lhs[is_w & pt$group == g])) {
      wrows <- which(is_w & pt$lhs == cname & pt$group == g)
      ind <- match(pt$rhs[wrows], ovn)
      others <- setdiff(seq_along(ovn), ind)
      marker <- which(pt$free[wrows] == 0L & pt$ustart[wrows] %in% 1)[1L]
      if (length(others) == 0L || anyNA(ind) || is.na(marker)) {
        next
      }
      u <- tryCatch(
        svd(solve(S[ind, ind], S[ind, others, drop = FALSE]))$u[, 1L],
        error = function(e) NULL
      )
      if (is.null(u)) {
        next
      }
      w <- u / u[marker]
      free <- pt$free[wrows] > 0L & is.na(pt$ustart[wrows])
      if (all(is.finite(w)) && max(abs(w)) <= 100) {
        parstart[wrows[free]] <- w[free]
      }
    }
  }
  parstart
}
