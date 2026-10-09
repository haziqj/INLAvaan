# Random-slope support. lavaan's `rv()` modifier turns a level-1 regression
# coefficient into a level-2 latent variable, and its likelihood is built
# from a per-cluster kernel that lives in the lavaan cache rather than in
# the model-implied moments. Everything downstream that needs to know about
# that kernel goes through rs_spec() below.

# lavaan refuses its own test statistics for random-slope models and says so.
# Under `do.fit = FALSE` they are never computed anyway, and INLAvaan never
# reads them, so the warning is noise in the setup calls.
muffle_rs_test_warning <- function(expr) {
  withCallingHandlers(expr, warning = function(cond) {
    msg <- conditionMessage(cond)
    if (grepl("random slopes", msg) && grepl("test set to", msg)) {
      invokeRestart("muffleWarning")
    }
  })
}

# Resolve the random-slope description of a fitted INLAvaan internal list.
# Returns NULL when the fit has no random slopes, so callers can gate with
# `if (!is.null(spec <- rs_spec(int)))`.
rs_spec <- function(int) {
  lavmodel <- int$lavmodel
  if (!has_random_slopes(lavmodel)) {
    return(NULL)
  }
  rs <- int$lavcache[[1L]]$rs
  if (is.null(rs)) {
    cli_abort(
      c(
        "This fit has random slopes but no stored random-slope cache.",
        "x" = "The per-cluster likelihood kernel cannot be rebuilt without
               it.",
        "i" = "Refit with the current version of INLAvaan."
      ),
      class = "inlavaan_rs_cache"
    )
  }
  # What the kernel conditions on, and what it scores. `x.names` are the
  # level-1 covariates carrying the slopes and `exo.b.names` the
  # between-level exogenous variables, both of which the kernel takes as
  # given. `zb.names` are the between-only endogenous variables, which it
  # models and which therefore belong to the response set, alongside
  # `y.names` -- on the quadrature route the latter already includes a
  # split covariate, which the kernel scores jointly with the outcomes.
  # (`yb.names`, the between-level responses, is a subset of `y.names`.)
  cond <- unique(c(rs$info$x.names, rs$info$exo.b.names))
  cond <- cond[nzchar(cond)]
  resp <- unique(c(rs$info$y.names, rs$info$zb.names))
  resp <- resp[nzchar(resp)]
  list(
    rs = rs,
    slopes = c(names(lavmodel@rv.ov), names(lavmodel@rv.lv)),
    route = if (isTRUE(rs$info$nl.flag)) "B" else "A",
    ngh = rs$info$ngh,
    ncl = rs$stats$nclusters,
    nobs = rs$stats$cluster.size,
    cond = cond,
    resp = resp
  )
}

# The covariates a random-slope fit's evidence and LOO condition on, and the
# variables they score. The kernel scores a covariate observed at both levels
# with the outcomes, but the fixed.x shift on the marginal likelihood and DIC,
# and loco_rs_split_const() in the LOO, take its frozen density out again. So
# every observed exogenous covariate ends up conditioned on.
rs_scored_sets <- function(int, spec) {
  ov_x <- unlist(int$lavdata@ov.names.x)
  list(
    cond = union(spec$cond, intersect(spec$resp, ov_x)),
    resp = setdiff(spec$resp, ov_x)
  )
}

# lavaan builds a random-slope derivative from the packed free parameters
# (`lavmodel@nx.free`, one entry per equality group), where every other
# model hands back one entry per free partable row (`lavmodel@nx.unco`,
# with a duplicate for each further member of a group). INLAvaan's chain
# rule works in that unpacked space -- it multiplies row u by
# `jcb[u] * sd1sd2[u]`, adds the off-diagonal variance-into-covariance
# terms, and only then repacks with `%*% K` -- so a packed random-slope
# derivative is scattered back onto the first row of each group, leaving
# the duplicates at zero. `g` is either a gradient vector or a matrix of
# per-cluster scores with the parameters in its columns.
#
# The scatter is exact whenever `jcb * sd1sd2` is constant within a group,
# because the repack sums the group again:
#   sum_u jcb[u] * sd1sd2[u] * dl/dx_u = jcb[1] * sd1sd2[1] * sum_u dl/dx_u,
# and `sum_u dl/dx_u` is exactly the packed element lavaan returns. The
# factor is constant when every member carries the same transformation
# (all identity, or all log with one shared value of theta) and none is a
# covariance, whose `sd1sd2` differs from row to row and whose gradient is
# read again by the off-diagonal terms. check_rs_ceq() refuses the rest at
# fit time.
rs_unpack_grad <- function(g, lavmodel) {
  K <- lavmodel@ceq.simple.K
  first <- apply(K, 2L, function(col) which(col != 0)[1L])
  if (is.matrix(g)) {
    out <- matrix(0, nrow = nrow(g), ncol = nrow(K))
    out[, first] <- g
  } else {
    out <- numeric(nrow(K))
    out[first] <- g
  }
  out
}

# Is a derivative lavaan just returned in the packed random-slope
# convention? `n` is its length (a gradient) or its number of columns (a
# score matrix).
rs_grad_is_packed <- function(n, lavmodel) {
  has_random_slopes(lavmodel) &&
    lavmodel@nx.free < lavmodel@nx.unco &&
    n == lavmodel@nx.free
}

# The transformation a free parameter is optimised under, in the three
# classes partable_transform_funcs() distinguishes.
rs_transform_class <- function(mat) {
  out <- rep("identity", length(mat))
  out[grepl("theta_var|psi_var", mat)] <- "log"
  out[grepl("theta_cor|theta_cov|psi_cor|psi_cov", mat)] <- "atanh"
  out
}

# Fit-time gate for the equality constraints rs_unpack_grad() cannot
# redistribute exactly. check_packed_kinds() has already refused groups that
# mix kinds of parameter, which leaves groups of covariances or correlations.
check_rs_ceq <- function(pt, lavmodel) {
  if (!isTRUE(lavmodel@ceq.simple.only)) {
    return(invisible(NULL)) # nocov -- general constraints never reach here
  }
  free <- pt$free[pt$free > 0L]
  groups <- unique(free[duplicated(free)])
  if (length(groups) == 0L) {
    return(invisible(NULL))
  }
  cls <- rs_transform_class(pt$mat)
  bad <- vapply(
    groups,
    function(gr) any(cls[pt$free == gr] == "atanh"),
    logical(1)
  )
  if (!any(bad)) {
    return(invisible(NULL))
  }
  rows <- which(pt$free %in% groups[bad])
  bad_names <- unique(pt$names[rows])
  cli_abort(
    c(
      "Random-slope models cannot hold covariances equal:
       {.val {bad_names}}.",
      "i" = "Hold loadings, regressions, intercepts or variances equal
             instead, or drop the constraint."
    ),
    class = "inlavaan_rs_ceq"
  )
}

# lavaan's quadrature route integrates over the distribution of each slope,
# and its kernel is not finite when a slope variance is fixed at zero.
check_rs_zero_var <- function(lavpartable, lavmodel) {
  pt <- lavpartable
  slopes <- c(names(lavmodel@rv.ov), names(lavmodel@rv.lv))
  zero <- pt$op == "~~" &
    pt$lhs == pt$rhs &
    pt$lhs %in% slopes &
    pt$free == 0L &
    pt$start == 0
  if (!any(zero)) {
    return(invisible(NULL))
  }
  cli_abort(
    c(
      "A random slope on a latent or split covariate cannot have its
       variance fixed at zero: {.val {unique(pt$lhs[zero])}}.",
      "i" = "Fit the fixed-slope model instead (the same path without
             {.code rv()})."
    ),
    class = "inlavaan_rs_zero_var"
  )
}

# Gate for what the moment-based methods still cannot give a random-slope
# fit: the residual types scaled by asymptotic standard errors, which need
# the model's derivatives of the moments.
check_rs_moments <- function(object, fn, type) {
  if (!has_random_slopes(object@external$inlavaan_internal$lavmodel)) {
    return(invisible(NULL))
  }
  is_fitted <- fn %in% c("fitted", "fitted.values")
  ok <- rs_is_casewise(type, is_fitted) ||
    if (is_fitted) type == "moments" else !is.na(rs_residual_type(type))
  if (ok) {
    return(invisible(NULL))
  }
  hint <- if (is_fitted) {
    "Use {.code type = \"moments\"} or {.code \"casewise\"}."
  } else {
    "Use {.code type = \"raw\"}, {.code \"cor\"}, {.code \"cor.bentler\"}
     or {.code \"casewise\"}."
  }
  cli_abort(
    c(
      "{.fn {fn}} with {.code type = \"{type}\"} is not available for a
       random-slope model.",
      "i" = hint
    ),
    class = "inlavaan_rs_moments"
  )
}

# The residual types a random-slope fit supports, in lavaan's canonical
# spelling, and NA for the others. As in lavaan, "cor" means "cor.bentler"
# for a fit that mimics EQS and "cor.bollen" otherwise.
rs_residual_type <- function(type, mimic = "lavaan") {
  type <- gsub("_", ".", type)
  alias <- c(
    raw = "raw",
    rmr = "raw",
    cor = if (identical(mimic, "EQS")) "cor.bentler" else "cor.bollen",
    cor.bollen = "cor.bollen",
    crmr = "cor.bollen",
    cor.bentler = "cor.bentler",
    cor.eqs = "cor.bentler",
    srmr = "cor.bentler"
  )
  unname(alias[type])
}

# The casewise types and their aliases, as lavaan names them for fitted() and
# for residuals()
rs_is_casewise <- function(type, is_fitted) {
  aliases <- if (is_fitted) {
    c("casewise", "obs", "ov")
  } else {
    c("casewise", "case", "obs", "observations", "ov")
  }
  type %in% aliases
}

# `per_cluster = TRUE` needs the per-cluster kernel of a random-slope fit
check_per_cluster <- function(object, per_cluster) {
  if (!isTRUE(per_cluster)) {
    return(invisible(FALSE))
  }
  if (!has_random_slopes(object@external$inlavaan_internal$lavmodel)) {
    cli_abort(
      "{.code per_cluster = TRUE} is available for random-slope models
       only.",
      class = "inlavaan_per_cluster"
    )
  }
  invisible(TRUE)
}

# summary() leaves out what the averaged moments cannot give, rather than
# stopping halfway through the table. Returns NULL for the caller to test.
warn_rs_left_out <- function(what) {
  cli_warn(
    c(
      "Leaving out the {what} of this random-slope model.",
      "x" = "An outcome observed at level 1 only has no place for the
             between-cluster variance its random slope adds.",
      "i" = "Run {.fn fitted} for the details."
    ),
    class = "inlavaan_rs_within_only"
  )
  NULL
}

# Route B (a random slope on a latent or split covariate) replaces the
# closed-form cluster kernel with Gauss-Hermite quadrature, so it is both
# slower and less accurate. Warn once wherever that route is entered.
warn_rs_route_b <- function(spec) {
  cli_warn(
    c(
      "Random slopes on a latent or split covariate are integrated by
       Gauss-Hermite quadrature with {spec$ngh} node{?s} per dimension.",
      "x" = "This route is slower and less extensively validated than the
             closed form.",
      "i" = "{.arg integration.ngh} (passed through to {.pkg lavaan}) trades
             accuracy for speed."
    ),
    class = "inlavaan_rs_route_b"
  )
  invisible(NULL)
}
