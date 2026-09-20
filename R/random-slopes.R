# Random-slope support. lavaan's `rv()` modifier turns a level-1 regression
# coefficient into a level-2 latent variable, and its likelihood is built
# from a per-cluster kernel that lives in the lavaan cache rather than in
# the model-implied moments. Everything downstream that needs to know about
# that kernel goes through rs_spec() below.

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
# redistribute exactly: a group whose members carry different
# transformations, or one holding a covariance.
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
    function(gr) {
      k <- which(pt$free == gr)
      length(unique(cls[k])) > 1L || any(cls[k] == "atanh")
    },
    logical(1)
  )
  if (!any(bad)) {
    return(invisible(NULL))
  }
  rows <- which(pt$free %in% groups[bad])
  bad_names <- unique(pt$names[rows])
  cli_abort(
    c(
      "Random-slope models do not support this equality constraint:
       {.val {bad_names}}.",
      "x" = "{.pkg lavaan} returns the random-slope gradient summed over
             the parameters a constraint ties together, and INLAvaan can
             redistribute that sum exactly only when every parameter in
             the group carries the same transformation and none of them is
             a covariance.",
      "i" = "Constrain loadings, regressions and intercepts among
             themselves, or variances among themselves, or drop the
             constraint."
    ),
    class = "inlavaan_rs_ceq"
  )
}

# Gate for the moment-based methods. Both report a single model-implied
# covariance matrix per level, which a random-slope model does not have.
check_rs_moments <- function(object, fn) {
  if (!has_random_slopes(object@external$inlavaan_internal$lavmodel)) {
    return(invisible(NULL))
  }
  cli_abort(
    c(
      "{.fn {fn}} has no model-implied moments for a random-slope model.",
      "x" = "The covariance of y depends on the covariate values, so there
             is no single within-cluster covariance matrix, and
             {.pkg lavaan}'s implied moments silently drop the slope
             variance -- the residuals would report it as misfit.",
      "i" = "Use {.code predict(object, type = \"lv\", level = 2)} for the
             cluster-level slopes."
    ),
    class = "inlavaan_rs_moments"
  )
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
