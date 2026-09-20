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
  cond <- unique(c(rs$info$x.names, rs$info$exo.b.names, rs$info$zb.names))
  cond <- cond[nzchar(cond)]
  list(
    rs = rs,
    slopes = c(names(lavmodel@rv.ov), names(lavmodel@rv.lv)),
    route = if (isTRUE(rs$info$nl.flag)) "B" else "A",
    ngh = rs$info$ngh,
    ncl = rs$stats$nclusters,
    nobs = rs$stats$cluster.size,
    cond = cond
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
