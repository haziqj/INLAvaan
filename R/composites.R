# Rows that lavaan derives from the composite weights inside
# lav_model_set_parameters(): the variance of each composite (its residual
# variance when the composite is endogenous) and its intercept. They are never
# free, and their values in a parameter table built with do.fit = FALSE are
# those at the start weights.
composite_derived_rows <- function(pt) {
  comp <- unique(pt$lhs[pt$op == "<~"])
  which(
    pt$free == 0L &
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
