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
