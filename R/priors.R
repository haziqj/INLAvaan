#' Density of a Beta distribution on a bounded interval
#'
#' @inheritParams stats::dbeta
#' @param x A numeric vector of quantiles.
#' @param a The lower bound of the interval.
#' @param b The upper bound of the interval.
#' @param log Logical; if TRUE, probabilities p are given as log(p).
#'
#' @returns A numeric vector of density values.
#' @export
#'
#' @examples
#' # Beta(2,5) on (0,100)
#' x <- seq(0, 100, length.out = 100)
#' y <- dbeta_box(x, shape1 = 2, shape2 = 5, a = 0, b = 100)
#' plot(x, y, type = "l", main = "Beta(2,5) on (0,100)")
#'
#' # Beta(1,1) i.e. uniform on (-1, 1)
#' x <- seq(-1, 1, length.out = 100)
#' y <- dbeta_box(x, shape1 = 1, shape2 = 1, a = -1, b = 1)
#' plot(x, y, type = "l", main = "Beta(1,1) on (-1,1)")
dbeta_box <- function(x, shape1, shape2, a, b, log = FALSE) {
  # basic checks
  if (
    !is.numeric(a) ||
      !is.numeric(b) ||
      length(a) != 1 ||
      length(b) != 1 ||
      !is.finite(a) ||
      !is.finite(b) ||
      b <= a
  ) {
    stop("Require finite scalars with b > a.")
  }

  # transform to (0,1)
  u <- (x - a) / (b - a)
  inside <- (x >= a) & (x <= b)

  # init output
  out <- if (log) rep(-Inf, length(x)) else rep(0, length(x))

  # evaluate where inside support
  if (any(inside, na.rm = TRUE)) {
    if (log) {
      out[inside] <- -log(b - a) + dbeta(u[inside], shape1, shape2, log = TRUE)
    } else {
      out[inside] <- dbeta(u[inside], shape1, shape2) / (b - a)
    }
  }

  # propagate NAs from x
  out[is.na(x)] <- NA_real_
  out
}
