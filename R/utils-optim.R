# Simple central-difference Jacobian of a gradient function.
# Does 2m evaluations vs numDeriv::jacobian's ~4m (Richardson extrapolation).
fast_jacobian <- function(gr_fn, x, h = 1e-5) {
  m <- length(x)
  H <- matrix(0, m, m)
  for (j in seq_len(m)) {
    xp <- xm <- x
    xp[j] <- x[j] + h
    xm[j] <- x[j] - h
    H[, j] <- (gr_fn(xp) - gr_fn(xm)) / (2 * h)
  }
  H
}

# Central-difference Hessian of a scalar function (no analytic gradient needed).
# Does 4m(m-1)/2 + 2m = 2m^2 evaluations of fn.
fast_hessian <- function(fn, x, h = 1e-5) {
  m <- length(x)
  f0 <- fn(x)
  H <- matrix(0, m, m)

  # Diagonal: (f(x+h*ei) - 2*f(x) + f(x-h*ei)) / h^2
  for (i in seq_len(m)) {
    xp <- xm <- x
    xp[i] <- x[i] + h
    xm[i] <- x[i] - h
    H[i, i] <- (fn(xp) - 2 * f0 + fn(xm)) / (h^2)
  }

  # Off-diagonal (symmetric): use the cross-partial formula
  # (f(x+h*ei+h*ej) - f(x+h*ei-h*ej) - f(x-h*ei+h*ej) + f(x-h*ei-h*ej)) / (4h^2)
  if (m > 1) {
    for (i in seq_len(m - 1)) {
      for (j in (i + 1):m) {
        xpp <- xpm <- xmp <- xmm <- x
        xpp[c(i, j)] <- x[c(i, j)] + h
        xmm[c(i, j)] <- x[c(i, j)] - h
        xpm[i] <- x[i] + h; xpm[j] <- x[j] - h
        xmp[i] <- x[i] - h; xmp[j] <- x[j] + h
        H[i, j] <- H[j, i] <-
          (fn(xpp) - fn(xpm) - fn(xmp) + fn(xmm)) / (4 * h^2)
      }
    }
  }

  H
}

# Central-difference gradient of a scalar function.
# Does 2m evaluations of fn.
fast_grad <- function(fn, x, h = 1e-5) {
  m <- length(x)
  g <- numeric(m)
  for (j in seq_len(m)) {
    xp <- xm <- x
    xp[j] <- x[j] + h
    xm[j] <- x[j] - h
    g[j] <- (fn(xp) - fn(xm)) / (2 * h)
  }
  g
}
