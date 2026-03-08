# Both functions below modified by Haavard Rue, April 2021
#
# Repo is https://github.com/thomasluu/sncdfinv/
#
# Copyright 2015-2016 Thomas Luu
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.
#
# plog: Computation of the Lambert W-function by Halley's Method.
#
# Initial guesses based on:
#
# D.A. Barry, J.-Y. Parlange, L. Li, H. Prommer, C.J. Cunningham, and F.
# Stagnitti. Analytical approximations for real values of the Lambert
# W-function. Mathematics and Computers in Simulation, 53(1):95-103, 2000.
#
# D.A. Barry, J.-Y. Parlange, L. Li, H. Prommer, C.J. Cunningham, and F.
# Stagnitti. Erratum to analytical approximations for real values of the Lambert
# W-function. Mathematics and computers in simulation, 59(6):543-543, 2002.
#
# GMRFLib_sn_Pinv:
#
# Based on: Luu, T; (2016) Fast and accurate parallel computation of quantile
# functions for random number generation. Doctoral thesis, UCL (University
# College London). https://discovery.ucl.ac.uk/1482128/

#' Fast Approximation of Skew-Normal Quantile Function
#'
#' A fast approximation of skew-normal quantiles using the high-performance
#' approximation algorithm from the INLA GMRFLib C source, and originally by
#' Thomas Luu (see details for reference).
#'
#' @details This function implements a high-performance approximation for the
#'   skew-normal quantile function based on the algorithm described by Luu
#'   (2016). The method uses a domain decomposition strategy to achieve high
#'   accuracy (\eqn{< 10^{-7}} relative error) without iterative numerical
#'   inversion.
#'
#'   The domain is split into two regions:
#' \itemize{
#'   \item **Tail Regions:** For extreme probabilities where \eqn{\vert u \vert} is large, the quantile
#'   is approximated using the Lambert W-function, \eqn{W(z)}, solving \eqn{z = \Phi(q)} via
#'   asymptotic expansion:
#'   \deqn{q \approx \sqrt{2 W\left(\frac{1}{2\pi (1-p)^2}\right)}}
#'   \item **Central Region:** For the main body of the distribution, the function
#'   uses a high-order Taylor expansion of the inverse error function around a
#'   carefully selected expansion point $x_0$:
#'   \deqn{\Phi^{-1}(p) \approx \sum_{k=0}^5 c_k (z - x_0)^k}
#' }
#'
#'   This approach is significantly faster than standard numerical inversion
#'   (e.g., `uniroot`) while maintaining sufficient precision for most
#'   statistical applications.
#'
#' @references Luu, T. (2016). *Fast and accurate parallel computation of
#'   quantile functions for random number generation* #' (Doctoral thesis). UCL
#'   (University College London). \url{https://discovery.ucl.ac.uk/1482128/}

#' @param p Vector of probabilities.
#' @param xi Location parameter (numeric vector).
#' @param omega Scale parameter (numeric vector).
#' @param alpha Shape parameter (numeric vector).
#'
#' @return Vector of quantiles.
#'
#' @examples
#' qsnorm_fast(c(0.025, 0.5, 0.975))
#' qsnorm_fast(c(0.025, 0.5, 0.975), xi = 2, omega = 0.5, alpha = 1)
#'
#' @export
qsnorm_fast <- function(p, xi = 0, omega = 1, alpha = 0) {
  # Recycling inputs to match length of p
  n <- length(p)
  if (length(xi) < n) {
    xi <- rep(xi, length.out = n)
  }
  if (length(omega) < n) {
    omega <- rep(omega, length.out = n)
  }
  if (length(alpha) < n) {
    alpha <- rep(alpha, length.out = n)
  }

  # Standardized quantile calculation
  z <- .qsn_std_fast(p, alpha)

  # Scale and shift
  return(xi + omega * z)
}

# Internal standardized quantile function
.qsn_std_fast <- function(u, a) {
  # Constants
  SQRT2 <- 1.4142135623730950488
  INV_SQRT_PI <- 0.564189583547756286948
  CONST_TOL <- 1.6448536269514722 # qnorm(0.99) / sqrt(2)
  TOL <- 0.01

  n <- length(u)
  res <- numeric(n)

  # Handle special cases for u
  res[u == 0] <- -Inf
  res[u == 1] <- Inf

  # ---- Symmetry relation: Q(-alpha, u) = -Q(alpha, 1 - u) ----
  # Fold negative alpha to positive so all downstream code assumes a >= 0.
  flip <- (a < 0)
  u[flip] <- 1 - u[flip]
  a[flip] <- -a[flip]

  # Pre-processing for a
  # If a == 1, u = sqrt(u)
  idx_a1 <- (a == 1)
  u[idx_a1] <- sqrt(u[idx_a1])

  # Standard normal quantile (z in C code)
  z <- stats::qnorm(u)

  A <- a  # a >= 0 after fold

  # --- Region Checks ---
  # right_limit = erf(const_tol / A)
  # erf(x) = 2 * pnorm(x * sqrt(2)) - 1
  right_limit <- 2 * stats::pnorm((CONST_TOL / A) * SQRT2) - 1

  # Mask for calculations (skip 0/1/Inf boundaries and a=0,1 special cases)
  mask <- (u > 0) & (u < 1) & (a != 0) & (a != 1)

  if (!any(mask)) {
    res[!mask] <- z[!mask]
    res[flip] <- -res[flip]
    return(res)
  }

  # Process masked elements
  u_m <- u[mask]
  A_m <- A[mask]
  z_m <- z[mask]
  rl_m <- right_limit[mask]

  final_vals <- z_m # Default to z, will overwrite

  # 1. Tail Region (Using Lambert W approximation 'plog')
  # a >= 0, so only positive tail check needed
  idx_tail <- (u_m > rl_m)

  if (any(idx_tail)) {
    final_vals[idx_tail] <- stats::qnorm((1 + u_m[idx_tail]) / 2)
  }

  # 2. Main Expansion Region
  idx_main <- !idx_tail

  if (any(idx_main)) {
    u_sub <- u_m[idx_main]
    A_sub <- A_m[idx_main]
    z_sub <- z_m[idx_main]

    # Expansion point 'x'
    val_x <- 0.5 - 0.31830988618379067154 * atan(A_sub)
    x_pt <- stats::qnorm(val_x)

    # Precompute terms
    expon <- exp(-0.5 * x_pt^2)
    errfn <- 1.0
    efder <- expon * 0.79788456080286535588 * A_sub / errfn

    # Coefficients c0 - c5
    c0 <- 0
    c1 <- expon / errfn
    c2 <- -expon * (efder + errfn * x_pt) / (2 * errfn^2)

    term_c3_num <- 3 *
      efder^2 +
      errfn^2 * (-1 + x_pt^2) +
      expon^2 +
      efder * (3 * errfn * x_pt)
    c3 <- 0.16666666666666666667 * expon * term_c3_num / (errfn^3)

    term_c4_num <- 15 *
      efder^3 +
      errfn^3 * x_pt * (-3 + x_pt^2) +
      6 * errfn * expon^2 * x_pt +
      efder^2 * (18 * errfn * x_pt) +
      efder * (errfn^2 * (-4 + 7 * x_pt^2) + expon^2 * (7 - A_sub^2))
    c4 <- -0.041666666666666666667 * expon * term_c4_num / (errfn^4)

    term_c5_num <- 105 *
      efder^4 +
      errfn^4 * (3 - 6 * x_pt^2 + x_pt^4) +
      5 * errfn^2 * expon^2 * (-2 + 5 * x_pt^2) +
      expon^4 * 7 +
      15 * efder^3 * (10 * errfn * x_pt) +
      efder *
        (5 *
          errfn^3 *
          x_pt *
          (-5 + 3 * x_pt^2) +
          10 * errfn * expon^2 * x_pt * (7 - A_sub^2)) +
      5 *
        efder^2 *
        (3 * errfn^2 * (-2 + 5 * x_pt^2) + expon^2 * (-3 * (-4 + A_sub^2)))
    c5 <- 0.0083333333333333333333 * expon * term_c5_num / (errfn^5)

    # Check "Deep Tail" sub-condition within main block using plog
    h <- 0.75 * abs(TOL / c5)^0.2
    left_limit <- x_pt - h

    idx_deep <- (z_sub < left_limit)
    res_sub <- numeric(length(z_sub))

    # -- Deep Tail Calculation (using plog) --
    if (any(idx_deep)) {
      u_d <- u_sub[idx_deep]
      A_d <- A_sub[idx_deep]

      C_PLOG <- 6.2831853071795864769 # 2*pi

      # a >= 0 guaranteed: -sqrt(2 * plog(1 / (2pi * u * a)) / (1 + a^2))
      arg <- 1 / (C_PLOG * u_d * A_d)
      res_sub[idx_deep] <- -sqrt(2 * .plog(arg) / (1 + A_d^2))
    }

    # -- Taylor Expansion Calculation --
    if (any(!idx_deep)) {
      h_val <- z_sub[!idx_deep] - x_pt[!idx_deep]

      poly <- c0 +
        h_val *
          (c1[!idx_deep] +
            h_val *
              (c2[!idx_deep] +
                h_val *
                  (c3[!idx_deep] +
                    h_val * (c4[!idx_deep] + h_val * c5[!idx_deep]))))
      res_sub[!idx_deep] <- poly
    }

    final_vals[idx_main] <- res_sub
  }

  res[mask] <- final_vals

  # Apply symmetry flip for originally-negative alpha
  res[flip] <- -res[flip]

  return(res)
}

# Lambert W-function Approximation (Halley's Method)
.plog <- function(x) {
  # Handle x <= 0 (though log(0) handled inside)
  res <- numeric(length(x))

  # Branch 1: x > 0
  idx_pos <- (x > 0)
  if (any(idx_pos)) {
    xx <- x[idx_pos]
    w0 <- log(1.2 * xx / log(2.4 * xx / log1p(2.4 * xx)))
    res[idx_pos] <- .plog_iterate(w0, xx)
  }

  # Branch 2: x <= 0 (Approximation for other branch/neg inputs)
  # In the context of qsn, x is derived from 1/prob, so usually x > 0.
  # But copying C logic for completeness.
  idx_neg <- !idx_pos
  if (any(idx_neg)) {
    # Check for 0 specifically
    is_zero <- (x == 0)
    # If exactly 0, res is 0 (already set)

    # If not zero but <= 0 (unlikely for 1/prob, but strictly following C)
    # The C code uses sqrt(1 + 2.718... * x). If x is negative this might NaN.
    # Assuming x is small positive or handled by caller.
    # We will compute for non-zero negatives if they exist.
    valid_neg <- idx_neg & !is_zero
    if (any(valid_neg)) {
      xx <- x[valid_neg]
      v <- 1.4142135623730950488 * sqrt(1 + 2.7182818284590452354 * xx)
      N2 <- 10.242640687119285146 + 1.9797586132081854940 * v
      N1 <- 0.29289321881345247560 * (1.4142135623730950488 + N2)
      w0 <- -1.0 + v * (N2 + v) / (N2 + v + N1 * v)
      res[valid_neg] <- .plog_iterate(w0, xx)
    }
  }

  return(res)
}

.plog_iterate <- function(w0, x) {
  # Halley's method iterations
  # 3 iterations is typically sufficient for machine precision
  for (i in 1:3) {
    e <- exp(w0)
    f <- w0 * e - x
    # Prevent div by zero if w0 approx -1 or -2, though unlikely in this domain
    denom <- f * (2.0 + w0) - (e + e) * (1.0 + w0)^2
    w1 <- w0 + ((f + f) * (1.0 + w0)) / denom
    w0 <- w1
  }
  return(w0)
}
