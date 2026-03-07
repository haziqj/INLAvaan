sobol_owen <- function(n, d) {
  N <- nrow(SobolOwen)
  D <- ncol(SobolOwen)

  # If within stored table dimensions, use it directly
  if (n <= N && d <= D) {
    return(SobolOwen[seq_len(n), seq_len(d), drop = FALSE])
  }

  # Otherwise, fall back to qrng if available
  if (requireNamespace("qrng", quietly = TRUE)) {
    return(qrng::sobol(n = n, d = d, randomize = "Owen"))
  }

  # No fallback available — error with guidance
  cli_abort(c(
    "Requested Sobol sequence ({n} x {d}) exceeds stored table ({N} x {D}).",
    "i" = "Install the {.pkg qrng} package to generate larger sequences on the fly."
  ))
}
