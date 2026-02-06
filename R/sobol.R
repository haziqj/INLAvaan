sobol_owen <- function(n, d) {
  if (d > ncol(SobolOwen)) {
    cli::cli_abort(
      "Requested D = {d} exceeds stored SobolOwen table (max {ncol(SobolOwen)})."
    )
  }

  if (n > nrow(SobolOwen)) {
    cli::cli_alert_warning(
      "Requested N = {n} exceeds stored SobolOwen table (max {nrow(SobolOwen)}). Recycling."
    )
    idx <- rep(seq_len(nrow(SobolOwen)), length.out = n)
    return(SobolOwen[idx, 1:d, drop = FALSE])
  } else {
    idx <- seq_len(n)
  }

  SobolOwen[idx, seq_len(d), drop = FALSE]
}

#' A Sobol sequence with Owen scrambling
#'
#' A Sobol sequence with Owen scrambling, pre-stored for up to N = 100 and D =
#' 1000.
#'
#' @format A matrix with 100 rows and 1000 columns, where each column is a
#'   scrambled Sobol sequence.
"SobolOwen"
