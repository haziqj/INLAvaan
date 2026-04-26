cli_messages <- c(
  "Laplace-ing through p dimensions",
  "Summoning Bayesian spirits",
  "Casting statistical spells",
  "Conjuring INLA magic",
  "Channeling Laplace's wizardry",
  "Harnessing the power of priors",
  "Diving into the probability pool",
  "Navigating the seas of stochasticity"
)

inlavaan_force_r_path <- function() {
  backend_opt <- getOption("inlavaan.backend", NULL)
  backend_opt <- if (length(backend_opt) > 0L) {
    tolower(trimws(as.character(backend_opt[[1L]])))
  } else {
    NULL
  }

  isTRUE(getOption("inlavaan.force_r_path", FALSE)) ||
    identical(backend_opt, "r")
}

inlavaan_can_use_native_backend <- function(lavdata = NULL, lavsamplestats = NULL) {
  if (inlavaan_force_r_path()) {
    return(FALSE)
  }

  if (!is.null(lavdata) && !is.null(lavdata@nlevels) && lavdata@nlevels > 2L) {
    return(FALSE)
  }

  if (!is.null(lavdata) && !is.null(lavdata@nlevels) && lavdata@nlevels > 1L &&
      !is.null(lavsamplestats) && isTRUE(lavsamplestats@missing.flag)) {
    return(FALSE)
  }

  TRUE
}

inlavaan_require_native_backend <- function(native_backend, what = "This model") {
  if (!is.null(native_backend) || inlavaan_force_r_path()) {
    return(invisible(TRUE))
  }

  cli_abort(paste(
    what,
    "requires the native C++ backend by default, but no native backend is available for the current configuration.",
    "Set `options(inlavaan.backend = \"r\")` or `options(inlavaan.force_r_path = TRUE)` to use the R path explicitly."
  ))
}

# Moore-Penrose pseudoinverse (replaces MASS::ginv, uses only base svd)
ginv_base <- function(X, tol = sqrt(.Machine$double.eps)) {
  s <- svd(X)
  pos <- s$d > max(tol * s$d[1], 0)
  if (all(pos)) {
    s$v %*% (1 / s$d * t(s$u))
  } else if (!any(pos)) { # nocov start
    array(0, dim(X)[2:1])
  } else {
    s$v[, pos, drop = FALSE] %*%
      ((1 / s$d[pos]) * t(s$u[, pos, drop = FALSE]))
  } # nocov end
}

#' Helper function to check if two functions are the same
#'
#' @param f,g Functions to compare.
#'
#' @returns Logical.
#' @export
#'
#' @examples
#' f1 <- function(x) { x^2 + 1 }
#' f2 <- function(x) { x^2 + 1 }
#' is_same_function(f1, f2)  # TRUE
is_same_function <- function(f, g) {
  identical(deparse(body(f)), deparse(body(g)))
}

#' Convert function to single string
#'
#' @param f Function to convert.
#' @returns A single character vector representing the function.
#' @export
#'
#' @examples
#' f <- function(x) { x^2 + 1 }
#' as_fun_string(f)
as_fun_string <- function(f) {
  gsub("\\s+", " ", paste(deparse(f), collapse = " "))
}

# Check if matrix is a bad covariance (not PD, or contains NA/NaN/Inf)
is_bad_cov <- function(mat) {
  if (any(!is.finite(mat))) return(TRUE)
  tryCatch({ chol(mat); FALSE }, error = function(e) TRUE)
}

# Nearest PD via eigenvalue clamping
make_pd <- function(X, tol = 1e-8) {
  e <- eigen(X, symmetric = TRUE)
  scale0 <- max(abs(e$values), 1)
  vals <- pmax(e$values, tol * scale0)
  e$vectors %*% (vals * t(e$vectors))
}

#' Extract the Internal INLAvaan Object
#'
#' Returns the `inlavaan_internal` list stored inside a fitted [INLAvaan]
#' object, optionally extracting a single named element.
#'
#' @param object An object of class [INLAvaan].
#' @param what Character. Name of the element to extract from the internal
#'   list. If missing, the entire list is returned. Common elements include
#'   `"coefficients"`, `"summary"`, `"Sigma_theta"`, `"vcov_x"`,
#'   `"theta_star"`, `"approx_data"`, `"pdf_data"`, `"partable"`,
#'   `"marginal_method"`, `"nsamp"`, `"mloglik"`, `"DIC"`, `"ppp"`,
#'   `"vb"`, `"opt"`, `"timing"`, `"visual_debug"`.
#'
#' @returns The full `inlavaan_internal` list, or the named element when
#'   `what` is supplied.
#'
#' @seealso [INLAvaan-class], [diagnostics()], [timing()]
#'
#' @examples
#' \donttest{
#' HS.model <- "
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' "
#' utils::data("HolzingerSwineford1939", package = "lavaan")
#' fit <- acfa(HS.model, HolzingerSwineford1939, std.lv = TRUE, nsamp = 100,
#'             test = "none", verbose = FALSE)
#'
#' # Full internal object
#' int <- get_inlavaan_internal(fit)
#' names(int)
#'
#' # Extract a specific element
#' get_inlavaan_internal(fit, "coefficients")
#' }
#'
#' @export
get_inlavaan_internal <- function(object, what) {
  if (!inherits(object, "INLAvaan")) {
    cli_abort("Object must be of class {.cls INLAvaan}.")
  }
  int <- object@external$inlavaan_internal
  if (missing(what)) {
    return(int)
  }
  if (!what %in% names(int)) { # nocov start
    cli_abort(c(
      "Element {.val {what}} not found in the internal list.",
      "i" = "Available: {.val {names(int)}}."
    ))
  } # nocov end
  int[[what]]
}

# Helper function to add timing information. Adapted by Haziq Jamil. Original
# author: Luc De Wilde (lavaan).
add_timing <- function(timing, part) {
  timenow <- proc.time()[3]
  timing[[part]] <- (timenow - timing$start.time)
  timing$start.time <- timenow

  timing
}
is_lavaan <- function(object) { # nocov start
  is(object, "lavaan") & attr(class(object), "package") == "lavaan"
}

is_blavaan <- function(object) {
  is(object, "blavaan") & attr(class(object), "package") == "blavaan"
} # nocov end

is_INLAvaan <- function(object) {
  is(object, "INLAvaan") & attr(class(object), "package") == "INLAvaan"
}

is_inlavaan <- function(object) {
  is(object, "INLAvaan") & attr(class(object), "package") == "INLAvaan"
}

dmode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  if (length(x) == 0) {
    return(NA)
  }
  if (length(x) == 1) {
    return(x)
  }
  if (stats::sd(x) < .Machine$double.eps^0.5) {
    return(mean(x))
  }
  d <- stats::density(x)
  d$x[which.max(d$y)]
}

# ---------------------------------------------------------------------------
# Parallel or serial lapply with cli progress (chunked for parallel)
# ---------------------------------------------------------------------------
run_parallel_or_serial <- function(m, FUN, cores = 1L, verbose = FALSE,
                                   msg_serial = NULL, msg_parallel = NULL) {
  if (cores > 1L) { # nocov start
    # Parallel: process in chunks of `cores` for progress feedback
    if (verbose) {
      msg <- if (!is.null(msg_parallel)) msg_parallel
             else "Processing {m} items ({cores} cores)."
      done <- 0L
      cli_progress_step(
        msg,
        spinner = TRUE
      )
    }
    chunk_ids <- split(seq_len(m), ceiling(seq_len(m) / cores))
    results <- vector("list", m)
    for (ch in chunk_ids) {
      results[ch] <- parallel::mclapply(ch, FUN, mc.cores = cores)
      if (verbose) {
        done <- max(ch)
        cli_progress_update()
      }
    }
  } else { # nocov end
    # Serial with per-item progress
    if (verbose) {
      j <- 0L
      cli_progress_step(
        if (!is.null(msg_serial)) msg_serial
        else "Processing {j}/{m} item{?s}.",
        spinner = TRUE
      )
    }
    results <- vector("list", m)
    for (j in seq_len(m)) {
      results[[j]] <- FUN(j)
      if (verbose) cli_progress_update()
    }
  }
  results
}

.detect_cores_cache <- local({
  cache <- new.env(parent = emptyenv())
  cache$val <- NULL
  function() {
    if (is.null(cache$val)) {
      cache$val <- parallel::detectCores()
    }
    cache$val
  }
})

with_safe_detectCores <- function(expr) {
  ncpu <- .detect_cores_cache()
  if (!is.na(ncpu)) {
    return(force(expr))
  }

  parallel_ns <- asNamespace("parallel")
  orig_detectCores <- get("detectCores", envir = parallel_ns)
  unlockBinding("detectCores", parallel_ns)
  assign(
    "detectCores",
    function(all.tests = FALSE, logical = TRUE) 2L,
    envir = parallel_ns
  )
  lockBinding("detectCores", parallel_ns)
  on.exit({
    unlockBinding("detectCores", parallel_ns)
    assign("detectCores", orig_detectCores, envir = parallel_ns)
    lockBinding("detectCores", parallel_ns)
  }, add = TRUE)
  force(expr)
}
