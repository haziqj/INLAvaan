## ----- The default LOO pass of a fit, kept for waic() --------------------------

# loo() and waic() with their default arguments need the same Taylor pass:
# waic_from_taylor() reads the per-unit table that inlav_loo() builds. A fit
# carries an environment, `loo_cache`, that keeps its last default pass, so that
# waic() after loo(), or loo() after waic(), does not run the pass again. The
# environment is shared by every copy of the fit, which R makes whenever the fit
# is passed to a function. The pass is kept with the fit's theta_star and Omega
# and is used only while both are identical, so a copy of the fit with another
# posterior summary computes its own. A fit made before the cache existed has no
# environment and computes the pass at every call, as before.
#
# The pass raises conditions that the two callers treat apart. loo() shows the
# reversion of the log CPO terms to first order (inlavaan_loo_first_order) and
# waic() does not, since it reads none of those terms. Both show the other
# warnings and the note on units with no observed data
# (inlavaan_loo_dropped_units). The conditions are recorded when the pass runs
# and signalled again when it is reused, so the output does not depend on which
# of the two was called first.

loo_cache_new <- function() {
  new.env(parent = emptyenv())
}

loo_cache_key <- function(int) {
  list(theta_star = int$theta_star, Omega = int$Omega)
}

# The default LOO pass of `int` for `caller` ("loo" or "waic"), from the cache
# when it holds the pass of this posterior summary
loo_default_pass <- function(
  int,
  eff_cores = 1L,
  verbose = FALSE,
  caller = c("loo", "waic")
) {
  caller <- match.arg(caller)
  env <- int$loo_cache
  key <- loo_cache_key(int)
  if (is.environment(env) && !is.null(env$res) && identical(env$key, key)) {
    if (isTRUE(verbose)) {
      cli_alert_info("Reusing the LOO pass computed earlier for this fit.")
    }
    loo_cache_replay(env$conds, caller)
    return(env$res)
  }
  conds <- list()
  res <- withCallingHandlers(
    inlav_loo(int, eff_cores = eff_cores, verbose = verbose),
    warning = function(w) {
      conds[[length(conds) + 1L]] <<- w
      if (loo_cache_muffled(w, caller)) {
        invokeRestart("muffleWarning")
      }
    },
    inlavaan_loo_dropped_units = function(m) {
      conds[[length(conds) + 1L]] <<- m
    }
  )
  if (is.environment(env)) {
    env$key <- key
    env$res <- res
    env$conds <- conds
  }
  res
}

# waic() does not show the reversion of the log CPO terms to first order
loo_cache_muffled <- function(cnd, caller) {
  caller == "waic" && inherits(cnd, "inlavaan_loo_first_order")
}

loo_cache_replay <- function(conds, caller) {
  for (cnd in conds) {
    if (inherits(cnd, "message")) {
      message(cnd)
    } else if (!loo_cache_muffled(cnd, caller)) {
      warning(cnd)
    }
  }
  invisible(NULL)
}
