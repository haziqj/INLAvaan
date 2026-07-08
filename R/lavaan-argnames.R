# lavaan >= 0.7 renamed many exported functions'/generics' arguments, mostly
# dot.case -> snake_case (e.g. fit.measures -> fit_measures, cov.std ->
# cov_std) and a few by letter-case (GLIST -> glist). INLAvaan keeps its
# historic dot.case names as its own stable convention and adapts at the
# lavaan boundary. Because every affected function/generic also has "...",
# passing the wrong spelling is not an error: the value silently falls into
# "..." and the caller's override is *dropped* rather than applied. So --
# exactly like the renamed function *names* in lavaan-unexported.R -- the
# spelling the installed lavaan actually uses must be resolved from the
# target's formals once per session in .onLoad() (see zzz.R), never baked in
# at build/install time. This one resolver serves every lavaan argument
# INLAvaan sets: the standardizedSolution()/parameterEstimates() calls below
# and the fitMeasures()/fitmeasures() methods in method-fitmeasures.R.
#
# Each target maps its INLAvaan-canonical (dot.case) argument names to the
# spellings to probe for, newest (lavaan >= 0.7) first and oldest last --
# mirroring the alias vectors in lavaan_internal_aliases.
lavaan_argname_aliases <- list(
  standardizedSolution = list(
    cov.std = c("cov_std", "cov.std"),
    remove.eq = c("remove_eq", "remove.eq"),
    remove.ineq = c("remove_ineq", "remove.ineq"),
    remove.def = c("remove_def", "remove.def"),
    GLIST = c("glist", "GLIST")
  ),
  parameterEstimates = list(
    remove.eq = c("remove_eq", "remove.eq"),
    remove.system.eq = c("remove_system_eq", "remove.system.eq"),
    remove.ineq = c("remove_ineq", "remove.ineq"),
    remove.def = c("remove_def", "remove.def")
  ),
  fitMeasures = list(
    fit.measures = c("fit_measures", "fit.measures"),
    baseline.model = c("baseline_model", "baseline.model")
  )
)

# Filled in by resolve_lavaan_argnames(): one named character vector per
# target above, mapping each INLAvaan-canonical name to the spelling the
# installed lavaan expects. Consumed by call_lavaan() and, for fitMeasures,
# by register_fitmeasures_methods().
lavaan_argnames <- NULL

# Formal names of a lavaan target, whether it is a plain function
# (standardizedSolution, parameterEstimates) or an S4 generic (fitMeasures).
lavaan_target_formals <- function(target) {
  gen <- methods::getGeneric(target, mustFind = FALSE)
  fun <- if (!is.null(gen)) gen else get(target, envir = asNamespace("lavaan"))
  names(formals(fun))
}

resolve_lavaan_argnames <- function(ns) {
  resolved <- lapply(names(lavaan_argname_aliases), function(target) {
    fnames <- lavaan_target_formals(target)
    vapply(
      lavaan_argname_aliases[[target]],
      function(cands) {
        hit <- cands[cands %in% fnames]
        if (length(hit)) hit[[1L]] else cands[[length(cands)]]
      },
      character(1)
    )
  })
  names(resolved) <- names(lavaan_argname_aliases)
  assign("lavaan_argnames", resolved, envir = ns)
  invisible(NULL)
}

# Call lavaan's `target` (standardizedSolution or parameterEstimates),
# renaming any INLAvaan-canonical dot.case argument in `...` to whichever
# spelling the installed lavaan expects. `get(target, ...)` fetches lavaan's
# own function, not INLAvaan's same-named wrapper.
call_lavaan <- function(target, ...) {
  args <- list(...)
  map <- lavaan_argnames[[target]]
  nm <- names(args)
  hit <- nm %in% names(map)
  nm[hit] <- map[nm[hit]]
  names(args) <- nm
  do.call(get(target, envir = asNamespace("lavaan")), args)
}
