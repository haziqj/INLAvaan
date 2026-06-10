# Unexported lavaan internals used by INLAvaan. lavaan >= 0.7 renamed many
# of its internal functions (e.g. lav_model_gradient -> lav_model_grad), and
# resolving them at the top level would bake in whichever name exists at
# *install* time. Instead, the bindings below start as NULL and are resolved
# once per session in .onLoad() (see zzz.R): for each binding, the first
# alias found in the lavaan namespace wins. Call-time overhead is zero.
#
# NOTE: lavaan >= 0.7 also renamed arguments (dot.case -> snake_case), so
# call sites must avoid argument names that differ across versions; they
# rely on positional matching instead (the order is stable).

lavaan___lav_model_loglik <- NULL
lavaan___lav_model_objective <- NULL
lavaan___lav_model_gradient <- NULL
lavaan___lav_model_lambda <- NULL
lavaan___lav_model_veta <- NULL
lavaan___lav_model_eeta <- NULL
lavaan___lav_model_ey <- NULL
lavaan___lav_mvnorm_loglik_samplestats <- NULL
lavaan___lav_mvnorm_missing_loglik_samplestats <- NULL
lavaan___lav_mvnorm_cluster_implied22l <- NULL

# Aliases ordered new (lavaan >= 0.7) first, old (lavaan < 0.7) last, so on
# new lavaan we bind the real function rather than its deprecated shim.
lavaan_internal_aliases <- list(
  lavaan___lav_model_loglik = "lav_model_loglik",
  lavaan___lav_model_objective = "lav_model_objective",
  lavaan___lav_model_gradient = c("lav_model_grad", "lav_model_gradient"),
  lavaan___lav_model_lambda = c("lav_model_lambda", "computeLAMBDA"),
  lavaan___lav_model_veta = c("lav_model_veta", "computeVETA"),
  lavaan___lav_model_eeta = c("lav_model_eeta", "computeEETA"),
  lavaan___lav_model_ey = c("lav_model_ey", "computeEY"),
  lavaan___lav_mvnorm_loglik_samplestats = c(
    "lav_mvn_loglik_samp",
    "lav_mvnorm_loglik_samplestats"
  ),
  lavaan___lav_mvnorm_missing_loglik_samplestats = c(
    "lav_mvn_mi_loglik_samp",
    "lav_mvnorm_missing_loglik_samplestats"
  ),
  lavaan___lav_mvnorm_cluster_implied22l = c(
    "lav_mvn_cl_implied22l",
    "lav_mvnorm_cluster_implied22l"
  )
)

resolve_lavaan_internals <- function(ns) {
  lav_ns <- asNamespace("lavaan")
  for (binding in names(lavaan_internal_aliases)) {
    aliases <- lavaan_internal_aliases[[binding]]
    found <- NULL
    for (alias in aliases) {
      if (exists(alias, envir = lav_ns, inherits = FALSE)) {
        found <- get(alias, envir = lav_ns, inherits = FALSE)
        break
      }
    }
    if (is.null(found)) {
      # nocov start
      stop(
        "INLAvaan needs the internal lavaan function ",
        paste(sQuote(aliases), collapse = " or "),
        ", but neither exists in lavaan ",
        as.character(utils::packageVersion("lavaan")),
        ". Please report this at https://github.com/haziqj/INLAvaan/issues.",
        call. = FALSE
      )
      # nocov end
    }
    assign(binding, found, envir = ns)
  }
  invisible(NULL)
}
