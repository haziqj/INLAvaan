# Unexported lavaan internals used by INLAvaan. CRAN policy does not permit
# calling another package's internals with `:::`, so the bindings below
# start as NULL and are bound once per session in .onLoad() (see zzz.R) via
# utils::getFromNamespace(). Call-time overhead is zero, and because the
# lookup runs against whichever lavaan is loaded in the session, upgrading
# lavaan in place can never leave stale function objects behind (which
# binding at build/install time could). The names are lavaan >= 0.7-2's own
# (required in DESCRIPTION); each binding is the lavaan name with a
# `lavaan___` prefix.

lavaan___lav_model_loglik <- NULL
lavaan___lav_model_objective <- NULL
lavaan___lav_model_grad <- NULL
lavaan___lav_model_lambda <- NULL
lavaan___lav_model_veta <- NULL
lavaan___lav_model_eeta <- NULL
lavaan___lav_model_ey <- NULL
lavaan___lav_model_delta <- NULL
lavaan___lav_mvn_loglik_samp <- NULL
lavaan___lav_mvn_mi_loglik_samp <- NULL
lavaan___lav_mvn_sc_mu_sigma <- NULL
lavaan___lav_mvn_cl_implied22l <- NULL
lavaan___lav_mvn_cl_loglik_samp_2l <- NULL
lavaan___lav_mvn_cl_dlogl_2l_samp <- NULL
lavaan___lav_mvn_cl_mi_loglik_samp_2l <- NULL
lavaan___lav_mvn_cl_mi_dlogl_2l_samp <- NULL
lavaan___lav_data_mi_patterns <- NULL
lavaan___lav_inspect_coef <- NULL

lavaan_internal_names <- c(
  "lav_model_loglik",
  "lav_model_objective",
  "lav_model_grad",
  "lav_model_lambda",
  "lav_model_veta",
  "lav_model_eeta",
  "lav_model_ey",
  "lav_model_delta",
  "lav_mvn_loglik_samp",
  "lav_mvn_mi_loglik_samp",
  "lav_mvn_sc_mu_sigma",
  "lav_mvn_cl_implied22l",
  "lav_mvn_cl_loglik_samp_2l",
  "lav_mvn_cl_dlogl_2l_samp",
  "lav_mvn_cl_mi_loglik_samp_2l",
  "lav_mvn_cl_mi_dlogl_2l_samp",
  "lav_data_mi_patterns",
  "lav_inspect_coef"
)

resolve_lavaan_internals <- function(ns) {
  lav_ns <- asNamespace("lavaan")
  for (name in lavaan_internal_names) {
    if (!exists(name, envir = lav_ns, inherits = FALSE)) {
      # nocov start
      stop(
        "INLAvaan needs the internal lavaan function ",
        sQuote(name),
        ", which does not exist in lavaan ",
        as.character(utils::packageVersion("lavaan")),
        ". Please report this at https://github.com/haziqj/INLAvaan/issues.",
        call. = FALSE
      )
      # nocov end
    }
    assign(
      paste0("lavaan___", name),
      utils::getFromNamespace(name, "lavaan"),
      envir = ns
    )
  }
  invisible(NULL)
}
