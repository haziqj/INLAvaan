# # This function computes the log-likelihood of a lavaan model given lavaan-side
# # parameter values 'x' (not unrestricted theta-side). The idea is to use an
# # empty lavaan shell via `do.fit = FALSE`, and then update the lavmodel value
# # and feed it into lav_model_objective().
# #
# # The value returned by lav_model_objective() is in fact NOT the log-likelihood,
# # but a convenient and numerically stable that is used for optimisation in
# # lavaan. Several cases:
# #
# # BLOCK A [MVN] Missing data case, FIML
# # BLOCK B [MVN] Multilevel case
# # BLOCK C [MVN] Standard
# # BLOCK D [PML] Pairwise likelihood
# #
# # In each case, we need to recover the actual log-likelihood by applying the
# # correct scaling and adding back the missing constant, if needed.
#
# inlav_model_loglik <- function(x, lavmodel, lavsamplestats, lavdata, lavcache) {
#   # The MVN fit function is F = log|Sigma| + tr(S Sigma^{-1}) - log|S| - p
#   # Single-level returns   fx = F / n
#   # Multilevel returns     fx = -logLik / n
#   # PML returns            fx = -PML
#   lavmodel_x <- lavaan::lav_model_set_parameters(lavmodel, x)
#   fx <- as.numeric(lavaan:::lav_model_objective(
#     lavmodel = lavmodel_x,
#     GLIST = NULL,
#     lavsamplestats = lavsamplestats,
#     lavdata = lavdata,
#     lavcache = lavcache
#   ))
#
#   # Recover the kernel  by scaling fx by (-1 * n), except for PML.
#   if (lavmodel@estimator == "PML") {
#     # >>> BLOCK A <<< PAIRWISE LIKELIHOOD
#     no_ord <- length(fit@Data@ordered)
#     kappa <- 1 / sqrt(no_ord)
#     ll_kernel <- -1 * kappa * fx
#   } else {
#     ntotal <- lavsamplestats@ntotal
#     ll_kernel <- -1 * ntotal * fx
#   }
#
#   # In principle, the constant part is -0.5 * n * p * log(2pi) for everything
#   # except PML. However the 'np' part depends on missing data and multilevel
#   # structure.
#   ll_constant <- 0
#   log2pi <- log(2 * pi)
#
#   if (lavmodel@estimator == "ML") {
#     if (lavsamplestats@missing.flag) {
#       # >>> BLOCK A <<< MISSING DATA FIML (SINGLE/MULTILEVEL)
#       for (g in 1:lavsamplestats@ngroups) {
#         nobs_g <- sum(sapply(
#           lavsamplestats@missing[[g]],
#           function(p) {
#             p$freq * sum(p$var.idx)
#           }
#         ))
#
#         ll_constant <- ll_constant + (-0.5 * log2pi * nobs_g)
#       }
#     } else {
#       for (g in 1:lavsamplestats@ngroups) {
#         nobs_g <- lavsamplestats@nobs[[g]]
#
#         # If conditional.x = TRUE, we only want Endogenous (Y).
#         p_g <- if (lavmodel@conditional.x) {
#           length(lavmodel@ov.y.idx[[g]])
#         } else {
#           if (is.list(lavmodel@nvar)) lavmodel@nvar[[g]] else lavmodel@nvar[g]
#         }
#
#         if (lavdata@nlevels > 1L) {
#           # >>> BLOCK B <<< MULTILEVEL, COMPLETE DATA
#           ll_constant <- ll_constant + (-0.5 * nobs_g * p_g * log2pi)
#         } else {
#           # >>> BLOCK C <<< STANDARD ML, COMPLETE DATA (ADD SATURATED MODEL)
#           log_det_S <- lavsamplestats@cov.log.det[[g]]
#           const_g <- -0.5 * nobs_g * (p_g * log2pi + log_det_S + p_g)
#           ll_constant <- ll_constant + const_g
#         }
#       }
#     }
#   }
#
#   print(ll_kernel)
#   print(ll_constant)
#   return(ll_kernel + ll_constant)
# }
#
# mvnorm_loglik_grad <- function(
#   x,
#   lavmodel,
#   lavsamplestats,
#   lavdata,
#   lavoptions,
#   lavcache
# ) {
#   # Gradient of fit function F_ML (not loglik yet)
#   grad_F <- lavaan___lav_model_gradient(
#     lavmodel = lavaan::lav_model_set_parameters(lavmodel, x),
#     lavsamplestats = lavsamplestats,
#     lavdata = lavdata
#   )
#   # Rescale so we get gradient of loglik
#   out <- -1 * lavsamplestats@ntotal * grad_F
#   out
# }
