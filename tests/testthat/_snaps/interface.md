# the exports and method registrations are stable

    Code
      cat_sorted("Exported objects:", info$exports)
    Output
      Exported objects:
        acfa
        add_loo
        agrowth
        as_fun_string
        asem
        bfit_indices
        compare
        dbeta_box
        diagnostics
        dsnorm
        fit_skew_normal
        fit_skew_normal_samp
        get_inlavaan_internal
        inlavaan
        is_same_function
        loo
        priors_for
        qsnorm_fast
        sampling
        standardisedSolution
        standardisedsolution
        standardizedSolution
        standardizedsolution
        timing
        waic
      
    Code
      cat_sorted("S3 methods:", s3)
    Output
      S3 methods:
        compare.inlavaan_internal
        deviance.INLAvaan
        loo.INLAvaan
        loo.inlavaan_internal
        plot.inlavaan_internal
        predict.inlavaan_internal
        print.bfit_indices
        print.compare.inlavaan_internal
        print.diagnostics.INLAvaan
        print.diagnostics.INLAvaan.param
        print.fitmeasures.inlavaan_internal
        print.inlavaan_deviance
        print.inlavaan_internal
        print.inlavaan_logLik
        print.inlavaan_loo
        print.inlavaan_waic
        print.predict.inlavaan_internal
        print.summary.inlavaan_internal
        print.summary.predict.inlavaan_internal
        print.timing.INLAvaan
        sampling.inlavaan_internal
        summary.bfit_indices
        summary.inlavaan_internal
        summary.inlavaan_loo
        summary.inlavaan_waic
        summary.predict.inlavaan_internal
        waic.INLAvaan
        waic.inlavaan_internal
      
    Code
      cat_sorted("S4 classes:", info$exportClasses)
    Output
      S4 classes:
        INLAvaan
      
    Code
      cat_sorted("S4 methods for:", info$exportMethods)
    Output
      S4 methods for:
        AIC
        BIC
        anova
        coef
        compare
        diagnostics
        fitMeasures
        fitmeasures
        fitted
        fitted.values
        logLik
        nobs
        plot
        predict
        resid
        residuals
        sampling
        show
        simulate
        summary
        timing
        update
        vcov
      

# the arguments of the exported functions are stable

    Code
      cat(vapply(exports[is_fun], signature, character(1)), sep = "\n")
    Output
      acfa(model, data, dp = priors_for(), test = "standard", vb_correction = TRUE, n_qmc = 64L, vb_method = c("sobol", "gauss_hermite"), marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"), marginal_correction = c("shortcut", "shortcut_fd", "hessian", "none"), nsamp = 1000, samp_copula = TRUE, samp_norta = FALSE, cov_as_cor = FALSE, sn_fit_ngrid = 21, sn_fit_logthresh = -6, sn_fit_temp = 1, sn_fit_sample = TRUE, control = list(), verbose = TRUE, debug = FALSE, add_priors = TRUE, optim_method = c("nlminb", "ucminf", "optim"), numerical_grad = FALSE, hessian_method = c("central", "forward"), cores = NULL, ppp_method = c("onestep", "em"), ppp_nsamp = 250L, ...)
      add_loo(object, cores = NULL, verbose = FALSE)
      agrowth(model, data, dp = priors_for(), test = "standard", vb_correction = TRUE, n_qmc = 64L, vb_method = c("sobol", "gauss_hermite"), marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"), marginal_correction = c("shortcut", "shortcut_fd", "hessian", "none"), nsamp = 1000, samp_copula = TRUE, samp_norta = FALSE, cov_as_cor = FALSE, sn_fit_ngrid = 21, sn_fit_logthresh = -6, sn_fit_temp = 1, sn_fit_sample = TRUE, control = list(), verbose = TRUE, debug = FALSE, add_priors = TRUE, optim_method = c("nlminb", "ucminf", "optim"), numerical_grad = FALSE, hessian_method = c("central", "forward"), cores = NULL, ppp_method = c("onestep", "em"), ppp_nsamp = 250L, ...)
      as_fun_string(f)
      asem(model, data, dp = priors_for(), test = "standard", vb_correction = TRUE, n_qmc = 64L, vb_method = c("sobol", "gauss_hermite"), marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"), marginal_correction = c("shortcut", "shortcut_fd", "hessian", "none"), nsamp = 1000, samp_copula = TRUE, samp_norta = FALSE, cov_as_cor = FALSE, sn_fit_ngrid = 21, sn_fit_logthresh = -6, sn_fit_temp = 1, sn_fit_sample = TRUE, control = list(), verbose = TRUE, debug = FALSE, add_priors = TRUE, optim_method = c("nlminb", "ucminf", "optim"), numerical_grad = FALSE, hessian_method = c("central", "forward"), cores = NULL, ppp_method = c("onestep", "em"), ppp_nsamp = 250L, ...)
      bfit_indices(object, baseline.model = NULL, rescale = c("devM", "MCMC"), nsamp = NULL, samp_copula = TRUE)
      compare(x, y, ..., fit.measures = NULL, loo = FALSE)
      dbeta_box(x, shape1, shape2, a, b, log = FALSE)
      diagnostics(object, ...)
      dsnorm(x, xi, omega, alpha, logC = 0, log = FALSE)
      fit_skew_normal(x, y, threshold_log_drop = -6, temp = NA)
      fit_skew_normal_samp(x)
      get_inlavaan_internal(object, what)
      inlavaan(model, data, model.type = "sem", dp = priors_for(), test = "standard", vb_correction = TRUE, n_qmc = 64L, vb_method = c("sobol", "gauss_hermite"), marginal_method = c("skewnorm", "asymgaus", "marggaus", "sampling"), marginal_correction = c("shortcut", "shortcut_fd", "hessian", "none"), nsamp = 1000, samp_copula = TRUE, samp_norta = FALSE, cov_as_cor = FALSE, sn_fit_ngrid = 21, sn_fit_logthresh = -6, sn_fit_temp = 1, sn_fit_sample = TRUE, control = list(), verbose = TRUE, debug = FALSE, add_priors = TRUE, optim_method = c("nlminb", "ucminf", "optim"), numerical_grad = FALSE, hessian_method = c("central", "forward"), start = NULL, cores = NULL, ppp_method = c("onestep", "em"), ppp_nsamp = 250L, ...)
      is_same_function(f, g)
      loo(x, ...)
      priors_for(...)
      qsnorm_fast(p, xi = 0, omega = 1, alpha = 0)
      sampling(object, ...)
      standardisedSolution(object, type = "std.all", se = TRUE, ci = TRUE, level = 0.95, postmedian = FALSE, postmode = FALSE, cov.std = TRUE, remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE, nsamp = 250, ...)
      standardisedsolution(object, type = "std.all", se = TRUE, ci = TRUE, level = 0.95, postmedian = FALSE, postmode = FALSE, cov.std = TRUE, remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE, nsamp = 250, ...)
      standardizedSolution(object, type = "std.all", se = TRUE, ci = TRUE, level = 0.95, postmedian = FALSE, postmode = FALSE, cov.std = TRUE, remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE, nsamp = 250, ...)
      standardizedsolution(object, type = "std.all", se = TRUE, ci = TRUE, level = 0.95, postmedian = FALSE, postmode = FALSE, cov.std = TRUE, remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE, nsamp = 250, ...)
      timing(object, ...)
      waic(x, ...)

