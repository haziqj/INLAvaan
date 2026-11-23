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
      cat(lines, sep = "\n")
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

# the arguments of the S4 methods for INLAvaan are stable

    Code
      cat(lines, sep = "\n")
    Output
      AIC(object, ..., k = 2)
      BIC(object, ...)
      anova(object, ...)
      coef(object, ...)
      compare(x, y, ..., fit.measures = NULL, loo = FALSE)
      diagnostics(object, type = c("global", "param"), ...)
      fitMeasures(object, fit_measures = "all", baseline_model = NULL, h1_model = NULL, fm_args = list(standard.test = "default", scaled.test = "default", rmsea.ci.level = 0.9, rmsea.close.h0 = 0.05, rmsea.notclose.h0 = 0.08, robust = TRUE, cat.nonpd = "na"), output = "vector", level = NULL, ...)
      fitmeasures(object, fit_measures = "all", baseline_model = NULL, h1_model = NULL, fm_args = list(standard.test = "default", scaled.test = "default", rmsea.ci.level = 0.9, rmsea.close.h0 = 0.05, rmsea.notclose.h0 = 0.08, robust = TRUE, cat.nonpd = "na"), output = "vector", level = NULL, ...)
      fitted(object, type = "moments", labels = TRUE, ..., per_cluster = FALSE)
      fitted.values(object, type = "moments", labels = TRUE, ..., per_cluster = FALSE)
      logLik(object, type = c("marginal", "plugin"), ...)
      nobs(object, ...)
      plot(x, y, type = c("marg_pdf", "sn_fit", "sn_fit_log"), params = "all", nrow = NULL, ncol = NULL, use_ggplot = TRUE, points = FALSE, ...)
      predict(object, type = c("lv", "yhat", "ov", "ypred", "ydist", "ymis", "ovmis"), newdata = NULL, level = 1L, nsamp = 1000, ymis_only = FALSE, summary = FALSE, ...)
      resid(object, type = "raw", ..., per_cluster = FALSE)
      residuals(object, type = "raw", labels = TRUE, ..., per_cluster = FALSE)
      sampling(object, type = c("lavaan", "theta", "latent", "observed", "implied", "all"), nsamp = 1000L, samp_copula = TRUE, prior = FALSE, silent = FALSE, ...)
      show(object)
      simulate(object, nsim = 1L, seed = NULL, sample.nobs = NULL, prior = FALSE, samp_copula = TRUE, silent = FALSE, ...)
      summary(object, header = TRUE, fit.measures = TRUE, estimates = TRUE, ci = TRUE, standardized = FALSE, standardised = standardized, rsquare = FALSE, postmedian = FALSE, postmode = FALSE, nmad = TRUE, kld = FALSE, vb_shift = FALSE, priors = TRUE, nd = 3L, ...)
      timing(object, what = "total", ...)
      update(object, model, add, ..., evaluate = TRUE)
      vcov(object, type = c("lavaan", "theta"), ...)

# the arguments of the S3 methods are stable

    Code
      cat(lines, sep = "\n")
    Output
      compare.inlavaan_internal(x, y, ..., fit.measures = NULL, loo = FALSE)
      deviance.INLAvaan(object, type = c("mean", "plugin"), ...)
      loo.INLAvaan(x, type = c("auto", "loso", "loco"), units = NULL, second_order = TRUE, theta = NULL, Omega = NULL, cores = NULL, verbose = FALSE, ...)
      loo.inlavaan_internal(x, type = c("auto", "loso", "loco"), units = NULL, second_order = TRUE, theta = NULL, Omega = NULL, cores = NULL, verbose = FALSE, ...)
      plot.inlavaan_internal(x, truth, type = c("marg_pdf", "sn_fit", "sn_fit_log"), params = "all", nrow = NULL, ncol = NULL, use_ggplot = TRUE, points = FALSE, ...)
      predict.inlavaan_internal(object, type = c("lv", "yhat", "ov", "ypred", "ydist", "ymis", "ovmis"), newdata = NULL, level = 1L, nsamp = 250, ymis_only = FALSE, summary = FALSE, ...)
      print.bfit_indices(x, ...)
      print.compare.inlavaan_internal(x, ...)
      print.diagnostics.INLAvaan(x, ...)
      print.diagnostics.INLAvaan.param(x, digits = 4, ...)
      print.fitmeasures.inlavaan_internal(x, ...)
      print.inlavaan_deviance(x, digits = 3L, ...)
      print.inlavaan_internal(x, digits = 3, ...)
      print.inlavaan_logLik(x, digits = 3L, ...)
      print.inlavaan_loo(x, ...)
      print.inlavaan_waic(x, ...)
      print.predict.inlavaan_internal(x, n = 10L, nd = 3L, ...)
      print.summary.inlavaan_internal(x, digits = 3, ...)
      print.summary.predict.inlavaan_internal(x, stat = "Mean", n = 10L, nd = 3L, ...)
      print.timing.INLAvaan(x, ...)
      sampling.inlavaan_internal(object, type = c("lavaan", "theta", "latent", "observed", "implied", "all"), nsamp = 1000L, samp_copula = TRUE, prior = FALSE, silent = FALSE, ...)
      summary.bfit_indices(object, ...)
      summary.inlavaan_internal(object, ...)
      summary.inlavaan_loo(object, ...)
      summary.inlavaan_waic(object, ...)
      summary.predict.inlavaan_internal(object, ...)
      waic.INLAvaan(x, type = c("auto", "loso", "loco"), units = NULL, second_order = TRUE, cores = NULL, verbose = FALSE, ...)
      waic.inlavaan_internal(x, type = c("auto", "loso", "loco"), units = NULL, second_order = TRUE, cores = NULL, verbose = FALSE, ...)

