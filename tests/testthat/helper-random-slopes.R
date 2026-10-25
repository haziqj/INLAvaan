## ----- Shared random-slope fixture (route A, 24 clusters) --------------------
# The 24-cluster subset of lavaan::Demo.twolevel (300 rows, cluster sizes
# cycling 5/10/15/20) used by the random-slope tests. `rv('s1')` makes the
# level-1 slope of x1 a level-2 latent variable, so the likelihood comes from
# lavaan's per-cluster random-slope kernels rather than from the model-implied
# moments. shared_rs_fit() fits the model on first use and returns that same
# fit to every later caller, so a test run pays for it once.
d_rs <- lavaan::Demo.twolevel[lavaan::Demo.twolevel$cluster %in% 1:24, ]
mod_rs <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ rv('s1')*x1
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1
    s1 ~ w1
"
shared_rs_fit <- local({
  fit <- NULL
  function() {
    if (is.null(fit)) {
      fit <<- asem(
        mod_rs,
        d_rs,
        cluster = "cluster",
        verbose = FALSE,
        test = "none",
        marginal_correction = "none",
        vb_correction = FALSE,
        nsamp = 3
      )
    }
    fit
  }
})
