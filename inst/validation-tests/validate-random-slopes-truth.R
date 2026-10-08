################################################################################
#
# Validate parameter recovery for a random-slope factor model with a genuinely
# positive slope variance, on both random-slope routes.
#
# Route A puts the random slope on an observed within-only covariate (closed
# form). Route B puts it on a latent within-level covariate, which lavaan
# integrates by Gauss-Hermite quadrature and is considerably slower. It is
# timed below and the expected 'inlavaan_rs_route_b' warning is captured.
# Run after devtools::load_all(".").
################################################################################

## ----- Configuration ---------------------------------------------------------
smoke <- FALSE # TRUE shrinks J, nsamp and ngh for a quick development run
n <- 10L

J_a <- if (smoke) 40L else 200L
nsamp_a <- if (smoke) 200L else 1000L

J_b <- if (smoke) 15L else 60L
nsamp_b <- if (smoke) 100L else 500L
ngh <- if (smoke) 7L else 21L

# Returns TRUE when `truth` falls inside [lo, hi], for the assertions below.
inside <- function(truth, lo, hi) truth >= lo & truth <= hi

## =============================================================================
## Route A -- observed within-level covariate
## =============================================================================

## ----- Data-generating process (route A) -------------------------------------
# fw =~ y1 + y2 + y3 at level 1 (loadings 1, 0.8, 0.7, residual variance 1
# each), regressed on x1 with random slope s1. fb =~ y1 + y2 + y3 at level 2
# (same loadings, variance 0.9, residual variance 0.1 each). x1 is
# within-only (no between-cluster component).
#
# s1 is generated directly as lavaan's `rv()` parameterises it: an
# intercept (the fixed effect, 0.5), a between-level regression on w1
# (0.2), and a residual (variance 0.25). It is the entire level-1 slope,
# not an addition on top of a separate fixed baseline.
set.seed(202603)
cluster_a <- rep(seq_len(J_a), each = n)
w1_a <- rnorm(J_a)
s1_a <- 0.5 + 0.2 * w1_a + rnorm(J_a, 0, sqrt(0.25))

x1 <- rnorm(J_a * n)
fw_a <- s1_a[cluster_a] * x1 + rnorm(J_a * n, 0, sqrt(0.8))
y1w_a <- 1.0 * fw_a + rnorm(J_a * n)
y2w_a <- 0.8 * fw_a + rnorm(J_a * n)
y3w_a <- 0.7 * fw_a + rnorm(J_a * n)

fb_a <- rnorm(J_a, 0, sqrt(0.9))
y1b_a <- 1.0 * fb_a + rnorm(J_a, 0, sqrt(0.1))
y2b_a <- 0.8 * fb_a + rnorm(J_a, 0, sqrt(0.1))
y3b_a <- 0.7 * fb_a + rnorm(J_a, 0, sqrt(0.1))

d_a <- data.frame(
  y1 = y1w_a + y1b_a[cluster_a],
  y2 = y2w_a + y2b_a[cluster_a],
  y3 = y3w_a + y3b_a[cluster_a],
  x1 = x1,
  w1 = w1_a[cluster_a],
  cluster = cluster_a
)

mod_a <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ rv('s1')*x1
  level: 2
    fb =~ y1 + y2 + y3
    s1 ~ w1
"

## ----- Fit route A -----------------------------------------------------------
fit_a_lav <- suppressWarnings(lavaan::sem(mod_a, d_a, cluster = "cluster"))
fit_a <- asem(mod_a, d_a, cluster = "cluster", nsamp = nsamp_a)
summ_a <- get_inlavaan_internal(fit_a)$summary

## ----- Comparison table (route A) --------------------------------------------
truth_a <- c(
  "fw=~y2" = 0.8,
  "fw=~y3" = 0.7,
  "y1~~y1" = 1,
  "y2~~y2" = 1,
  "y3~~y3" = 1,
  "fw~~fw" = 0.8,
  "fb=~y2.l2" = 0.8,
  "fb=~y3.l2" = 0.7,
  "s1~w1.l2" = 0.2,
  "y1~~y1.l2" = 0.1,
  "y2~~y2.l2" = 0.1,
  "y3~~y3.l2" = 0.1,
  "fb~~fb.l2" = 0.9,
  "s1~~s1.l2" = 0.25,
  "y1~1.l2" = 0,
  "y2~1.l2" = 0,
  "y3~1.l2" = 0,
  "s1~1.l2" = 0.5
)
pn_a <- rownames(summ_a)
tab_a <- data.frame(
  parameter = pn_a,
  truth = unname(truth_a[pn_a]),
  lavaan_mle = unname(coef(fit_a_lav)[pn_a]),
  inlavaan_mean = summ_a[pn_a, "Mean"],
  ci_2.5 = summ_a[pn_a, "2.5%"],
  ci_97.5 = summ_a[pn_a, "97.5%"]
)
print(tab_a, digits = 3, row.names = FALSE)

## ----- Assertions (route A) --------------------------------------------------
stopifnot(
  "s1~w1 (the covariate effect on the slope) truth is inside its 95% interval" = inside(
    truth_a[["s1~w1.l2"]],
    tab_a$ci_2.5[pn_a == "s1~w1.l2"],
    tab_a$ci_97.5[pn_a == "s1~w1.l2"]
  ),
  "the slope variance truth is inside its 95% interval" = inside(
    truth_a[["s1~~s1.l2"]],
    tab_a$ci_2.5[pn_a == "s1~~s1.l2"],
    tab_a$ci_97.5[pn_a == "s1~~s1.l2"]
  ),
  "INLAvaan means fall within 3 posterior SD of the lavaan MLEs" = all(
    abs(tab_a$inlavaan_mean - tab_a$lavaan_mle) <= 3 * summ_a[pn_a, "SD"]
  )
)

## =============================================================================
## Route B -- latent covariate (quadrature)
## =============================================================================
# Same structure, but the random slope multiplies a latent within-level
# covariate fz =~ z1 + z2 + z3 (loadings 1, 0.9, 0.8, residual variance 0.5
# each, factor variance 1) instead of an observed one. lavaan replaces the
# closed-form cluster kernel with Gauss-Hermite quadrature for this case, so
# the fit is timed and the expected warning is captured rather than
# silenced outright.

## ----- Data-generating process (route B) -------------------------------------
set.seed(202604)
cluster_b <- rep(seq_len(J_b), each = n)
w1_b <- rnorm(J_b)
s1_b <- 0.5 + 0.2 * w1_b + rnorm(J_b, 0, sqrt(0.25))

fz <- rnorm(J_b * n, 0, 1)
z1 <- 1.0 * fz + rnorm(J_b * n, 0, sqrt(0.5))
z2 <- 0.9 * fz + rnorm(J_b * n, 0, sqrt(0.5))
z3 <- 0.8 * fz + rnorm(J_b * n, 0, sqrt(0.5))

fw_b <- s1_b[cluster_b] * fz + rnorm(J_b * n, 0, sqrt(0.8))
y1w_b <- 1.0 * fw_b + rnorm(J_b * n)
y2w_b <- 0.8 * fw_b + rnorm(J_b * n)
y3w_b <- 0.7 * fw_b + rnorm(J_b * n)

fb_b <- rnorm(J_b, 0, sqrt(0.9))
y1b_b <- 1.0 * fb_b + rnorm(J_b, 0, sqrt(0.1))
y2b_b <- 0.8 * fb_b + rnorm(J_b, 0, sqrt(0.1))
y3b_b <- 0.7 * fb_b + rnorm(J_b, 0, sqrt(0.1))

d_b <- data.frame(
  y1 = y1w_b + y1b_b[cluster_b],
  y2 = y2w_b + y2b_b[cluster_b],
  y3 = y3w_b + y3b_b[cluster_b],
  z1 = z1,
  z2 = z2,
  z3 = z3,
  w1 = w1_b[cluster_b],
  cluster = cluster_b
)

mod_b <- "
  level: 1
    fw =~ y1 + y2 + y3
    fw ~ rv('s1')*fz
    fz =~ z1 + z2 + z3
  level: 2
    fb =~ y1 + y2 + y3
    s1 ~ w1
"

## ----- Fit route B -----------------------------------------------------------
route_b_warned <- FALSE
t0 <- Sys.time()
fit_b <- withCallingHandlers(
  asem(mod_b, d_b, cluster = "cluster", integration.ngh = ngh, nsamp = nsamp_b),
  inlavaan_rs_route_b = function(w) {
    route_b_warned <<- TRUE
    invokeRestart("muffleWarning")
  }
)
t_route_b <- as.numeric(Sys.time() - t0, units = "secs")
cat(sprintf("\nRoute B fit time: %.1f s\n", t_route_b))
cat(
  if (route_b_warned) {
    "The `inlavaan_rs_route_b` warning fired, as expected.\n"
  } else {
    "The `inlavaan_rs_route_b` warning did NOT fire.\n"
  }
)

## ----- Comparison table (route B) --------------------------------------------
truth_b <- c(
  "fw=~y2" = 0.8,
  "fw=~y3" = 0.7,
  "fz=~z2" = 0.9,
  "fz=~z3" = 0.8,
  "y1~~y1" = 1,
  "y2~~y2" = 1,
  "y3~~y3" = 1,
  "z1~~z1" = 0.5,
  "z2~~z2" = 0.5,
  "z3~~z3" = 0.5,
  "fw~~fw" = 0.8,
  "fz~~fz" = 1,
  "z1~1" = 0,
  "z2~1" = 0,
  "z3~1" = 0,
  "fb=~y2.l2" = 0.8,
  "fb=~y3.l2" = 0.7,
  "s1~w1.l2" = 0.2,
  "y1~~y1.l2" = 0.1,
  "y2~~y2.l2" = 0.1,
  "y3~~y3.l2" = 0.1,
  "fb~~fb.l2" = 0.9,
  "s1~~s1.l2" = 0.25,
  "y1~1.l2" = 0,
  "y2~1.l2" = 0,
  "y3~1.l2" = 0,
  "s1~1.l2" = 0.5
)
summ_b <- get_inlavaan_internal(fit_b)$summary
pn_b <- rownames(summ_b)
tab_b <- data.frame(
  parameter = pn_b,
  truth = unname(truth_b[pn_b]),
  inlavaan_mean = summ_b[pn_b, "Mean"],
  ci_2.5 = summ_b[pn_b, "2.5%"],
  ci_97.5 = summ_b[pn_b, "97.5%"]
)
print(tab_b, digits = 3, row.names = FALSE)

stopifnot(
  "the `inlavaan_rs_route_b` warning fired" = route_b_warned
)
