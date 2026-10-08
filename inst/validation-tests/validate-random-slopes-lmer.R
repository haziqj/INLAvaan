################################################################################
#
# Validate the simplest random-slope regression against lme4's and lavaan's
# maximum-likelihood fits.
#
# The data-generating process has a random intercept and a random slope on a
# single within-cluster covariate, with a nonzero intercept-slope covariance.
# Three fits are compared on the same simulated data: lme4::lmer() (ML),
# lavaan::sem() (ML), and asem() (posterior). Requires the lme4 package
# (Suggests). Run after devtools::load_all(".").
################################################################################

## ----- Configuration ---------------------------------------------------------
smoke <- FALSE # TRUE shrinks J and nsamp for a quick development run
library(lme4)
set.seed(202601)

J <- if (smoke) 30L else 150L
n <- 10L
nsamp <- if (smoke) 200L else 1000L

## =============================================================================
## Simulate data
## =============================================================================
# Random-intercept variance 0.5, random-slope variance 0.25, and an
# intercept-slope covariance of 0.1. x1 is the sum of a cluster mean and a
# within-cluster deviate, both standard normal.
cluster <- rep(seq_len(J), each = n)
x1 <- rep(rnorm(J), each = n) + rnorm(J * n)

Sigma_u <- matrix(c(0.5, 0.1, 0.1, 0.25), nrow = 2)
u <- matrix(rnorm(J * 2), nrow = J) %*% chol(Sigma_u)
u0 <- u[, 1]
u1 <- u[, 2]

y1 <- 1 + u0[cluster] + (0.5 + u1[cluster]) * x1 + rnorm(J * n)
d <- data.frame(y1 = y1, x1 = x1, cluster = cluster)

# lavaan refuses `s1 ~~ y1` only when the model cannot support a mean
# structure for it. Here it is accepted, so the intercept-slope covariance
# stays in the model and in the DGP above.
mod <- "
  level: 1
    y1 ~ rv('s1')*x1
  level: 2
    y1 ~~ y1
    s1 ~~ s1
    s1 ~~ y1
"

## =============================================================================
## Fit models
## =============================================================================

## ----- lme4 maximum likelihood -----------------------------------------------
fit_lmer <- lmer(y1 ~ x1 + (1 + x1 | cluster), REML = FALSE, data = d)
vc_lmer <- as.data.frame(VarCorr(fit_lmer))

## ----- lavaan maximum likelihood ---------------------------------------------
# lavaan warns about a negative variance on some resamples of this DGP.
fit_lav <- suppressWarnings(lavaan::sem(mod, d, cluster = "cluster"))

## ----- INLAvaan posterior ----------------------------------------------------
fit_inlv <- asem(mod, d, cluster = "cluster", nsamp = nsamp)
summ <- get_inlavaan_internal(fit_inlv)$summary

## =============================================================================
## Compare and assert
## =============================================================================

## ----- Comparison table ------------------------------------------------------
rows <- c(
  "y1~1.l2", # fixed intercept
  "s1~1.l2", # fixed slope
  "y1~~y1.l2", # random-intercept variance
  "s1~~s1.l2", # random-slope variance
  "s1~~y1.l2", # intercept-slope covariance
  "y1~~y1" # residual variance
)
truth <- c(1, 0.5, 0.5, 0.25, 0.1, 1)

lme4_est <- c(
  fixef(fit_lmer)[["(Intercept)"]],
  fixef(fit_lmer)[["x1"]],
  vc_lmer$vcov[
    vc_lmer$grp == "cluster" &
      vc_lmer$var1 == "(Intercept)" &
      is.na(vc_lmer$var2)
  ],
  vc_lmer$vcov[
    vc_lmer$grp == "cluster" & vc_lmer$var1 == "x1" & is.na(vc_lmer$var2)
  ],
  vc_lmer$vcov[vc_lmer$grp == "cluster" & !is.na(vc_lmer$var2)],
  vc_lmer$vcov[vc_lmer$grp == "Residual"]
)

tab <- data.frame(
  parameter = rows,
  truth = truth,
  lme4_mle = lme4_est,
  lavaan_mle = unname(coef(fit_lav)[rows]),
  inlavaan_mean = summ[rows, "Mean"],
  ci_2.5 = summ[rows, "2.5%"],
  ci_97.5 = summ[rows, "97.5%"]
)
print(tab, digits = 3, row.names = FALSE)

## ----- Assertions ------------------------------------------------------------
sd_inlv <- summ[rows, "SD"]
stopifnot(
  "INLAvaan means fall within 3 posterior SD of the lme4 estimates" = all(
    abs(tab$inlavaan_mean - tab$lme4_mle) <= 3 * sd_inlv
  )
)
n_outside <- sum(truth < tab$ci_2.5 | truth > tab$ci_97.5)
stopifnot(
  "at most one truth falls outside its 95% interval" = n_outside <= 1
)
cat(sprintf(
  "\n%d/%d truths fall outside their 95%% interval.\n",
  n_outside,
  length(rows)
))
