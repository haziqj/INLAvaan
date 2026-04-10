library(INLAvaan)

# ==============================================================================
# MSEM SCENARIO: The Science of Student Engagement
# Students (Level 1) nested within Classrooms (Level 2).
# y1, y2, y3 = Survey items measuring "Engagement" (e.g. Participation,
#              Attention, Effort)
# ==============================================================================

mod <- "
    # --------------------------------------------------------------------------
    # LEVEL 1: WITHIN-CLASSROOM (Individual Student Differences)
    # Question: Why is one student more engaged than their peers in the same
    #           classroom?
    # --------------------------------------------------------------------------
    level: 1
        # Measurement: fw = Individual Student Engagement
        fw =~ y1 + y2 + y3

        # Structural: Predicted by student traits (e.g., Sleep, Interest,
        #             Breakfast)
        fw ~ x1 + x2 + x3

    # --------------------------------------------------------------------------
    # LEVEL 2: BETWEEN-CLASSROOM (Classroom Culture Differences)
    # Question: Why is an entire class more engaged than the class next door?
    # --------------------------------------------------------------------------
    level: 2
        # Measurement: fb = Classroom Average Engagement Culture
        fb =~ y1 + y2 + y3

        # Structural: Predicted by class traits (e.g., Teacher Exp., Class Size)
        fb ~ w1 + w2
"
dat <- lavaan::Demo.twolevel
summary(dat)

fit <- asem(mod, data = lavaan::Demo.twolevel, cluster = "cluster")
summary(fit)

## ----- Model selection -------------------------------------------------------

# ANALYTICAL WORKFLOW:
#
# 1. First, the use of a multilevel SEM must be justified. The null model (mod1)
# is run so that the ICCs for the indicators can be calculated. If the ICCs are
# found to be close to zero, it is assumed that classroom clustering does not
# matter.
#
# 2. Assuming a two-level approach is justified by the ICCs, the working theory
# (mod2) is estimated. The global fit is checked, and it is observed whether
# meaningful variance is explained by the predictors.

# 3. Before structural paths are interpreted, a psychometric sanity check is
# required: the factor loadings are forced to be equal across levels (mod3). If
# a massive drop in fit is shown by the LRT, a homology problem is identified.
# Assuming cross-level invariance holds, the model is trimmed.

# 4. A non-essential path is dropped (mod4) and the BIC is checked to see if the
# more parsimonious model is favored.

# 5. Finally, the boundaries of the theory are probed: a localized cross-level
# direct effect (mod5) is tested to see if variance missed by the factor
# structure can be captured.

# MODEL 1: The Unconditional "Null" Model
# (Measurement models only, no structural predictors. Used to establish baseline)
mod1 <- '
    level: 1
        fw =~ y1 + y2 + y3
    level: 2
        fb =~ y1 + y2 + y3
'

# MODEL 2: The Hypothesized Model
# (Original working theory with all hypothesized paths)
mod2 <- '
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2
'

# MODEL 3: The Cross-Level Invariance Model
# (Forces the factor loadings to be equal across the within and between levels.
# y1 is implicitly fixed to 1, so we apply labels to y2 and y3 to constrain them)
mod3 <- '
    level: 1
        fw =~ y1 + load2*y2 + load3*y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + load2*y2 + load3*y3
        fb ~ w1 + w2
'

# MODEL 4: The Parsimonious Model
# (Trimming non-significant paths. Here, removing x3 as a predictor at level 1)
mod4 <- '
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1 + x2
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2
'

# MODEL 5: The Cross-Level Direct Effects Model
# (Adding a specific path from a Level 2 predictor directly to a specific
# indicator's cluster mean, bypassing the factor. e.g., w1 directly impacts y3)
mod5 <- '
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2
        y3 ~ w1
'
