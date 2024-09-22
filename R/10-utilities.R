cli_messages <- c(
  "Laplace-ing through p dimensions",
  "Summoning Bayesian spirits",
  "Casting statistical spells",
  "Conjuring INLA magic",
  "Channeling Laplace's wizardry",
  "Harnessing the power of priors",
  "Diving into the probability pool",
  "Navigating the seas of stochasticity"
)

theta_to_rho <- function(x) {
  pos_only <- FALSE

  u <- 1 / (1 + exp(-x))
  if (pos_only) {
    rho <- u
  } else {
    rho <- 2 * u - 1
  }

  rho
}

rho_to_theta <- function(x) {
  pos_only <- FALSE

  x[x > 1] <- 1
  if (pos_only) {
    x[x < 0] <- 0
    u <- x
  } else {
    x[x < -1] <- -1
    u <- (x + 1) / 2
  }

  theta <- log(u / (1 - u))
  theta
}

safe_solve <- function(x) {
  try_chol <- try(chol(x), silent = TRUE)
  if (any(class(try_chol) %in% "try-error")) {
    xm <- forceSymmetric(Matrix(x))
    return(solve(xm))
  } else {
    return(chol2inv(try_chol))
  }
}


lav_object_inspect_modelmatrices <- function(object, what = "free", # nolint
                                             type = "free", add.labels = FALSE, add.class = FALSE,
                                             list.by.group = FALSE,
                                             drop.list.single.group = FALSE) {

  glist <- object@Model@GLIST

  current.verbose <- lav_verbose()
  if (what == "dx.free") {
    if (lav_verbose(FALSE)) on.exit(lav_verbose(current.verbose), TRUE)
    tmp.dx <- lav_model_gradient(
      lavmodel       = object@Model,
      GLIST          = NULL,
      lavsamplestats = object@SampleStats,
      lavdata        = object@Data,
      lavcache       = object@Cache,
      type           = "free",
      group.weight   = TRUE,
      ceq.simple     = TRUE,
      Delta          = NULL)
  } else if (what == "dx.all") {
    if (lav_verbose(FALSE)) on.exit(lav_verbose(current.verbose), TRUE)
    glist <- lav_model_gradient(lavmodel   = object@Model,
                                GLIST          = NULL,
                                lavsamplestats = object@SampleStats,
                                lavdata        = object@Data,
                                lavcache       = object@Cache,
                                type           = "allofthem",
                                group.weight   = TRUE,
                                ceq.simple     = FALSE,
                                Delta          = NULL)
    names(glist) <- names(object@Model@GLIST)
  } else if (what == "std.all") {
    tmp.std <- lav_standardize_all(object)
  } else if (what == "std.lv") {
    tmp.std <- lav_standardize_lv(object)
  } else if (what == "std.nox") {
    tmp.std <- lav_standardize_all_nox(object)
  } else if (what == "se") {
    tmp.se <- lav_object_inspect_se(object)
  } else if (what == "std.se") {
    tmp.se <- lav_object_inspect_std_se(object)
  } else if (what == "start") {
    tmp.start <- lav_object_inspect_start(object)
  } else if (what == "est") {
    tmp.est <- lav_object_inspect_est(object)
  } else if (what == "est.unrotated") {
    if (!is.null(object@Options$rotation) &&
        object@Options$rotation == "none") {
      tmp.est <- lav_object_inspect_est(object, unrotated = FALSE)
    } else {
      tmp.est <- lav_object_inspect_est(object, unrotated = TRUE)
    }
  }

  for (mm in seq_along(glist)) {

    if (add.labels) {
      dimnames(glist[[mm]]) <- object@Model@dimNames[[mm]]
    }

    if (what == "free") {
      # fill in free parameter counts
      if (type == "free") {
        m.el.idx <- object@Model@m.free.idx[[mm]]
        x.el.idx <- object@Model@x.free.idx[[mm]]
        # } else if(type == "unco") {
        #    m.el.idx <- object@Model@m.unco.idx[[mm]]
        #    x.el.idx <- object@Model@x.unco.idx[[mm]]
      } else if (type == "partable") {
        m.el.idx <- object@Model@m.user.idx[[mm]]
        x.el.idx <- object@Model@x.user.idx[[mm]]
      } else {
        lav_msg_stop(gettextf(
          "%1$s argument unknown: %2$s",
          "type", lav_msg_view(type)
        ))
      }
      # erase everything
      glist[[mm]][, ] <- 0.0
      glist[[mm]][m.el.idx] <- x.el.idx
    } else if (what == "se" || what == "std.se") {
      # fill in standard errors
      m.user.idx <- object@Model@m.user.idx[[mm]]
      x.user.idx <- object@Model@x.user.idx[[mm]]
      # erase everything
      glist[[mm]][, ] <- 0.0
      glist[[mm]][m.user.idx] <- tmp.se[x.user.idx]
    } else if (what == "start") {
      # fill in starting values
      m.user.idx <- object@Model@m.user.idx[[mm]]
      x.user.idx <- object@Model@x.user.idx[[mm]]
      glist[[mm]][m.user.idx] <- tmp.start[x.user.idx]
    } else if (what %in% c("est", "est.unrotated")) {
      # fill in estimated parameter values
      m.user.idx <- object@Model@m.user.idx[[mm]]
      x.user.idx <- object@Model@x.user.idx[[mm]]
      glist[[mm]][m.user.idx] <- tmp.est[x.user.idx]
    } else if (what == "dx.free") {
      # fill in derivatives free parameters
      m.el.idx <- object@Model@m.free.idx[[mm]]
      x.el.idx <- object@Model@x.free.idx[[mm]]
      # erase everything
      glist[[mm]][, ] <- 0.0
      glist[[mm]][m.el.idx] <- tmp.dx[x.el.idx]
    } else if (what %in% c("std.all", "std.lv", "std.nox")) {
      m.user.idx <- object@Model@m.user.idx[[mm]]
      x.user.idx <- object@Model@x.user.idx[[mm]]
      glist[[mm]][m.user.idx] <- tmp.std[x.user.idx]
    }

    # class
    if (add.class) {
      if (object@Model@isSymmetric[mm]) {
        class(glist[[mm]]) <- c("lavaan.matrix.symmetric", "matrix")
      } else {
        class(glist[[mm]]) <- c("lavaan.matrix", "matrix")
      }
    }
  }

  # try to reflect `equality constraints'
  con.flag <- FALSE
  if (what == "free" && object@Model@eq.constraints) {
    # extract constraints from parameter table
    partable <- parTable(object)
    tmp.con <-  partable[partable$op %in% c("==", "<", ">"),
                         c("lhs", "op", "rhs")]
    rownames(tmp.con) <- NULL

    # replace 'labels' by parameter numbers
    tmp.id <- lav_partable_constraints_label_id(partable)
    tmp.label <- names(tmp.id)
    for (con in seq_len(nrow(tmp.con))) {
      # lhs
      lhs.labels <- all.vars(as.formula(paste("~", tmp.con[con, "lhs"])))

      if (length(lhs.labels) > 0L) {
        # par id
        lhs.freeid <- tmp.id[match(lhs.labels, tmp.label)]

        # substitute
        tmp <- tmp.con[con, "lhs"]
        for (pat in seq_along(lhs.labels)) {
          tmp <- sub(lhs.labels[pat], lhs.freeid[pat], tmp)
        }
        tmp.con[con, "lhs"] <- tmp
      }

      # rhs
      rhs.labels <- all.vars(as.formula(paste("~", tmp.con[con, "rhs"])))

      if (length(rhs.labels) > 0L) {
        # par id
        rhs.freeid <- tmp.id[match(rhs.labels, tmp.label)]
        # substitute
        tmp <- tmp.con[con, "rhs"]
        for (pat in seq_along(rhs.labels)) {
          tmp <- sub(rhs.labels[pat], rhs.freeid[pat], tmp)
        }
        tmp.con[con, "rhs"] <- tmp
      }
    } # con

    # add this info at the top
    # glist <- c(constraints = list(tmp.con), glist)
    # no, not a good idea, it does not work with list.by.group

    # add it as a 'header' attribute?
    attr(tmp.con, "header") <- "Note: model contains equality constraints:"
    con.flag <- TRUE
  }

  # should we group them per block?
  if (list.by.group) {
    lavmodel       <- object@Model
    nmat           <- lavmodel@nmat

    return.value <- vector("list", length = lavmodel@nblocks)
    for (b in seq_len(lavmodel@nblocks)) {
      # which mm belong to this block?
      mm.in.group <- 1:nmat[b] + cumsum(c(0, nmat))[b]

      return.value[[b]] <- glist[mm.in.group]
    }

    if (lavmodel@nblocks == 1L && drop.list.single.group) {
      return.value <- return.value[[1]]
    } else if (lavmodel@nblocks > 1L) {
      names(return.value) <- object@Data@block.label
    }
  } else {
    return.value <- glist
  }

  # header
  if (con.flag) {
    attr(return.value, "header") <- tmp.con
  }

  # lavaan.list
  if (add.class) {
    class(return.value) <- c("lavaan.list", "list")
  }

  return.value
}








# 1. From theta to PT
# 2. From PT to Sigma and hence Q
# 3. From PT to log priors

# get_p <- function(.PT) {
#   nrow(PT_to_matrices(.PT = .PT)$Theta)
# }
#
# theta_to_PT <- function(.PT) {
#
#   PT <- .PT
#   idx_free <- which(PT$free > 0)
# print(theta)
#   par_vals <- theta
#   names(par_vals) <- PT$mat[idx_free]
#   par_vals[names(par_vals) %in% c("theta", "psi")] <- exp(
#     par_vals[names(par_vals) %in% c("theta", "psi")]
#   )
#   PT$est[idx_free] <- par_vals
#
#   PT
# }
#
# PT_to_matrices <- function(.PT) {
#   # Lambda
#   lambdas <- .PT[grep("lambda", .PT$mat), c("est", "row", "col")]
#   Lambda <- Matrix::sparseMatrix(i = lambdas$row,
#                                  j = lambdas$col,
#                                  x = lambdas$est)
#
#   # Theta
#   thetas <- .PT[grep("theta", .PT$mat), c("est", "row", "col")]
#   Theta <- Matrix::sparseMatrix(i = thetas$row,
#                                 j = thetas$col,
#                                 x = thetas$est)
#   Thetat <- Matrix::sparseMatrix(j = thetas$row,
#                                  i = thetas$col,
#                                  x = thetas$est)
#   Theta <- (Theta + Thetat) / 2
#
#   # Psi
#   psis <- .PT[grep("psi", .PT$mat), c("est", "row", "col")]
#   Psi <- Matrix::sparseMatrix(i = psis$row,
#                               j = psis$col,
#                               x = psis$est)
#
#   list(
#     Lambda = Lambda,
#     Theta = Theta,
#     Psi = Psi
#   )
# }
#
# # Don't forget to write test functions!
# parse_dist <- function(x, debug = FALSE) {
#   if (grepl("normal", x)) {
#     res <- strsplit(x, ",|\\(|\\)")[[1]]
#     mean <- as.numeric(res[2])
#     sd <- sqrt(as.numeric(res[3]))
#     if (isTRUE(debug)) {
#       return(list(dist = "normal", mean = mean, sd = sd))
#     } else {
#       return(function(theta) dnorm(theta, mean = mean, sd = sd, log = TRUE))
#     }
#   }
#
#   if (grepl("gamma", x)) {
#     res <- strsplit(x, ",|\\(|\\)")[[1]]
#     shape <- as.numeric(res[2])
#     rate <- as.numeric(res[3])
#     # FIXME: something to do with [sd] or not?
#     if (isTRUE(debug)) {
#       return(list(dist = "gamma", shape = shape, rate = rate))
#     } else {
#       return(function(theta) dgamma(theta, shape = shape, rate = rate,
#                                     log = TRUE))
#     }
#
#     # FIXME: Also, since we parameterise as var = log(theta), need to add Jacobian
#   }
#
#   if (grepl("beta", x)) {
#     res <- strsplit(x, ",|\\(|\\)")[[1]]
#     shape1 <- as.numeric(res[2])
#     shape2 <- as.numeric(res[3])
#     # FIXME: something to do with [sd] or not?
#     if (isTRUE(debug)) {
#       return(list(dist = "beta", shape1 = shape1, shape2 = shape2))
#     } else {
#       return(function(theta) dgamma(theta, shape1 = shape1, shape2 = shape2,
#                                     log = TRUE))
#     }
#   }
# }

# parse_dist("normal(0,10)", TRUE)
# parse_dist("gamma(1,.5)[sd]", TRUE)
# parse_dist("beta(1,1)", TRUE)
