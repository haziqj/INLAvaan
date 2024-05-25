blavaan_namecheck <- function (ov.names)
{
  forbidden <- c("mu", "invthetstar", "invtheta", "nu", "lambda",
                 "eta", "mu_eta", "invpsistar", "invpsi", "alpha", "beta",
                 "rho", "theta", "psi", "rstar", "cov", "ibpsi", "bpsi",
                 "iden", "yvec", paste(".phant", 1:100, sep = ""), "def")
  forbid.idx <- which(ov.names %in% forbidden)
  if (length(forbid.idx) > 0L) {
    stop("blavaan ERROR: the following variable names must be changed:\n",
         "                   ", paste(ov.names[forbid.idx],
                                      collapse = " "))
  }
}

# blavaan_get_ll <- function (postsamp = NULL, lavobject = NULL, measure = "logl",
#                             casewise = FALSE, conditional = FALSE, standata = NULL)
# {
#   if (lavInspect(lavobject, "categorical")) {
#     ll.samp <- get_ll_ord(postsamp, lavobject, measure, casewise,
#                           conditional, standata)
#   }
#   else if (lavInspect(lavobject, "options")$.multilevel) {
#     ll.samp <- get_ll_2l(postsamp, lavobject, standata)
#   }
#   else {
#     ll.samp <- get_ll_cont(postsamp, lavobject, measure,
#                            casewise, conditional)
#   }
#   ll.samp
# }

blavaan_checkcovs <- function (lavobject)
{
  free <- lavInspect(lavobject, "free")
  if (inherits(free[[1]], "matrix"))
    free <- list(free)
  if (nrow(free[[1]]$psi) > 0) {
    psis <- lapply(free, function(x) x$psi)
    psinums <- sapply(psis, function(x) x[lower.tri(x)])
    diagpsi <- all(unlist(psinums) == 0L, na.rm = TRUE)
    fullpsi <- all(unlist(psinums) > 0L, na.rm = TRUE) &
      (anyDuplicated(unlist(psinums), MARGIN = 0) == 0L)
  }
  else {
    diagpsi <- FALSE
    fullpsi <- TRUE
  }
  if (nrow(free[[1]]$theta) > 0) {
    thets <- lapply(free, function(x) x$theta)
    thetnums <- sapply(thets, function(x) x[lower.tri(x)])
    diagthet <- all(unlist(thetnums) == 0L, na.rm = TRUE)
    fullthet <- all(unlist(thetnums) > 0L, na.rm = TRUE) &
      (anyDuplicated(unlist(thetnums), MARGIN = 0) == 0L)
  }
  else {
    diagthet <- FALSE
    fullthet <- TRUE
  }
  list(diagpsi = diagpsi, fullpsi = fullpsi, diagthet = diagthet,
       fullthet = fullthet)
}

blavaan_set_mv0 <- function (partable, ov.names, ngroups)
{
  mv0 <- which(partable$op == "~~" & partable$lhs %in% ov.names &
                 partable$rhs == partable$lhs & partable$group == 1 &
                 partable$free == 0 & partable$ustart == 0)
  if (length(mv0) > 0) {
    ovn <- partable$lhs[mv0]
    for (i in 1:length(ovn)) {
      for (j in 1:ngroups) {
        mvloc <- which(partable$op == "~~" & partable$lhs ==
                         ovn[i] & partable$rhs == partable$lhs & partable$group ==
                         j & partable$free == 0 & partable$ustart ==
                         0)
        lvloc <- which(partable$op == "=~" & partable$rhs ==
                         ovn[i] & partable$group == j)
        lvreg <- which(partable$op == "~" & partable$lhs %in%
                         c(ovn[i], partable$lhs[lvloc]) & partable$group ==
                         j)
        lvcov <- which(partable$op == "~~" & (partable$lhs %in%
                                                partable$lhs[lvloc] | partable$rhs %in% partable$lhs[lvloc]) &
                         partable$lhs != partable$rhs)
        if (length(lvloc) + length(lvreg) + length(lvcov) >
            1) {
          if (length(mvloc) > 1) {
            stop("blavaan ERROR: Problem with ov variances fixed to 0.")
          }
          partable$ustart[mvloc] <- 0.001
          message(paste("blavaan NOTE: The variance of variable",
                        ovn[i], "in group", j, "has been fixed to .001 instead of 0 (necessary for conditional model specification).\n"))
        }
        else {
          lvname <- partable$lhs[lvloc]
          lvvar <- which(partable$lhs == lvname & partable$rhs ==
                           lvname & partable$op == "~~" & partable$group ==
                           j)
          tmpfree <- partable$free[lvvar]
          tmpustart <- partable$ustart[lvvar]
          tmpplabel <- partable$plabel[lvvar]
          tmpstart <- partable$start[lvvar]
          partable$free[lvvar] <- partable$free[mvloc]
          partable$ustart[lvvar] <- partable$ustart[mvloc]
          partable$plabel[lvvar] <- partable$plabel[mvloc]
          partable$start[lvvar] <- partable$start[mvloc]
          partable$free[mvloc] <- tmpfree
          partable$ustart[mvloc] <- tmpustart
          partable$plabel[mvloc] <- tmpplabel
          partable$start[mvloc] <- tmpstart
        }
      }
    }
  }
  partable
}

blavaan_set_stancovs <- function (partable, std.lv)
{
  if (is.na(match("prior", names(partable))))
    partable$prior <- rep("", length(partable$id))
  partable <- lavaan::lavMatrixRepresentation(partable, add.attributes = TRUE)
  defpar <- which(partable$op == ":=")
  if (length(defpar) > 0) {
    partable$mat[defpar] <- "def"
    partable$row[defpar] <- 1:length(defpar)
    partable$col[defpar] <- 1
    partable$group[defpar] <- 1
  }
  if (std.lv) {
    partable$mat[partable$mat == "psi"] <- "psiUNC"
  }
  covpars <- which(partable$op == "~~" & partable$lhs != partable$rhs &
                     partable$free > 0L)
  partable$rhoidx <- rep(NA, length(partable$id))
  blkrow <- rep(NA, ncol(partable))
  if (length(covpars) > 0) {
    mvcov <- 0
    lvcov <- 0
    for (i in 1:length(covpars)) {
      eq.const <- FALSE
      eq.idx <- which(partable$op == "==" & partable$rhs ==
                        partable$plabel[covpars[i]])
      if (length(eq.idx) > 0) {
        eq.const <- TRUE
        full.idx <- which(partable$plabel == partable$lhs[eq.idx])
        old.idx <- partable$rhoidx[full.idx]
      }
      tmprows <- nrow(partable) + 1
      partable <- rbind(partable, blkrow)
      partable$group[tmprows] <- partable$block[tmprows] <- partable$group[covpars[i]]
      partable$lhs[tmprows] <- partable$lhs[covpars[i]]
      partable$rhs[tmprows] <- partable$rhs[covpars[i]]
      if (partable$mat[covpars[i]] == "theta") {
        if (!eq.const) {
          mvcov <- mvcov + 1
          covidx <- mvcov
        }
        partable$mat[tmprows] <- "rho"
      }
      else {
        if (!eq.const) {
          lvcov <- lvcov + 1
          covidx <- lvcov
        }
        partable$mat[tmprows] <- "lvrho"
      }
      partable$op[tmprows] <- "~~"
      partable$row[tmprows] <- partable$row[covpars[i]]
      partable$col[tmprows] <- partable$col[covpars[i]]
      partable$group[tmprows] <- partable$group[covpars[i]]
      v1var <- which(partable$lhs == partable$lhs[covpars[i]] &
                       partable$rhs == partable$lhs[covpars[i]] & partable$group ==
                       partable$group[covpars[i]] & partable$op == "~~")
      tmpv1 <- paste(partable$mat[v1var], "[", partable$row[v1var],
                     ",", partable$col[v1var], ",", partable$group[v1var],
                     "]", sep = "")
      v2var <- which(partable$lhs == partable$rhs[covpars[i]] &
                       partable$rhs == partable$rhs[covpars[i]] & partable$group ==
                       partable$group[covpars[i]] & partable$op == "~~")
      tmpv2 <- paste(partable$mat[v2var], "[", partable$row[v2var],
                     ",", partable$col[v2var], ",", partable$group[v2var],
                     "]", sep = "")
      if (partable$prior[covpars[i]] != "") {
        partable$prior[tmprows] <- partable$prior[covpars[i]]
      }
      else {
        partable$prior[tmprows] <- ""
      }
      if (eq.const) {
        partable$ustart[covpars[i]] <- paste0(partable$mat[full.idx],
                                              "[", partable$row[full.idx], ",", partable$col[full.idx],
                                              ",", partable$group[full.idx], "]")
        partable$ustart[tmprows] <- paste0(partable$ustart[covpars[i]],
                                           "/sqrt(", tmpv1, "*", tmpv2, ")")
      }
      else {
        partable$rhoidx[tmprows] <- partable$rhoidx[covpars[i]] <- covidx
        partable$ustart[covpars[i]] <- paste0(partable$mat[tmprows],
                                              "[", partable$row[tmprows], ",", partable$col[tmprows],
                                              ",", partable$group[tmprows], "] * sqrt(",
                                              tmpv1, " * ", tmpv2, ")")
        partable$start[tmprows] <- partable$start[covpars[i]]
      }
      partable$free[tmprows] <- as.integer(partable$free[covpars[i]])
      partable$free[covpars[i]] <- 0L
      partable$plabel[tmprows] <- paste(".p", tmprows,
                                        ".", sep = "")
      partable$label[tmprows] <- ""
      partable$exo[tmprows] <- 0L
    }
    ptcov <- partable[covpars, ]
    partable <- partable[-covpars, ]
    partable <- rbind(partable, ptcov)
  }
  partable
}

blavaan_set_stanpars <- function (TXT2, partable, nfree, dp, ov.names)
{
  t1 <- paste(rep(" ", 2L), collapse = "")
  t2 <- paste(rep(" ", 4L), collapse = "")
  t3 <- paste(rep(" ", 6L), collapse = "")
  eqop <- "="
  commop <- "// "
  eolop <- ";"
  TXT3 <- paste("\n", t1, commop, "Priors", sep = "")
  partable$freeparnums <- rep(0, length(partable$id))
  matparnums <- rep(0, length(nfree))
  parvecnum <- 0
  for (i in 1:nrow(partable)) {
    miscignore <- partable$mat[i] == ""
    eqpar <- which((partable$rhs == partable$plabel[i] &
                      partable$op == "==") | (grepl("rho", partable$mat[i]) &
                                                is.na(partable$rhoidx[i])))
    compeq <- which((partable$lhs == partable$plabel[i] |
                       partable$lhs == partable$label[i]) & partable$op %in%
                      c("==", ":=") & grepl("\\+|-|/|\\*|\\(|\\)|\\^",
                                            partable$rhs))
    fixed <- partable$free[i] == 0 & partable$op[i] != ":="
    if (length(eqpar) > 0 | length(compeq) > 0 | fixed |
        miscignore) {
      next
    }
    else {
      partype <- match(partable$mat[i], names(nfree))
      matparnums[partype] <- matparnums[partype] + 1
      partable$freeparnums[i] <- matparnums[partype]
    }
  }
  for (i in 1:nrow(partable)) {
    if (partable$mat[i] != "" | partable$op[i] == ":=") {
      eqpar <- which(partable$rhs == partable$plabel[i] &
                       partable$op == "==")
      defeq <- (partable$op[i] == ":=") | (partable$op[i] ==
                                             "==" & grepl("\\+|-|/|\\*|\\(|\\)|\\^", partable$rhs[i]))
      compeq <- which((partable$lhs == partable$plabel[i] |
                         partable$lhs == partable$label[i]) & partable$op %in%
                        c("==", ":=") & grepl("\\+|-|/|\\*|\\(|\\)|\\^",
                                              partable$rhs))
      TXT2 <- paste(TXT2, "\n", t1, partable$mat[i], "[",
                    partable$row[i], ",", partable$col[i], ",", partable$group[i],
                    "] ", eqop, " ", sep = "")
      if (grepl("rho", partable$mat[i]) & is.na(partable$ustart[i]) &
          partable$free[i] > 0) {
        TXT2 <- paste(TXT2, "-1 + 2*", sep = "")
      }
      if ((partable$free[i] == 0 & partable$op[i] != ":=") |
          (grepl("rho", partable$mat[i]) & !is.na(partable$ustart[i]))) {
        if (is.na(partable$ustart[i])) {
          TXT2 <- paste(TXT2, partable$start[i], eolop,
                        sep = "")
        }
        else {
          TXT2 <- paste(TXT2, partable$ustart[i], eolop,
                        sep = "")
        }
      }
      else if (length(eqpar) > 0) {
        eqpar <- which(partable$plabel == partable$lhs[eqpar] |
                         partable$label == partable$lhs[eqpar])
        if (length(eqpar) > 1)
          eqpar <- eqpar[which(partable$label[eqpar] ==
                                 partable$plabel[eqpar])]
        if (partable$freeparnums[eqpar] == 0) {
          eqtxt <- paste(partable$mat[eqpar], "[", partable$row[eqpar],
                         ",", partable$col[eqpar], ",", partable$group[eqpar],
                         "]", sep = "")
        }
        else {
          eqtxt <- paste(partable$mat[eqpar], "free[",
                         partable$freeparnums[eqpar], "]", sep = "")
        }
        vpri <- grepl("\\[var\\]", partable$prior[eqpar])
        spri <- grepl("\\[sd\\]", partable$prior[eqpar])
        if (!vpri & (grepl("theta", partable$mat[i]) |
                     grepl("psi", partable$mat[i]))) {
          sq <- ifelse(spri, "2", "-1")
          TXT2 <- paste(TXT2, "pow(", eqtxt, ",", sq,
                        ")", eolop, sep = "")
        }
        else {
          TXT2 <- paste(TXT2, eqtxt, eolop, sep = "")
        }
      }
      else if (defeq | length(compeq) > 0) {
        if (length(compeq) == 0)
          compeq <- i
        rhsvars <- all.vars(parse(file = "", text = partable$rhs[compeq]))
        if (compeq == i) {
          pvnum <- match(rhsvars, partable$label)
        }
        else {
          pvnum <- match(rhsvars, partable$plabel)
          if (is.na(pvnum[1]))
            pvnum <- match(rhsvars, partable$label)
        }
        rhstrans <- paste(partable$mat[pvnum], "[", partable$row[pvnum],
                          ",", partable$col[pvnum], ",", partable$group[pvnum],
                          "]", sep = "")
        defvars <- which(partable$mat[pvnum] == "def")
        if (length(defvars) > 0) {
          defpt <- pvnum[defvars]
          rhstrans[defvars] <- paste0(partable$mat[defpt],
                                      "[", partable$row[defpt], ",", partable$col[defpt],
                                      ",", partable$group[defpt], "]")
        }
        oldjageq <- partable$rhs[compeq]
        transtab <- as.list(rhstrans)
        names(transtab) <- rhsvars
        jagexpr <- parse(text = oldjageq)[[1]]
        jageq <- do.call("substitute", list(jagexpr,
                                            transtab))
        jageq <- paste(deparse(jageq, width.cutoff = 500),
                       collapse = "")
        jageq <- gsub("\"", "", jageq)
        TXT2 <- paste(TXT2, jageq, eolop, sep = "")
      }
      else {
        TXT3 <- paste(TXT3, "\n", t1, "target += ", sep = "")
        if (partable$prior[i] == "") {
          if (partable$mat[i] == "lvrho") {
            partype <- grep("rho", names(dp))
          }
          else if (grepl("star", partable$mat[i])) {
            pname <- paste("i", strsplit(partable$mat[i],
                                         "star")[[1]][1], sep = "")
            partype <- grep(pname, names(dp))
          }
          else if (grepl("UNC", partable$mat[i])) {
            pname <- strsplit(partable$mat[i], "UNC")[[1]][1]
            partype <- grep(pname, names(dp))
          }
          else if (partable$mat[i] == "alpha" & partable$lhs[i] %in%
                   ov.names) {
            partype <- grep("nu", names(dp))
          }
          else {
            partype <- grep(partable$mat[i], names(dp))
          }
          if (length(partype) > 1)
            partype <- partype[1]
          partable$prior[i] <- dp[partype]
          if (partable$mat[i] %in% c("rho", "lvrho")) {
            covr <- grep(paste0(partable$mat[i], "[",
                                partable$row[i], ",", partable$col[i],
                                ",", partable$group[i], "]"), partable$ustart,
                         fixed = TRUE)
            partable$prior[covr] <- dp[partype]
          }
        }
        if (grepl(")[", partable$prior[i], fixed = TRUE)) {
          jagpri <- paste0(strsplit(partable$prior[i],
                                    ")[", fixed = TRUE)[[1]][1], ")")
        }
        else {
          jagpri <- partable$prior[i]
        }
        vpri <- grepl("\\[var\\]", partable$prior[i])
        spri <- grepl("\\[sd\\]", partable$prior[i])
        ppri <- grepl("\\[prec\\]", partable$prior[i])
        if (vpri) {
          jagpri <- strsplit(partable$prior[i], "\\[var")[[1]][1]
        }
        else if (spri) {
          jagpri <- strsplit(partable$prior[i], "\\[sd")[[1]][1]
        }
        else if (ppri) {
          jagpri <- strsplit(partable$prior[i], "\\[prec")[[1]][1]
        }
        else {
          jagpri <- partable$prior[i]
        }
        splpri <- unlist(strsplit(jagpri, "\\("))
        jagpdist <- paste0(splpri[1], "_lpdf(")
        if (grepl("[", splpri[-1], fixed = TRUE)) {
          splpar <- strsplit(splpri[2], "],")
          matpar <- splpar[[1]][1]
          fpar <- strsplit(TXT2, paste0(matpar, "] = "),
                           fixed = TRUE)[[1]][2]
          fpar <- strsplit(fpar, ";")[[1]][1]
          jagpparm <- paste(fpar, splpar[[1]][2], sep = ",")
        }
        else {
          jagpparm <- paste(splpri[-1], collapse = "(")
        }
        if (!vpri & (grepl("theta", partable$mat[i]) |
                     grepl("psi", partable$mat[i]))) {
          sq <- ifelse(spri, "2", "-1")
          TXT2 <- paste(TXT2, "pow(", partable$mat[i],
                        "free[", partable$freeparnums[i], "],", sq,
                        ")", eolop, sep = "")
        }
        else {
          TXT2 <- paste(TXT2, partable$mat[i], "free[",
                        partable$freeparnums[i], "]", eolop, sep = "")
        }
        TXT3 <- paste0(TXT3, jagpdist, partable$mat[i],
                       "free[", partable$freeparnums[i], "] | ", jagpparm,
                       eolop)
      }
    }
  }
  list(TXT2 = TXT2, TXT3 = TXT3, partable = partable)
}


blavaan_set_inits_stan <- function (partable, nfree, n.chains, inits, ntot = NULL, nlvno0 = 0)
{
  initvals <- vector("list", n.chains)
  names(initvals) <- paste("c", 1:n.chains, sep = "")
  pveclen <- nfree[nfree > 0]
  initmats <- list()
  for (i in 1:length(pveclen)) {
    initmats <- c(initmats, list(array(NA, dim = pveclen[i])))
  }
  names(initmats) <- paste0(names(pveclen), "free")
  if (nlvno0 > 0) {
    initmats <- c(initmats, list(etafree = array(1, dim = c(ntot,
                                                            nlvno0))))
  }
  for (i in 1:n.chains) {
    initvals[[i]] <- initmats
  }
  partable$freeparnums[is.na(partable$freeparnums)] <- 0
  freepartable <- partable[partable$freeparnums > 0, ]
  if ("rhoidx" %in% names(freepartable)) {
    rhorows <- which(!is.na(freepartable$rhoidx) & freepartable$free >
                       0 & freepartable$mat == "rho")
    if (length(rhorows) > 0) {
      freepartable$freeparnums[rhorows] <- 1:length(rhorows)
    }
    lvrhorows <- which(!is.na(freepartable$rhoidx) & freepartable$free >
                         0 & freepartable$mat == "lvrho")
    if (length(rhorows) > 0) {
      freepartable$freeparnums[lvrhorows] <- 1:length(lvrhorows)
    }
  }
  rloc <- paste0(system.file("R", package = "rstan"), "/sysdata")
  lazyLoad(rloc)
  rosetta <- rosetta
  prilist <- dist2r(freepartable$prior, target = "stan")
  for (i in 1:nrow(freepartable)) {
    if (inits == "prior") {
      pricom <- prilist[[i]]
      if (grepl("dnorm", pricom[1])) {
        pricom[3] <- "1"
        if (grepl("lambda", freepartable$mat[i]) | grepl("beta",
                                                         freepartable$prior[i])) {
          pricom[1] <- "dunif"
          pricom[2] <- ".75"
          pricom[3] <- "2"
        }
      }
      if (grepl("dbeta", pricom[1])) {
        pricom[2] <- "100"
        pricom[3] <- "100"
      }
      pricom[1] <- gsub("^d", "r", pricom[1])
      ivs <- try(do.call(pricom[1], list(n.chains, as.numeric(pricom[2]),
                                         as.numeric(pricom[3]))), silent = TRUE)
      if (inherits(ivs, "try-error")) {
        ivs <- rep(1, n.chains)
      }
      else if (pricom[1] == "rgamma" & !grepl("[sd]", freepartable$prior[i],
                                              fixed = TRUE) & !grepl("[var]", freepartable$prior[i],
                                                                     fixed = TRUE)) {
        ivs <- 1/ivs
      }
    }
    else {
      ivs <- rep(freepartable$start[i], n.chains)
    }
    if (grepl("beta", freepartable$prior[i])) {
      ivs <- rep(0.5, n.chains)
    }
    for (j in 1:n.chains) {
      matidx <- which(names(initvals[[j]]) == paste0(freepartable$mat[i],
                                                     "free"))
      initvals[[j]][[matidx]][freepartable$freeparnums[i]] <- ivs[j]
    }
  }
  initvals
}

dist2r <- function (priors, target)
{
  gsub("sqrt\\((.*)\\)\\).*", "\\1^.5\\)", priors)
  if (target == "jags") {
    out <- jagsdist2r(priors)
  }
  else if (target == "stan") {
    rloc <- paste0(system.file("R", package = "rstan"), "/sysdata")
    lazyLoad(rloc)
    rosetta <- rosetta
    prisplit <- strsplit(priors, "[, ()]+")
    pridist <- sapply(prisplit, function(x) x[1])
    newdist <- rosetta$RFunction[match(pridist, rosetta$StanFunction)]
    for (i in 1:length(newdist)) {
      if (!is.na(newdist[i]))
        prisplit[[i]][1] <- newdist[i]
    }
    out <- prisplit
  }
  out
}
