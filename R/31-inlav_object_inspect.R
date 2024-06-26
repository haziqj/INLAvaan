## inspect blavaan object (wrapper around lavInspect with
## some additions)
blavTech <- function(blavobject, what, ...) {

  blavInspect(blavobject, what, ...)
}

## use lavInspect everywhere we can:
blavInspect <- function(blavobject, what, ...) {

  stopifnot(inherits(blavobject, "INLAvaan"))

  what <- tolower(what)

  dotdotdot <- list(...)
  dotNames <- names(dotdotdot)
  add.labels <- TRUE
  if(any(dotNames == "add.labels")) add.labels <- dotdotdot$add.labels
  level <- 1L
  if(any(dotNames == "level")) level <- dotdotdot$level

  jagtarget <- lavInspect(blavobject, "options")$target == "jags"

  ## whats unique to blavaan
  blavwhats <- c("start", "starting.values", "inits", "psrf",
                 "ac.10", "neff", "mcmc", "draws", "samples",
                 "n.chains", "cp", "dp", "postmode", "postmean",
                 "postmedian", "hpd", "jagnames", "stannames",
                 "fscores", "lvs", "fsmeans", "lvmeans", "mcobj",
                 "rhat", "n_eff", "nchain", "nchains")

  ## blavwhats that don't require do.fit
  blavnofit <- c("start", "starting.values", "inits", "n.chains", "cp", "dp",
                 "jagnames", "stannames", "nchain", "nchains")

  ## whats that are not handled
  nowhats <- c("mi", "modindices", "modification.indices",
               "wls.est", "wls.obs", "wls.v")

  if(what %in% blavwhats){
    if(!(what %in% blavnofit) & !blavobject@Options$do.fit){
      stop(paste0("blavaan ERROR: ", what, " does not exist when do.fit = FALSE"))
    }
    if(jagtarget){
      idx <- blavobject@ParTable$jagpnum
      idx <- idx[!is.na(idx)]
    } else {
      idx <- blavobject@ParTable$stansumnum
      if("pxnames" %in% names(blavobject@ParTable)){
        drows <- grepl("^def", blavobject@ParTable$pxnames)
      } else {
        drows <- grepl("def", blavobject@ParTable$mat)
      }
      idx <- idx[blavobject@ParTable$free > 0 | drows]
    }
    labs <- lav_partable_labels(blavobject@ParTable, type = "free")
    if(what %in% c("start", "starting.values", "inits")){
      blavobject@external$inits
    } else if(what %in% c("psrf", "ac.10", "neff", "rhat", "n_eff")){
      if(jagtarget){
        # mcmcsumm <- blavobject@external$mcmcout$summaries
      } else {
        # mcmcsumm <- rstan::summary(blavobject@external$mcmcout)$summary
        mcmcsumm <- NULL
      }
      if(what %in% c("psrf", "rhat")){
        if(jagtarget){
          OUT <- mcmcsumm[idx,'psrf']
        } else {
          OUT <- mcmcsumm[idx,'Rhat']
        }
      }else if(what == "ac.10"){
        if(jagtarget){
          OUT <- mcmcsumm[idx,'AC.10']
        } else {
          stop("blavaan ERROR: autocorrelation stat currently unavailable for Stan.")
        }
      } else {
        if(jagtarget){
          OUT <- mcmcsumm[idx,'SSeff']
        } else {
          OUT <- mcmcsumm[idx,'n_eff']
        }
      }
      if(add.labels) names(OUT) <- labs
      OUT
    } else if(what %in% c("mcmc", "draws", "samples", "hpd")){
      ## add defined parameters to labels
      pt <- blavobject@ParTable
      pt$free[pt$op == ":="] <- max(pt$free, na.rm = TRUE) + 1:sum(pt$op == ":=")
      labs <- lav_partable_labels(pt, type = "free")
      draws <- make_mcmc(blavobject@external$mcmcout)
      draws <- lapply(draws, function(x) mcmc(x[, idx, drop = FALSE]))
      draws <- mcmc.list(draws)
      if(add.labels) {
        for (i in 1:length(draws)) {
          colnames(draws[[i]]) <- labs
        }
      }
      if(what == "hpd"){
        pct <- .95
        if("level" %in% dotNames) pct <- dotdotdot$level
        if("prob" %in% dotNames) pct <- dotdotdot$prob
        draws <- mcmc(do.call("rbind", draws))
        draws <- HPDinterval(draws, pct)
      }
      draws
    } else if(what == "mcobj"){
      blavobject@external$mcmcout
    } else if(what %in% c("fscores","lvs","fsmeans","lvmeans")){
      if(jagtarget){
        etas <- any(blavobject@external$mcmcout$monitor == "eta")
      } else {
        etas <- any(grepl("^eta", rownames(blavobject@external$stansumm)))
      }

      ## how many lvs, excluding phantoms
      lvmn <- lavInspect(blavobject, "mean.lv")
      if(!inherits(lvmn, "list")){
        lvmn <- list(lvmn)
      }
      if(level == 1L){
        nlv <- length(lvmn[[1]])
        nsamp <- sum(lavInspect(blavobject, "nobs"))
      } else {
        nlv <- length(lvmn[[2]])
        nsamp <- unlist(lavInspect(blavobject, "nclusters"))
      }

      if(nlv == 0) stop("blavaan ERROR: no latent variables are at this level of the model")
      if(!etas) stop("blavaan ERROR: factor scores not saved; set save.lvs=TRUE")

      if(level == 2L & all(nsamp == 1)) stop("blavaan ERROR: level 2 was requested but the model is not multilevel")

      draws <- make_mcmc(blavobject@external$mcmcout, blavobject@external$stanlvs)

      if(jagtarget){
        drawcols <- grep("^eta\\[", colnames(draws[[1]]))
        ## remove phantoms
        drawcols <- drawcols[1:(nlv * nsamp)]
      } else {
        if(level == 1L){
          drawcols <- grep("^eta\\[", colnames(draws[[1]]))
        } else {
          drawcols <- grep("^eta_b", colnames(draws[[1]]))
          nsamp <- sum(nsamp)
        }
        nfound <- length(drawcols)/nsamp

        drawcols <- drawcols[as.numeric(matrix(1:length(drawcols),
                                               nsamp, nfound,
                                               byrow=TRUE)[,1:nlv])]
      }
      draws <- lapply(draws, function(x) mcmc(x[,drawcols]))

      ## for target="stan" + missing, use @Data@Mp to reorder rows to correspond
      ## to original data
      mis <- any(is.na(unlist(blavobject@Data@X)))
      Mp <- blavobject@Data@Mp
      if(blavobject@Options$target == "stan" & mis){
        rorig <- sapply(Mp, function(x) unlist(x$case.idx))
        empties <- sapply(Mp, function(x) x$empty.idx)
        if(inherits(rorig, "list")){
          ## multiple groups
          for(ii in length(rorig)){
            rorig[[ii]] <- blavobject@Data@case.idx[[ii]][rorig[[ii]]]
          }
          rorig <- unlist(rorig)
        }
        cids <- Mp2dataidx(Mp, blavobject@Data@case.idx)

        ## reordering for lvs:
        nfit <- sum(lavInspect(blavobject, 'nobs'))
        rsamps <- rep(NA, nlv*nfit)
        for(j in 1:nlv){
          rsamps[((j-1)*nfit + 1):(j*nfit)] <- (j-1)*nfit + cids
        }

        for(j in 1:length(draws)){
          draws[[j]][,rsamps] <- draws[[j]]
        }
      }
      draws <- mcmc.list(draws)

      if((what %in% c("lvmeans", "fsmeans")) | ("means" %in% dotdotdot)){
        br <- TRUE
        if(jagtarget){
          summ <- blavobject@external$mcmcout$summaries
          summname <- "Mean"
          br <- FALSE
        } else {
          summ <- blavobject@external$stansumm
          summname <- "mean"
        }
        if(level == 1L){
          mnrows <- grep("^eta\\[", rownames(summ))
        } else {
          mnrows <- grep("^eta_b", rownames(summ))
        }

        draws <- matrix(summ[mnrows,summname], nsamp,
                        length(mnrows)/nsamp, byrow=br)[,1:nlv,drop=FALSE]
        colnames(draws) <- names(lvmn[[level]])

        if(blavobject@Options$target == "stan" & mis){
          draws[rank(rorig),] <- draws
        }
      }
      draws
    } else if(what %in% c("n.chains", "nchain", "nchains")){
      draws <- make_mcmc(blavobject@external$mcmcout)
      length(draws)
    } else if(what == "cp"){
      blavobject@Options$cp
    } else if(what == "dp"){
      blavobject@Options$dp
    } else if(what %in% c("postmode", "postmean", "postmedian")){
      if(jagtarget){
        mcmcsumm <- blavobject@external$mcmcout$summaries
      } else {
        # mcmcsumm <- rstan::summary(blavobject@external$mcmcout)$summary
        mcmcsumm <- NULL
      }

      if(what == "postmean"){
        if(jagtarget){
          OUT <- mcmcsumm[idx,'Mean']
        } else {
          OUT <- mcmcsumm[idx,'mean']
        }
      }else if(what == "postmedian"){
        if(jagtarget){
          OUT <- mcmcsumm[idx,'Median']
        } else {
          OUT <- mcmcsumm[idx,'50%']
        }
      } else {
        if(jagtarget){
          OUT <- mcmcsumm[idx,'Mode']
        } else {
          draws <- do.call("rbind", blavInspect(blavobject, "mcmc"))
          OUT <- modeapprox(draws)
        }
      }
      if(add.labels) names(OUT) <- labs
      OUT
    } else if(what == "jagnames"){
      if(!jagtarget) stop("blavaan ERROR: JAGS was not used for model estimation.")
      OUT <- blavobject@ParTable$pxnames[blavobject@ParTable$free > 0]
      OUT <- OUT[order(blavobject@ParTable$free[blavobject@ParTable$free > 0])]
      if(add.labels) names(OUT) <- labs
      OUT
    } else if(what == "stannames"){
      if(jagtarget) stop("blavaan ERROR: Stan was not used for model estimation.")
      # mcmcsumm <- rstan::summary(blavobject@external$mcmcout)$summary
      mcmcsumm <- NULL
      OUT <- rownames(mcmcsumm)[idx]
      if(add.labels) names(OUT) <- labs
      OUT
    }
  } else if(what %in% nowhats){
    stop(paste("blavaan ERROR: argument", what,
               "not available for Bayesian models."))
  } else {
    ## we can use lavInspect
    lavargs <- c(dotdotdot, list(object = blavobject, what = what))
    do.call("lavInspect", lavargs)
  }
}
