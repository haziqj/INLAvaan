# Parallel evaluation through the future package, which is optional. There are
# two switches: the `cores` argument of inla_vanilla(), which sets up workers
# for one fit and restores the user's plan afterwards, and the user's own
# future::plan(), which applies when `cores` is NULL. With neither, nothing here
# runs and the fit is a plain loop. Whatever the switch, a fit gives the numbers
# of a serial fit. The work divides into items that do not depend on each other,
# and anything that goes wrong on a worker is done again in the parent, so
# parallelism only ever saves time.
#
# The items are spread over the jobs at the start, at most one job per worker,
# and each job runs to its end. A job signals the parent after each item, so
# the parent's progress line moves one item at a time, as in a serial fit. See
# lab/DESIGN.md.

# Below this estimate of the serial time, in seconds, the work is not worth
# sending to workers: a job costs tens of milliseconds at least, and on the
# fits tried the gain was a few hundred milliseconds at best.
par_min_s <- 1

# Above this estimate, with neither `cores` nor a plan set, an interactive fit
# says once per session that more cores would help.
par_hint_s <- 10

# What is kept for the session
the <- new.env(parent = emptyenv())
the$hinted <- FALSE

# Functions of our own, so that the tests can replace them. The core count and
# the fork check come from parallelly, which future imports, so it is there
# whenever future is. future's copies of these functions are deprecated.
is_interactive <- function() interactive()
par_has_future <- function() {
  requireNamespace("future", quietly = TRUE) &&
    requireNamespace("parallelly", quietly = TRUE)
}
par_available <- function() parallelly::availableCores()

# The program running R, in lower case: "ark" in Positron's console
par_program <- function() tolower(basename(commandArgs(FALSE)[1L]))

# Forked workers share everything, compiled code included, so they are the
# choice where they are safe. parallelly's check knows that forking is not
# available on Windows and unstable in RStudio, but says yes in Positron, whose
# ark kernel refuses to fork, with an error. The program is checked and not the
# POSITRON variable, which can reach Positron's integrated terminals, where a
# plain R forks well.
par_can_fork <- function() {
  parallelly::supportsMulticore() && !identical(par_program(), "ark")
}

# Put a plan for `cores` workers in place for one fit, with never more workers
# than the machine offers. Returns a function that puts the old plan back.
par_local_plan <- function(cores) {
  workers <- min(cores, par_available())
  old <- if (par_can_fork()) {
    future::plan(future::multicore, workers = workers)
  } else {
    future::plan(future::multisession, workers = workers)
  }
  function() future::plan(old)
}

# The workers the user's plan gives. Without the future namespace loaded nobody
# has set a plan, so it is not loaded here, which would cost a whole small fit.
par_workers <- function() {
  if (!isNamespaceLoaded("future")) {
    return(1L)
  }
  n <- tryCatch(future::nbrOfWorkers(), error = function(e) 1L)
  if (is.finite(n)) as.integer(n) else .Machine$integer.max
}

# What the items need besides their arguments: anything the user's functions
# use from the global environment, and the packages they use. `functions`
# holds the user's functions, NULL for those not given. future finds these by
# reading the functions, once, because reading them again for each job is slow.
# The workers also load the package that holds this code, for the jobs, by the
# name packageName() gives, so a package that copies the engine files loads
# itself.
par_globals <- function(functions) {
  functions <- functions[!vapply(functions, is.null, NA)]
  names(functions) <- paste0(".par_fn", seq_along(functions))
  expr <- as.call(c(list(as.name("list")), lapply(names(functions), as.name)))
  gp <- future::getGlobalsAndPackages(
    expr,
    envir = list2env(functions, parent = globalenv())
  )
  found <- setdiff(
    names(gp$globals),
    c(names(functions), "...", "future.call.arguments", "list")
  )
  list(
    found = stats::setNames(lapply(found, function(k) gp$globals[[k]]), found),
    packages = unique(c(gp$packages, utils::packageName()))
  )
}

# One job as a future: `job(ids, data)` on a worker. The function and its data
# travel as one object each, so an environment they share is sent once. The
# names are ours so that they cannot mask a global the user's function needs.
par_future <- function(job, ids, data, setup) {
  future::future(
    quote(.par_job(.par_ids, .par_data)),
    substitute = FALSE,
    globals = c(
      setup$found,
      list(.par_job = job, .par_ids = ids, .par_data = data)
    ),
    packages = setup$packages,
    seed = NULL
  )
}

# Decide how to run `n` items of about `est_s` serial seconds in all. `probe`
# is run with `data` on a worker and here, and the two values must agree: the
# user's function may not survive the trip to a separate R session, and may
# then fail or, worse, return NaN with no error (compiled code does, depending
# on how it was built). `label`, `what` and `how` word the messages. `cores` is
# the fit's argument: NULL follows the user's plan, and 1 stays serial whatever
# the plan. Returns `jobs`, 1 to stay serial, `restore`, which puts the user's
# plan back and is for the caller to run when done, and what par_future() needs.
#
# The time to hand the probe to a worker is the real cost of reaching one, with
# the real data. Jobs are handed over one at a time, but what a worker then
# does, such as building a compiled model again, happens alongside the others.
# So the time with w jobs is about est_s / w + cost * w, which is least at
# w = sqrt(est_s / cost), and a long scan uses every worker of the plan and a
# shorter one fewer.
par_setup <- function(
  functions,
  probe,
  data,
  est_s,
  n,
  note,
  label,
  what,
  cores = NULL,
  how = "set the `cores` argument"
) {
  serial <- list(jobs = 1L, restore = function() NULL)
  if (identical(cores, 1L)) {
    return(serial)
  }
  if (is.null(cores) && par_workers() < 2L) {
    par_hint(est_s, note, label, how)
    return(serial)
  }
  if (est_s < par_min_s || n < 2L) {
    return(serial)
  }
  if (!is.null(cores) && !par_has_future()) {
    note(paste0(
      label,
      " will run serially, because `cores` needs the future package: ",
      "install.packages(\"future\")."
    ))
    return(serial)
  }
  restore <- if (is.null(cores)) {
    function() NULL
  } else {
    tryCatch(par_local_plan(cores), error = function(e) NULL)
  }
  if (is.null(restore)) {
    note(paste0(label, " will run serially: the workers could not be started."))
    return(serial)
  }
  workers <- par_workers()
  if (workers < 2L) {
    restore()
    return(serial)
  }
  # Only the values matter here. Anything the function says while being
  # checked it has said already, or will say again in the real run. The
  # check gives NULL when the worker agrees, and otherwise what went wrong.
  problem <- suppressMessages(suppressWarnings(tryCatch(
    {
      setup <- par_globals(functions)
      t0 <- elapsed()
      fut <- par_future(probe, 0L, data, setup)
      cost <- elapsed() - t0
      got <- future::value(fut)
      same <- isTRUE(all.equal(
        got,
        probe(0L, data),
        tolerance = 1e-6,
        check.attributes = FALSE
      ))
      if (same) NULL else "did not give the same value on a parallel worker"
    },
    error = function(e) {
      paste0(
        "failed on a parallel worker (",
        gsub("\\s+", " ", trimws(conditionMessage(e))),
        ")"
      )
    }
  )))
  if (!is.null(problem)) {
    restore()
    note(paste0(
      label,
      " will run serially, because `",
      what,
      "` ",
      problem,
      ". Compiled code ",
      "(Rcpp, TMB, RTMB) cannot be sent to a separate R session: rebuild it on ",
      "each worker, or use forked workers from a terminal on macOS or Linux ",
      "(see Parallel computation in ?inla_vanilla)."
    ))
    return(serial)
  }
  jobs <- min(workers, n, ceiling(sqrt(est_s / max(cost, 1e-3))))
  if (jobs < 2L) {
    restore()
    return(serial)
  }
  c(list(jobs = as.integer(jobs), restore = restore), setup)
}

# Say once per session, at the console, that more cores would help. `note`
# returns whether the message was shown, so a silent fit does not use the hint
# up.
par_hint <- function(est_s, note, label, how) {
  if (!the$hinted && est_s >= par_hint_s && is_interactive()) {
    the$hinted <- isTRUE(note(paste0(
      label,
      " should take about ",
      format_clock(est_s),
      ". To use more cores, ",
      how,
      "."
    )))
  }
}

# Signal the parent from a worker that an item is done. future relays an
# immediate condition while the parent polls, and not only at the end. Some
# backends relay it again with the job's result, so the parent counts each item
# once.
par_tick <- function(item) {
  signalCondition(structure(
    class = c("inlavanilla_tick", "immediateCondition", "condition"),
    list(message = "", call = NULL, item = item)
  ))
  invisible()
}

# The items split evenly over the jobs, in turn, so that items that are alike
# and next to each other, as parameters often are, do not land on one worker.
par_groups <- function(n, jobs) {
  unname(split(seq_len(n), rep_len(seq_len(jobs), n)))
}

# Run one job per group and wait for all of them, calling `on_item` as items
# finish. Returns a list with the value of each job, or NULL for one that
# failed, for any reason, which the caller then does itself. Warnings raised on
# the workers are signalled here by value().
par_run <- function(groups, job, data, setup, on_item) {
  withCallingHandlers(
    {
      running <- lapply(groups, function(ids) {
        tryCatch(par_future(job, ids, data, setup), error = function(e) NULL)
      })
      started <- running[!vapply(running, is.null, NA)]
      tryCatch(
        while (!all(future::resolved(started))) {
          Sys.sleep(0.02)
        },
        error = function(e) NULL
      )
      lapply(running, function(f) {
        if (is.null(f)) {
          return(NULL)
        }
        tryCatch(future::value(f), error = function(e) NULL)
      })
    },
    inlavanilla_tick = function(cond) on_item(cond$item)
  )
}
