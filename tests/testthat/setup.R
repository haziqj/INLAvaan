if (is.na(parallel::detectCores())) {
  parallel_ns <- asNamespace("parallel")
  unlockBinding("detectCores", parallel_ns)
  assign("detectCores", function(all.tests = FALSE, logical = TRUE) 2L, envir = parallel_ns)
  lockBinding("detectCores", parallel_ns)
}
