#' @export
print.inlavaan <- function(x, ...) {
  cat("Hello, World!")
}

after_inlavaan <- function(x) {
  list2env(x, envir = environment())

  PT$mode <-
    PT$`0.975quant` <-
    PT$`0.5quant` <-
    PT$`0.025quant` <-
    PT$sd <-
    PT$mean <-
    PT$ustart
  PT$sd <- NA
  res_names <- c("mean", "sd", "0.025quant", "0.5quant", "0.975quant", "mode")

  # Add intercepts
  PT[PT$mat == "nu", res_names] <- fit$summary.fixed[, res_names]

  # Add loadings and B coefficients
  betas <-
    fit$summary.hyperpar |>
    rownames_to_column("name") |>
    filter(grepl("Beta", name)) |>
    mutate(idx_name = gsub("Beta for ", "", name)) |>
    left_join(select(IT, id, idx_name, mat), by = "idx_name")

  PT[PT$mat == "lambda" & PT$free > 0, res_names] <-
    betas[betas$mat == "lambda", res_names]
  PT[PT$mat == "beta" & PT$free > 0, res_names] <-
    betas[betas$mat == "beta", res_names]

  # Retrieve back the Theta and Psi matrices
  # Theta = LambdaD %*% PsiD %*% t(LambdaD) + ThetaStar


  PT |> select(id:rhs, mat, all_of(res_names)) |> print(n=100)

}
