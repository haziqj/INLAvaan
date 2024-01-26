#' @export
print.inlavaan <- function(x, ...) {
  y <- after_inlavaan(x)

  cli::cli_h2(cli::col_blue("Latent variables"))
  cat(make_table(y, "loadings"))
  cat("\n\n")

  cli::cli_h2("Covariances")

  cli::cli_h2("Regressions")

  cli::cli_h2("Intercepts")

  cli::cli_h2(cli::col_blue("Variances"))
  cat(make_table(y, "variances"))

}

make_table <- function(y = after_inlavaan(fit), what = "variances") {
  if (what == "loadings") {
    y <- y |> filter(mat == "lambda")
  }
  if (what == "variances") {
    y1 <- y |>
      filter(mat %in% "theta", lhs == rhs) |>
      mutate(lhs = "", op = "", rhs = paste0(".", rhs))
    y2 <- y |>
      filter(mat %in% "psi", lhs == rhs) |>
      mutate(lhs = "", op = "", rhs = paste0(" ", rhs))
    y <- bind_rows(y1, y2)
  }

  print_loadings <-
    y |>
    select(lhs, op, rhs, "Post. Mean" = mean, "Post. SD" = sd,
           `2.5% CI` = `0.025quant`,
           `97.5% CI` =  `0.975quant`) |>
    group_by(lhs) |>
    mutate(
      lhs = ifelse(row_number() == 1, lhs , "")
    ) |>
    rename(" " = lhs, "  " = op, "   " = rhs)

  tmp <- kableExtra::kbl(print_loadings, "rst", digits = 3)
  tmp[2] <- gsub("\\\\", "", tmp[2])
  tmp <- gsub(" NA", "   ", tmp)
  paste(as.character(tmp), collapse = "\n")
}

after_inlavaan <- function(x) {
  list2env(x, envir = environment())

  PT$mode <-
    PT$`0.975quant` <-
    PT$`0.5quant` <-
    PT$`0.025quant` <-
    PT$sd <- NA
  PT$mean <- PT$ustart
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
  bs <- betas[betas$mat == "lambdaD", ]
  rowb <-
    PT |>
    filter(op == "~~", lhs != rhs) |>
    select(row, col) |>
    as.matrix() |>
    t() |>
    c()
  ncorr <- length(rowb) / 2
  colb <- rep(seq_len(ncorr), each = 2)
  lambdad <- c(rbind(rep(1, ncorr), bs$mean))
  LambdaD <- Matrix::sparseMatrix(i = rowb, j = colb, x = lambdad,
                                  dims = c(p, ncorr))

  # In INLA, the precisions are reported. We need to transform them to variances
  where_logprec <- grep("Log precision for",
                        names(fit$internal.marginals.hyperpar))

  suppressMessages({
    vars <-
      map(fit$internal.marginals.hyperpar[where_logprec], \(m) {
        m.var <- INLA::inla.tmarginal(function(x) 1 / exp(x), m)
        x <- INLA::inla.zmarginal(m.var, silent = TRUE)
        x
      }) |>
      bind_rows() |>
    mutate(name = gsub("Log precision for ", "",
                       names(fit$internal.marginals.hyperpar[where_logprec])),
           mode = NA)
  })

  ThetaStar <- Matrix::sparseMatrix(i = 1:p, j = 1:p, x = vars$mean[1:p])
  PsiD <- Matrix::sparseMatrix(i = 1:ncorr, j = 1:ncorr,
                               x = vars$mean[grep("d[0-9]+", vars$name)])
  Theta <- LambdaD %*% PsiD %*% Matrix::t(LambdaD) + ThetaStar
  theta_cov <- Theta[lower.tri(Theta)]
  theta_cov <- theta_cov[theta_cov != 0]
  theta_var <- diag(as.matrix(Theta))

  PT[PT$mat == "theta" & PT$lhs == PT$rhs, "mean"] <- theta_var
  PT[PT$mat == "theta" & PT$lhs != PT$rhs, "mean"] <- theta_cov

  psis <-
    vars |>
    slice(-(1:p)) |>
    filter(!grepl("d[0-9]+", name))
  colnames(psis) <- c("mean", "sd", "0.025quant", "0.5quant", "0.975quant",
                      "name", "mode")
  PT[PT$mat == "psi" & PT$free > 0 & PT$lhs == PT$rhs, res_names] <-
    psis[, res_names]
  # vars
  # list(vars)

  # PT |> select(id:rhs, mat, all_of(res_names)) |> print(n=100)
  return(PT)

}
