sample_covariances <- function(theta, Sigma_theta, pt, nsamp = 1000) {

  pt_cov_rows <- grep("cov", pt$mat)
  pt_cov_free_rows <- pt_cov_rows[pt$free[pt_cov_rows] > 0]
  idxcov <- pt$free[pt_cov_free_rows]

  theta_samp <- mvtnorm::rmvnorm(nsamp, mean = theta, sigma = Sigma_theta)
  x_samp <- apply(theta_samp, 1, pars_to_x, pt = pt)

  cov_samp <- x_samp[idxcov, , drop = FALSE]
  rownames(cov_samp) <- pt$names[pt_cov_free_rows]

  # RES
  apply(cov_samp, 1, function(y) {
    Ex <- mean(y)
    SDx <- sd(y)
    qq <- quantile(y, probs = c(0.025, 0.5, 0.975))
    dens <- density(y)
    xmax <- dens$x[which.max(dens$y)]
    res <- c(Ex, SDx, qq, xmax)
    names(res) <- c("Mean", "SD", "2.5%", "50%", "97.5%", "Mode")

    list(
      summary = res,
      pdf_data = data.frame(x = dens$x, y = dens$y)
    )
  })

}
