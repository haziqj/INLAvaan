mat <- matrix(c(" 2*a", "-3*b", "c *3", "d * 1  "), 2, 2)

extract_coef <- function(expr) {
  coeff <- as.numeric(sub("[^0-9-]+.*$", "", expr))
  if (length(coeff) == 0 || is.na(coeff)) {
    return(1)
  } else {
    return(coeff)
  }
}

extract_var <- function(expr) {
  var <- gsub("[-0-9]*", "", expr)
  if (length(var) == 0) {
    return("")
  } else {
    return(gsub(" ", "", gsub("\\*", "", var)))
  }
}

list(
  matrix(sapply(mat, extract_coef), nrow = 2),
  matrix(sapply(mat, extract_var), nrow = 2)
)


