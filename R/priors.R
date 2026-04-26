#' Density of a Beta distribution on a bounded interval
#'
#' @inheritParams stats::dbeta
#' @param x A numeric vector of quantiles.
#' @param a The lower bound of the interval.
#' @param b The upper bound of the interval.
#' @param log Logical; if TRUE, probabilities p are given as log(p).
#'
#' @returns A numeric vector of density values.
#' @export
#'
#' @examples
#' # Beta(2,5) on (0,100)
#' x <- seq(0, 100, length.out = 100)
#' y <- dbeta_box(x, shape1 = 2, shape2 = 5, a = 0, b = 100)
#' plot(x, y, type = "l", main = "Beta(2,5) on (0,100)")
#'
#' # Beta(1,1) i.e. uniform on (-1, 1)
#' x <- seq(-1, 1, length.out = 100)
#' y <- dbeta_box(x, shape1 = 1, shape2 = 1, a = -1, b = 1)
#' plot(x, y, type = "l", main = "Beta(1,1) on (-1,1)")
dbeta_box <- function(x, shape1, shape2, a, b, log = FALSE) {
  # basic checks
  if (
    !is.numeric(a) ||
      !is.numeric(b) ||
      length(a) != 1 ||
      length(b) != 1 ||
      !is.finite(a) ||
      !is.finite(b) ||
      b <= a
  ) {
    stop("Require finite scalars with b > a.")
  }

  # transform to (0,1)
  u <- (x - a) / (b - a)
  inside <- (x >= a) & (x <= b)

  # init output
  out <- if (log) rep(-Inf, length(x)) else rep(0, length(x))

  # evaluate where inside support
  if (any(inside, na.rm = TRUE)) {
    if (log) {
      out[inside] <- -log(b - a) + dbeta(u[inside], shape1, shape2, log = TRUE)
    } else {
      out[inside] <- dbeta(u[inside], shape1, shape2) / (b - a)
    }
  }

  # propagate NAs from x
  out[is.na(x)] <- NA_real_
  out
}

split_top_level_tokens <- function(x, delim) {
  if (!nzchar(x)) return(character(0))

  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  out <- character(0)
  start <- 1L
  depth <- 0L
  quote <- ""
  i <- 1L

  while (i <= length(chars)) {
    ch <- chars[[i]]
    if (nzchar(quote)) {
      if (ch == quote) quote <- ""
    } else if (ch %in% c("'", "\"")) {
      quote <- ch
    } else if (ch == "(") {
      depth <- depth + 1L
    } else if (ch == ")") {
      depth <- max(0L, depth - 1L)
    } else if (depth == 0L && ch == delim) {
      out <- c(out, trimws(substr(x, start, i - 1L)))
      start <- i + 1L
    }
    i <- i + 1L
  }

  c(out, trimws(substr(x, start, nchar(x))))
}

locate_prior_call <- function(term) {
  m <- regexpr("prior\\s*\\(", term, perl = TRUE)
  if (m[[1L]] == -1L) return(NULL)

  start <- as.integer(m[[1L]])
  open <- start + attr(m, "match.length") - 1L
  chars <- strsplit(term, "", fixed = TRUE)[[1]]
  depth <- 1L
  quote <- ""
  i <- open + 1L

  while (i <= length(chars) && depth > 0L) {
    ch <- chars[[i]]
    if (nzchar(quote)) {
      if (ch == quote) quote <- ""
    } else if (ch %in% c("'", "\"")) {
      quote <- ch
    } else if (ch == "(") {
      depth <- depth + 1L
    } else if (ch == ")") {
      depth <- depth - 1L
    }
    i <- i + 1L
  }

  if (depth != 0L) {
    cli::cli_abort("Malformed inline prior specification: unmatched parentheses.")
  }

  list(start = start, end = i - 1L)
}

strip_wrapping_quotes <- function(x) {
  x <- trimws(x)
  if (nchar(x) >= 2L) {
    q1 <- substr(x, 1L, 1L)
    q2 <- substr(x, nchar(x), nchar(x))
    if (q1 %in% c("'", "\"") && identical(q1, q2)) {
      return(substr(x, 2L, nchar(x) - 1L))
    }
  }
  x
}

extract_inline_prior_from_term <- function(term) {
  prior <- NULL
  out <- term

  repeat {
    loc <- locate_prior_call(out)
    if (is.null(loc)) break

    content <- substr(out, loc$start, loc$end)
    inner <- sub("^prior\\s*\\(", "", content)
    inner <- sub("\\)$", "", inner)
    prior <- strip_wrapping_quotes(inner)

    prefix <- substr(out, 1L, loc$start - 1L)
    suffix <- substr(out, loc$end + 1L, nchar(out))
    suffix <- sub("^\\s*\\*\\s*", "", suffix, perl = TRUE)
    out <- paste0(prefix, suffix)
  }

  list(term = trimws(out), prior = prior)
}

extract_inline_priors <- function(model) {
  if (!is.character(model) || length(model) != 1L || !nzchar(model)) {
    return(list(model = model, priors = NULL))
  }

  lines <- strsplit(model, "\n", fixed = TRUE)[[1]]
  cleaned <- lines
  priors <- vector("list", 0L)

  for (i in seq_along(lines)) {
    line <- lines[[i]]
    comment_pos <- regexpr("#", line, fixed = TRUE)[[1L]]
    code <- if (comment_pos > 0L) substr(line, 1L, comment_pos - 1L) else line
    if (!nzchar(trimws(code))) next

    op_match <- regexpr("(=~|~~|~)", code, perl = TRUE)
    if (op_match[[1L]] == -1L) next

    op <- regmatches(code, op_match)
    lhs <- trimws(substr(code, 1L, op_match[[1L]] - 1L))
    rhs <- trimws(substr(
      code,
      op_match[[1L]] + attr(op_match, "match.length"),
      nchar(code)
    ))
    if (!nzchar(lhs) || !nzchar(rhs)) next

    rhs_terms <- split_top_level_tokens(rhs, "+")
    if (length(rhs_terms) == 0L) next

    changed <- FALSE
    clean_terms <- rhs_terms
    for (j in seq_along(rhs_terms)) {
      res <- extract_inline_prior_from_term(rhs_terms[[j]])
      clean_terms[[j]] <- res$term
      if (!is.null(res$prior)) {
        changed <- TRUE
        factors <- split_top_level_tokens(res$term, "*")
        target <- trimws(factors[[length(factors)]])
        prior_op <- if (op == "~" && identical(target, "1")) "~1" else op
        prior_rhs <- if (prior_op == "~1") "" else target
        priors[[length(priors) + 1L]] <- list(
          lhs = lhs,
          op = prior_op,
          rhs = prior_rhs,
          prior = trimws(res$prior),
          line = i,
          term = j
        )
      }
    }

    if (changed) {
      rebuilt <- paste(clean_terms, collapse = " + ")
      suffix <- if (comment_pos > 0L) substr(line, comment_pos, nchar(line)) else ""
      indent <- sub("^([[:space:]]*).*$", "\\1", line, perl = TRUE)
      cleaned[[i]] <- paste0(indent, lhs, " ", op, " ", rebuilt, suffix)
    }
  }

  priors_df <- if (length(priors) > 0L) {
    as.data.frame(do.call(rbind, lapply(priors, as.data.frame)), stringsAsFactors = FALSE)
  } else {
    NULL
  }

  list(model = paste(cleaned, collapse = "\n"), priors = priors_df)
}

apply_inline_priors_to_partable <- function(pt, inline_priors = NULL) {
  if (is.null(inline_priors) || NROW(inline_priors) == 0L) {
    return(pt)
  }

  for (i in seq_len(nrow(inline_priors))) {
    row <- inline_priors[i, , drop = FALSE]
    hit <- which(
      pt$lhs == row$lhs &
        pt$op == row$op &
        pt$rhs == row$rhs
    )
    if (length(hit) == 0L) {
      cli::cli_warn(c(
        "Could not attach inline prior to any parameter-table row.",
        "i" = paste0(row$lhs, " ", row$op, " ", ifelse(nzchar(row$rhs), row$rhs, "1"))
      ))
      next
    }
    pt$prior[hit] <- row$prior
  }

  pt
}
