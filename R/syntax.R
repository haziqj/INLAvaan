# INLAvaan parses model syntax with lavaan's old parser, which reads prior().
# That parser keeps only one modifier per term, so `prior("normal(0,1)")*a*x2`
# silently loses its prior and `0.5*a*x2` its fixed value. Writing each
# modifier as its own term (`prior("normal(0,1)")*x2 + a*x2`) keeps them all,
# so split_modifiers() rewrites chained modifiers that way. Statements are
# formed as the old parser forms them, and only the right-hand side of =~, ~,
# ~~ and <~ is touched. The model comes back unchanged when nothing is
# chained.
split_modifiers <- function(model) {
  if (!is.character(model)) {
    return(model)
  }
  text <- paste(model, collapse = "\n")
  lines <- strsplit(gsub(";", "\n", text, fixed = TRUE), "\n")[[1]]
  lines <- trimws(gsub("[#!].*$", "", lines))
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0L || any(grepl("efa(", lines, fixed = TRUE))) {
    return(model)
  }

  # A statement starts at a line with an operator and runs until the next one
  ops <- c("=~", "<~", "~*~", "~~", "~", "==", "<", ">", ":=", ":", "|", "%")
  has_op <- vapply(
    mask_quotes(lines),
    function(l) any(vapply(ops, grepl, logical(1), l, fixed = TRUE)),
    logical(1)
  )
  if (!any(has_op)) {
    return(model)
  }
  starts <- which(has_op)
  ends <- c(starts[-1] - 1L, length(lines))
  stmts <- mapply(
    function(s, e) paste(lines[s:e], collapse = " "),
    starts,
    ends
  )

  changed <- FALSE
  for (k in seq_along(stmts)) {
    masked <- mask_quotes(stmts[k])
    op <- ops[vapply(ops, grepl, logical(1), masked, fixed = TRUE)][1]
    if (!op %in% c("=~", "~", "~~", "<~")) {
      next
    }
    pos <- regexpr(op, masked, fixed = TRUE)
    lhs <- substr(stmts[k], 1L, pos - 1L)
    rhs <- substr(stmts[k], pos + nchar(op), nchar(stmts[k]))
    terms <- split_top_level(rhs, "+")
    new_terms <- vapply(
      terms,
      function(term) {
        parts <- trimws(split_top_level(term, "*"))
        n <- length(parts)
        if (n < 3L) {
          return(trimws(term))
        }
        paste0(parts[-n], "*", parts[n], collapse = " + ")
      },
      character(1)
    )
    if (!identical(unname(new_terms), trimws(terms))) {
      changed <- TRUE
      stmts[k] <- paste0(
        trimws(lhs),
        " ",
        op,
        " ",
        paste(new_terms, collapse = " + ")
      )
    }
  }
  if (!changed) {
    return(model)
  }
  paste(c(lines[seq_len(starts[1] - 1L)], stmts), collapse = "\n")
}

# Replace each character inside quotes with "_", keeping the positions
mask_quotes <- function(x) {
  vapply(
    x,
    function(s) {
      chars <- strsplit(s, "")[[1]]
      quote <- ""
      for (k in seq_along(chars)) {
        ch <- chars[k]
        if (nzchar(quote)) {
          if (ch == quote) {
            quote <- ""
          } else {
            chars[k] <- "_"
          }
        } else if (ch %in% c("\"", "'")) {
          quote <- ch
        }
      }
      paste(chars, collapse = "")
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# Split x at each sep that is outside quotes and parentheses
split_top_level <- function(x, sep) {
  chars <- strsplit(mask_quotes(x), "")[[1]]
  depth <- cumsum(chars == "(") - cumsum(chars == ")")
  at <- which(chars == sep & depth == 0L)
  starts <- c(1L, at + 1L)
  ends <- c(at - 1L, length(chars))
  substring(x, starts, ends)
}
