# INLAvaan parses model syntax with lavaan's old parser, which reads prior().
# That parser keeps only one modifier per term, so `prior("normal(0,1)")*a*x2`
# silently loses its prior and `0.5*a*x2` its fixed value. Writing each
# modifier as its own term (`prior("normal(0,1)")*x2 + a*x2`) keeps them all,
# so split_modifiers() rewrites chained modifiers that way. Statements are
# formed as the old parser forms them, and only right-hand sides are touched
# (of =~, <~, ~*~, ~~, ~ and |). The model comes back unchanged when nothing is
# chained.
split_modifiers <- function(model) {
  if (!is.character(model)) {
    return(model)
  }

  # Comments go first, then `;` separates statements, as in lavaan
  lines <- strsplit(paste(model, collapse = "\n"), "\n", fixed = TRUE)[[1]]
  lines <- unlist(strsplit(gsub("[#!].*$", "", lines), ";", fixed = TRUE))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0L) {
    return(model)
  }

  # A statement starts at a line with an operator (or an efa() line, which
  # takes the next line with it) and runs until the next start
  ops <- c("=~", "<~", "~*~", "~~", "~", "==", "<", ">", ":=", ":", "|", "%")
  masked <- mask_quotes(lines)
  has_op <- vapply(
    masked,
    function(l) any(vapply(ops, grepl, logical(1), l, fixed = TRUE)),
    logical(1),
    USE.NAMES = FALSE
  )
  is_efa <- grepl("efa(", masked, fixed = TRUE)
  is_start <- has_op | is_efa
  efa_only <- which(is_efa & !has_op)
  is_start[efa_only[efa_only < length(lines)] + 1L] <- FALSE
  starts <- which(is_start)
  if (length(starts) == 0L) {
    return(model)
  }
  ends <- c(starts[-1] - 1L, length(lines))
  stmts <- vapply(
    seq_along(starts),
    function(k) paste(lines[starts[k]:ends[k]], collapse = " "),
    character(1)
  )

  changed <- FALSE
  for (k in seq_along(stmts)) {
    masked <- mask_quotes(stmts[k])
    op <- ops[vapply(ops, grepl, logical(1), masked, fixed = TRUE)][1]
    if (!op %in% c("=~", "<~", "~*~", "~~", "~", "|")) {
      next
    }
    pos <- regexpr(op, masked, fixed = TRUE)
    lhs <- trimws(substr(stmts[k], 1L, pos - 1L))
    rhs <- substr(stmts[k], pos + nchar(op), nchar(stmts[k]))
    terms <- trimws(split_top_level(rhs, "+"))
    new_terms <- vapply(terms, split_term, character(1), USE.NAMES = FALSE)
    if (!identical(new_terms, terms)) {
      changed <- TRUE
      stmts[k] <- paste(lhs, op, paste(new_terms, collapse = " + "))
    }
  }
  if (!changed) {
    return(model)
  }
  paste(c(lines[seq_len(starts[1] - 1L)], stmts), collapse = "\n")
}

# One term with chained modifiers as separate terms, e.g. "a*0.5?x" becomes
# "a*x + start(0.5)*x". A term with at most one modifier is returned as is.
split_term <- function(term) {
  parts <- trimws(split_top_level(term, "*"))
  parts <- unlist(lapply(parts, function(p) {
    q <- trimws(split_top_level(p, "?"))
    if (length(q) == 2L) c(paste0("start(", q[1], ")"), q[2]) else p
  }))
  n <- length(parts)
  if (n < 3L) {
    return(term)
  }
  paste0(parts[-n], "*", parts[n], collapse = " + ")
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
  x <- unname(x)
  chars <- strsplit(mask_quotes(x), "")[[1]]
  depth <- cumsum(chars == "(") - cumsum(chars == ")")
  at <- which(chars == sep & depth == 0L)
  substring(x, c(1L, at + 1L), c(at - 1L, length(chars)))
}
