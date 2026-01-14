testthat::skip()
test_that("All exported functions have @return and @examples", {
  # --- 1. Get Exported Functions (Text Parsing) ---
  if (!file.exists("NAMESPACE")) {
    stop("No NAMESPACE file found. Are you in the package root?")
  }
  ns_lines <- readLines("NAMESPACE")
  export_lines <- grep("^export\\(", ns_lines, value = TRUE)
  exported_funcs <- gsub("^export\\(|\\)$", "", export_lines)
  exported_funcs <- unlist(strsplit(exported_funcs, ","))
  exported_funcs <- gsub('"', '', trimws(exported_funcs))
  exported_funcs <- exported_funcs[exported_funcs != ""] # Remove empties

  cli::cli_alert_info("Found {length(exported_funcs)} exported functions.")

  # --- 2. Check Documentation ---
  man_files <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)

  results <- data.frame(
    Function = character(),
    Has_Return = logical(),
    Has_Examples = logical(),
    stringsAsFactors = FALSE
  )

  for (func in exported_funcs) {
    # Find the .Rd file for this function
    rd_file <- NA
    for (f in man_files) {
      rd_content <- tryCatch(tools::parse_Rd(f), error = function(e) NULL)
      if (is.null(rd_content)) {
        next
      }

      # Extract aliases manually
      aliases <- unlist(lapply(rd_content, function(x) {
        if (!is.null(attr(x, "Rd_tag")) && attr(x, "Rd_tag") == "\\alias") {
          return(as.character(x))
        }
      }))

      if (func %in% aliases) {
        rd_file <- f
        break
      }
    }

    if (is.na(rd_file)) {
      results <- rbind(
        results,
        data.frame(Function = func, Has_Return = FALSE, Has_Examples = FALSE)
      )
      next
    }

    # --- FIX: Extract tags manually using attributes ---
    rd_data <- tools::parse_Rd(rd_file)
    # Get the tag for every top-level element in the Rd file
    tags <- sapply(rd_data, attr, "Rd_tag")

    has_return <- "\\value" %in% tags
    has_examples <- "\\examples" %in% tags

    results <- rbind(
      results,
      data.frame(
        Function = func,
        Has_Return = has_return,
        Has_Examples = has_examples
      )
    )
  }

  # --- 3. Report ---
  missing_docs <- results[!results$Has_Return | !results$Has_Examples, ]
  if (nrow(missing_docs) > 0) {
    cli::cli_h1("Documentation Check")
    cli::cli_alert_warning(
      "Found {nrow(missing_docs)} functions with missing tags:"
    )

    cli::cli_ul() # Start a list

    for (i in 1:nrow(missing_docs)) {
      row <- missing_docs[i, ]

      # Identify exactly which parts are missing
      missing_parts <- character()
      if (!row$Has_Return) {
        missing_parts <- c(missing_parts, "@return")
      }
      if (!row$Has_Examples) {
        missing_parts <- c(missing_parts, "@examples")
      }

      # Format the specific line
      # {.fn name} formats it as a function code style
      # {.val name} formats the missing tags nicely
      cli::cli_li(
        "Function {.fn {row$Function}} is missing: {.val {missing_parts}}"
      )
    }

    cli::cli_end() # End the list
  } else {
    cli::cli_alert_success(
      "All exported functions have {.strong @return} and {.strong @examples}!"
    )
  }

  expect_true(
    nrow(missing_docs) == 0,
    info = paste(
      "Some exported functions are missing @return or @examples tags. See details below."
    )
  )
})
