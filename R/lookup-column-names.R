#' Lookup new column names for an accident dataset
#'
#' This function retrieves the new column names corresponding to
#' the original column names in a specified dataset. The column
#' names are based on the configuration loaded by `load_config()`.
#' This is useful when the original names are in Japanese (2-byte
#' characters), which can be difficult to handle in programs.
#' This function converts them to corresponding English names.
#'
#' @param original_names A vector of original column names.
#' @return A vector of new column names corresponding to the provided
#'   original names, or `NA` for unmatched names.
#' @export
lookup_column_names <- function(original_names) {
  # Load column name configuration
  config <- get_config()

  # Helper function to find a matching column name
  find_column_name <- function(name) {
    matches <- vapply(
      config$columns,
      function(column) name %in% column,
      logical(1)
    )

    if (any(matches)) {
      names(config$columns)[matches]
    } else {
      NA_character_
    }
  }

  # Apply the helper function to all provided original names
  new_names <- vapply(original_names, find_column_name, character(1))

  return(new_names)
}
