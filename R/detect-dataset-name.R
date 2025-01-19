#' Identify dataset names from file paths
#'
#' This function identifies dataset names based on the file paths and
#' configuration patterns.
#'
#' Invalid files are replaced with `NA`, and a warning is issued.
#'
#' @param files A character string specifying the file paths.
#' @param quiet A logical indicating whether to suppress warnings.
#' @return A character vector of dataset names
#' @export
#' @examples
#' detect_dataset_name(c("example/honhyo_2022.csv", "example/honhyo_2023.csv"))
detect_dataset_name <- function(files, quiet = FALSE) {
  # Get configuration
  config <- get_config()

  # Determine dataset name based on file pattern
  patterns <- sapply(config$file_types, `[[`, "pattern")
  file_type_names <- names(config$file_types)

  # Helper function: Match file path to a pattern
  detect_file_type <- function(file) {
    matches <- sapply(patterns, grepl, logical(1), x = file)
    if (any(matches)) {
      return(file_type_names[which(matches)[1]]) # Return the first match
    }
    NA_character_
  }

  # Apply detection logic to all files
  dataset_names <- vapply(
    files,
    detect_file_type,
    character(1),
    USE.NAMES = FALSE
  )

  # Warn about files with no matching dataset
  invalid_files <- files[is.na(dataset_names)]
  if (0 < length(invalid_files) && !quiet) {
    cli_alert_warning("Unknown dataset for file(s): {invalid_files}. These have been replaced with NA")
  }

  return(dataset_names)
}
