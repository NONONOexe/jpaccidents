#' Get configuration
#'
#' This function retrieves the configuration that were loaded using
#' `load_config()`.
#'
#' @return A list containing the configuration.
#' @seealso [load_config()] for loading the configuration.
#' @seealso [reset_config()] for resetting the configuration.
#' @export
#' @examples
#' get_config()
get_config <- function() {
  config <- getOption("jpaccidents.config")
  if (is.null(config)) {
    stop("Configuration not loaded. Call `load_config()` first.")
  }

  return(config)
}

#' Load configuration from YAML file
#'
#' This function loads the accident data configuration from a YAML file.
#'
#' @param  config_path Path to the YAML file containing the configuration.
#' @seealso [get_config()] for retrieving the configuration.
#' @seealso [reset_config()] for resetting the configuration.
#'   to the default values.
#' @export
#' @examples
#' \dontrun{
#' load_config("path/to/config.yaml")
#' }
load_config <- function(config_path) {
  if (!file.exists(config_path)) {
    stop("Configuration file not found.")
  }
  config <- read_yaml(config_path)
  attr(config, "path") <- config_path
  class(config) <- "jpaccidents_config"
  options(jpaccidents.config = config)
}

#' Reset Configuration
#'
#' This function resets the configuration to the default values.
#'
#' @seealso [get_config()] for retrieving the configuration.
#' @seealso [load_config()] for loading the configuration.
#' @export
#' @examples
#' reset_config()
reset_config <- function() {
  options(jpaccidents.config = default_config)
}

#' @export
print.jpaccidents_config <- function(x, ...) {
  cat("Accident Data Configuration\n")
  cat("---------------------------\n")

  # File Types
  cat("File types:\n")
  for (name in names(x$file_types)) {
    info <- x$file_types[[name]]
    pattern <- info$pattern
    column_count <- length(info$columns)
    cat(sprintf("  - %s: %s (%d columns)\n", name, pattern, column_count))
  }

  # Schemas
  cat("\nSchemas:\n")
  for (name in names(x$schemas)) {
    schema <- x$schemas[[name]]
    primary_key <- paste(schema$primary_key, collapse = ", ")
    column_count <- length(schema$data_columns)
    cat(sprintf("  - %s: Primary Key = %s (%d columns)\n", name, primary_key, column_count))
  }
}

#' @export
summary.jpaccidents_config <- function(object, ...) {
  cat("Accident Data Configuration Summary\n")
  cat("-----------------------------------\n")

  # File Types
  cat("File types:\n")
  for (name in names(object$file_types)) {
    info <- object$file_types[[name]]
    cat(sprintf("  - %s\n", name))
    cat(sprintf("      Pattern: %s\n", info$pattern))
    cat(sprintf("      Columns: [%s]\n", paste(info$columns, collapse = ", ")))
  }

  # Schemas
  cat("\nSchemas:\n")
  for (name in names(object$schemas)) {
    schema <- object$schemas[[name]]
    cat(sprintf("  - %s\n", name))
    cat(sprintf("      Primary Key: [%s]\n", paste(schema$primary_key, collapse = ", ")))
    cat(sprintf("      Data Columns: [%s]\n", paste(schema$data_columns, collapse = ", ")))
    cat(sprintf("      Source Files: [%s]\n", paste(schema$source_files, collapse = ", ")))
    if (!is.null(schema$suffixes)) {
      cat(sprintf("    Suffixes: [%s]\n", paste(schema$suffixes, collapse = ", ")))
    }
  }

  # Column Name Mapping
  cat("\nColumn Name Mapping:\n")
  for (name in names(object$columns)) {
    mapping <- object$columns[[name]]
    cat(sprintf("  - %s: [%s]\n", name, paste(mapping, collapse = ", ")))
  }
}
