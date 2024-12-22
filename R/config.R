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
#' \dontrun{
#' config <- get_config()
#' }
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
  load_config(system.file(
    "config",
    "jpaccidents-config.yaml",
    package = "jpaccidents"
  ))
}

#' @export
print.jpaccidents_config <- function(x, ...) {
  print_cols <- function(cols) {
    for (name in names(x$main_data$columns)) {
      column <- x$main_data$columns[[name]]
      cat("    -", name)
      cat(":", column$original_name, "\n")
    }
  }

  cat("Accident Data Configuration\n")
  cat("Path:", paste0("\"", attr(x, "path"), "\"\n"))

  cat("Main data:\n")
  cat("  File pattern:", paste0("\"", x$main_data$file_pattern, "\"\n"))
  cat("  Columns:\n")
  print_cols(x$main_data$columns)

  cat("Sub data:\n")
  cat("  File pattern:", paste0("\"", x$sub_data$file_pattern, "\"\n"))
  cat("  Columns:\n")
  print_cols(x$sub_data$columns)

  cat("Highway data:\n")
  cat("  File pattern:", paste0("\"", x$highway_data$file_pattern, "\"\n"))
  cat("  Columns:\n")
  print_cols(x$highway_data$columns)
}
