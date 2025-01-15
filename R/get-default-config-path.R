#' Get the path to the default configuration file
#'
#' This function retrieves the path to the default configuration YAML file for
#' the jpaccidents package.
#'
#' @return The path to the default configuration file
#' @export
#' @examples
#' get_default_config_path()
get_default_config_path <- function() {
  default_config_path <- system.file(
    "extdata",
    "jpaccidents-config.yaml",
    package  = "jpaccidents",
    mustWork = TRUE
  )

  return(default_config_path)
}
