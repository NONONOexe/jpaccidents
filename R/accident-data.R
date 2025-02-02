#' Convert a list to an accident_data object
#'
#' `as_accident_data` converts a list to an `accident_data` object.
#'
#' @param x A list containing accident data. The list must contain followings:
#'   `accident_info`, `person_info`, and `highway_info`.
#' @return A `accident_data` object.
#' @keywords internal
as_accident_data <- function(x) {
  if (!all(c("accident_info", "person_info", "highway_info") %in% names(x))) {
    cli::cli_abort("Accident data must contain {.code accident_info}, {.code person_info}, and {.code highway_info}.")
  }
  accident_data <- structure(x, class = c("accident_data", "list"))

  return(accident_data)
}
