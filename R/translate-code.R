#' Translate codes to human-readable labels using configuration data
#'
#' `translate_code()` translates codes to their corresponding human-readable
#' labels based on the configuration data. If the code is not found in the
#' configuration data, it will issue a warning and return `NA`.
#'
#' @param code A character vector of codes to be converted to labels.
#' @param concept A string specifying the concept category
#'   (e.g., "impact_part")
#' @return A character vector of the same length as the input `code`,
#'   containing the corresponding labels. Invalid codes are converted to `NA`
#'   with a warning.
#' @export
#' @examples
#' # Convert a single code
#' translate_code("1", "impact_part")
#'
#' # Convert multiple codes
#' translate_code(c("1", "2"), "impact_part")
#'
#' # Handle invalid codes
#' translate_code(c("1", "999"), "impact_part")
#'
translate_code <- function(code, concept) {
  # Get configuration
  config <- get_config()

  # Extract the relevant concept configuration
  concept_config <- config$concepts[[concept]]

  # Create lookup table
  lookup <- setNames(
    sapply(concept_config, function(x) x$label, USE.NAMES = FALSE),
    sapply(concept_config, function(x) x$code, USE.NAMES = FALSE)
  )

  # Issue a warning if there are invalid codes
  invalid_code <- code[!code %in% names(lookup)]
  if (0 < length(invalid_code)) {
    cli_alert_warning(
      "In concept {concept}, The following code(s) is not registered: {paste(invalid_code, collapse = ', ')}"
    )
  }

  # Map codes to their labels; assign NA if code is not found
  label <- ifelse(code %in% names(lookup), lookup[code], NA_character_)

  return(label)
}
