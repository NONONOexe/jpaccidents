#' Translate codes to human-readable labels using configuration data
#'
#' `translate_codes()` translates codes to their corresponding human-readable
#' labels based on the configuration data. If the code is not found in the
#' configuration data, it will issue a warning and return `NA`.
#'
#' If the specified `concept` is not found in the configuration, the function
#' will raise an error and halt execution.
#'
#' @param codes A character vector of codes to be converted to labels.
#' @param concept A string specifying the concept category.
#' @return A character vector of the same length as the input `codes`,
#'   containing the corresponding labels. Invalid codes are converted to `NA`
#'   with a warning.
#' @export
#' @examples
#' # Convert a single code
#' translate_codes("1", "impact_part")
#'
#' # Convert multiple codes
#' translate_codes(c("1", "2"), "impact_part")
#'
#' # Handle invalid codes
#' translate_codes(c("1", "999"), "impact_part")
#'
translate_codes <- function(codes, concept) {
  # Get configuration
  config <- get_config()

  # Extract the relevant concept configuration
  concept_config <- config$concepts[[concept]]

  # Error handling for undefined concept
  if (is.null(concept_config)) {
    cli_abort("The concept '{concept}' is not defined in the configuration.")
  }

  # Create lookup table
  concept_codes <- sapply(concept_config, function(x) x$code)
  lookup <- setNames(
    rep(names(concept_codes), lengths(concept_codes)),
    unlist(concept_codes, use.names = FALSE)
  )

  # Issue a warning if there are invalid codes
  invalid_codes <- codes[!codes %in% names(lookup)]
  if (0 < length(invalid_codes)) {
    cli_alert_warning("In concept {concept}, The following code(s) is not registered: {invalid_codes}.")
  }

  # Map codes to their labels; assign NA if code is not found
  labels <- lookup[codes]
  labels[is.na(labels)] <- NA_character_

  return(labels)
}
