#' Filter accident data
#'
#' `filter_accident_data()` filters accident data, retaining only the records
#' that have matching values in the key columns specified in `filter_keys`.
#'
#' @param data A accident_data object.
#' @param filter_keys A data frame containing key columns to filter the data.
#' @return A filtered accident_data object.
#' @keywords internal
filter_accident_data <- function(data, filter_keys) {

  # Display matched record count
  matched_num <- nrow(filter_keys)
  cli::cli_alert_info(
    if (matched_num == 0) "No matched records found."
    else "Matched records: {matched_num}"
  )

  # Filter datasets based on the matching keys
  filtered_data <- data %>%
    purrr::map(dplyr::semi_join, filter_keys, by = key_columns) %>%
    as_accident_data()

  return(filtered_data)
}
