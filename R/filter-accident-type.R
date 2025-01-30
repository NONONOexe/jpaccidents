#' Filter accident data by accident type
#'
#' `filter_accident_type()` filters accident data based on the accident type.
#'
#' @param data A list containing accident data.
#' @param accident_type A character string specifying the accident type to
#'   filter. Must be one of the following values:
#'   * `"vehicle_to_pedestrian"`: Vehicle-to-pedestrian collision
#'   * `"vehicle_to_vehicle"`   : Vehicle-to-vehicle collision
#'   * `"single_vehicle"`       : Single-vehicle accident
#'   * `"train"`                : Train accident
#' @return A list containing filtered accident data.
#' @export
#' @examples
#' \dontrun{
#' # Filter accident data by vehicle-to-pedestrian collisions
#' vehicle_to_pedestrian_collisions <- filter_accident_type(
#'   data,
#'   "vehicle_to_pedestrian"
#' )
#'
#' # Filter accident data by vehicle-to-vehicle collisions
#' vehicle_to_pedestrian_collisions <- filter_accident_type(
#'   data,
#'   "vehicle_to_vehicle"
#' )
#' }
filter_accident_type <- function(data, accident_type) {
  # Validate accident type values
  validate_accident_type(accident_type)

  # Display filtering message
  cli::cli_alert_info(c(
    "Filtering by following condition(s): ",
    "accident_type == {.val {accident_type}}"
  ))

  # Define key columns for accident records
  key_columns <- c(
    "document_type", "report_year", "prefecture_code",
    "police_code", "report_number"
  )

  # Filter accident information based on the accident type
  filtered_key <- data$accident_info %>%
    dplyr::filter(.data$accident_type == accident_type) %>%
    dplyr::select(dplyr::all_of(key_columns)) %>%
    sf::st_drop_geometry()

  # Display matched record count
  matched_num <- nrow(filtered_key)
  cli::cli_alert_info(
    if (matched_num == 0) "No matched records found."
    else "Matched records: {matched_num}"
  )

  # Filter datasets based on the matching keys
  data %>%
    purrr::map(dplyr::semi_join, filtered_key, by = key_columns)
}

validate_accident_type <- function(accident_type) {
  valid_values <- code_label_map$accident_type$labels
  if (length(accident_type) != 1 || !(accident_type %in% valid_values)) {
    cli::cli_abort("{.code accident_type} must be one of the following values: {.val {valid_values}}.")
  }
}
