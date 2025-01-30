#' Filter accident data by accident type
#'
#' `filter_accident_type()` filters accident data based on the accident type.
#'
#' @param data A list containing accident data.
#' @param accident_type A vector of specifying the accident type to
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

  # Filter accident information based on the accident type
  filter_keys <- data$accident_info %>%
    dplyr::filter(.data$accident_type %in% {{accident_type}}) %>%
    dplyr::select(dplyr::all_of(key_columns)) %>%
    sf::st_drop_geometry()

  # Filter datasets based on the matching keys
  filter_accident_data(filter_keys)
}

validate_accident_type <- function(accident_type) {
  valid_values <- code_label_map$accident_type$labels
  invalid_accident_type <- !(accident_type %in% valid_values)
  if (any(invalid_accident_type)) {
    cli::cli_abort("{.code accident_type} must be one of the following values: {.val {valid_values}}.")
  }
}
