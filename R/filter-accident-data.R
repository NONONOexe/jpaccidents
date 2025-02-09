#' Filter accident data by categories
#'
#' These functions filter accident data based on the specified categories.
#'
#' @param data A `accident_data` object.
#' @param accident_type A vector of specifying the accident type to
#'   filter. Must be one of the following values:
#'   * `"vehicle_to_pedestrian"`: Vehicle-to-pedestrian collision
#'   * `"vehicle_to_vehicle"`   : Vehicle-to-vehicle collision
#'   * `"single_vehicle"`       : Single-vehicle accident
#'   * `"train"`                : Train accident
#' @param road_shape A vector of specifying the road type to filter.
#'   Must be one of the following values:
#'   * `"intersection_roundabout"`     : Roundabout intersection
#'   * `"intersection_other"`          : Regular intersection
#'   * `"near_intersection_roundabout"`: Area near a roundabout
#'   * `"near_intersection_other"`     : Area near an intersection
#'   * `"tunnel"`                      : Tunnel
#'   * `"bridge"`                      : Bridge
#'   * `"curve"`                       : Curve or bend in the road
#'   * `"single_road"`                 : Straight single road
#'   * `"railroad_crossing_type1"`     : Type 1 railroad crossing
#'   * `"railroad_crossing_type3"`     : Type 3 railroad crossing
#'   * `"railroad_crossing_type4"`     : Type 4 railroad crossing
#'   * `"general_traffic_area"`        : General traffic area
#' @return A filtered `accident_data` object.
#' @name filter_accident_data
#' @examples
#' \dontrun{
#' # Extract accidents where vehicle collided with pedestrian
#' vehicle_to_pedestrian_collisions <- data %>%
#'   filter_accident_type("vehicle_to_pedestrian")
#'
#' # Extract accidents involving vehicle-to-vehicle collisions
#' vehicle_to_pedestrian_collisions <- data %>%
#'   filter_accident_type("vehicle_to_pedestrian")
#'
#' # Extract accidents that occured on curved roads
#' curved_road_accidents <- data %>%
#'   filter_road_shape("curved")
#' }
NULL

#' @rdname filter_accident_data
#' @export
filter_accident_type <- function(data, accident_type) {
  filter_by_category(data, accident_type, "accident_type")
}

#' @rdname filter_accident_data
#' @export
filter_road_shape <- function(data, road_shape) {
  filter_by_category(data, road_shape, "curve")
}

#' Filter accident data by category
#'
#' `filter_by_category()` filters accident data based on the specified category
#' and its values.
#'
#' @param data A `accident_data` object.
#' @param category A vector specifying the category values to filter by.
#' @param category_name A string specifying the name of the category column.
#' @return A filtered `accident_data` object.
#' @keywords internal
filter_by_category <- function(data, category, category_name) {
  # Validate category values
  validate_category(category, category_name)

  # Display filtering message
  cli::cli_alert_info(c(
    "Filtering data where {.code {category_name}} ",
    "{if (1 < length(category)) 'is one of' else '=='} {.val {category}}"
  ))

  # Filter accident information based on the category
  filter_keys <- data$accident_info %>%
    dplyr::filter(.data[[category_name]] %in% {{category}}) %>%
    dplyr::select(dplyr::all_of(key_columns)) %>%
    sf::st_drop_geometry()

  # Filter datasets based on the matching keys
  filter_by_key(data, filter_keys)
}

#' Filter accident data by matching keys
#'
#' `filter_by_key()` filters accident data based on a set of matching keys.
#'
#' @param data A `accident_data` object.
#' @param filter_keys A data frame containing keys for filtering.
#' @return A filtered `accident_data` object.
#' @keywords internal
filter_by_key <- function(data, filter_keys) {
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

# Validate category values
validate_category <- function(category, category_name) {
  valid_values <- code_label_map[[category_name]]$labels
  invalid_category <- !(category %in% valid_values)
  if (any(invalid_category)) {
    cli::cli_abort("{.code invalid_category} must be one of the following values: {.val {valid_values}}.")
  }
}
