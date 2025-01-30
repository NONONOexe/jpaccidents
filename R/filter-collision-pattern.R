#' Filter accident data by collision patterns
#'
#' `filter_collision_pattern()` filters accident data based on primary and
#' secondary impact conditions for each party involved in the accident.
#'
#' The values of `primary_impacts` and `secondary_impacts` are following:
#'   * `"no_impact"`  : No impact
#'   * `"front"`      : Front of the vehicle
#'   * `"right"`      : Right side of the vehicle
#'   * `"rear"`       : Rear of the vehicle
#'   * `"left"`       : Left side of the vehicle
#'   * `"front_right"`: Front right of the vehicle
#'   * `"rear_right"` : Rear right of the vehicle
#'   * `"rear_left"`  : Rear left of the vehicle
#'   * `"front_left"` : Front left of the vehicle
#' The order of values in `primary_impacts` and `secondary_impacts`
#' corresponds to the party order (`party_order`).
#'
#' @param data A list containing accident data.
#' @param primary_impacts A vector specifying the initial collision points
#'   (primary impact) for each involved party.
#' @param secondary_impacts A vector specifying the subsequent collision points
#'   (secondary impact) for each involved party (default: `NA_character_`).
#' @return A list containing filtered accident data.
#' @export
#' @examples
#' \dontrun{
#' # Filter accident data by cases indicating head-on collisions
#' head_on_collisions <- filter_collision_pattern(data, c("front", "front"))
#'
#' # Filter accident data by cases indicating rear-end collisions
#' rear_end_collisions <- filter_collision_pattern(data, c("rear", "front"))
#' }
filter_collision_pattern <- function(data,
                                     primary_impacts,
                                     secondary_impacts = NA_character_) {
  # Validate impact values
  validate_conditions(primary_impacts, secondary_impacts)

  # Display filtering message
  cli::cli_alert_info(c(
    "Filtering by following condition(s): ",
    "primary_impacts == {.val {primary_impacts}}",
    if (all(!is.na(secondary_impacts)))
      "and secondary_impacts == {.val {secondary_impacts}}"
  ))

  # Create condition table for filtering
  conditions <- create_condition_table(primary_impacts, secondary_impacts)

  # Filter party information based on the impact conditions
  filter_keys <- data$person_info %>%
    dplyr::inner_join(conditions, by = "party_order") %>%
    dplyr::filter(
      .data$primary_impact == .data$primary_impact_condition,
      dplyr::coalesce(
        .data$secondary_impact == .data$secondary_impact_condition,
        TRUE
      )
    ) %>%
    dplyr::filter(dplyr::n() == nrow(conditions), .by = key_columns) %>%
    dplyr::select(dplyr::all_of(key_columns)) %>%
    dplyr::distinct()

  # Filter datasets based on the matching keys
  filter_accident_data(data, filter_keys)
}

validate_conditions <- function(primary_impacts, secondary_impacts) {
  valid_values <- code_label_map$primary_impact$labels
  invalid_primary <- !(primary_impacts %in% valid_values)
  invalid_secondary <- !is.na(secondary_impacts) &
    !(secondary_impacts %in% valid_values)

  if (any(invalid_primary) || any(invalid_secondary)) {
    cli::cli_abort("{.code primary_impacts} and {.code secondary_impacts} must be one of the following values: {.val {valid_values}}.")
  }
}

create_condition_table <- function(primary_impacts, secondary_impacts) {
  num_parties <- length(primary_impacts)
  tibble::tibble(
    party_order                = seq_len(num_parties),
    primary_impact_condition   = primary_impacts,
    secondary_impact_condition = secondary_impacts
  )
}
