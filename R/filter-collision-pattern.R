#' Filter collision patterns from accident data
#'
#' `filter_collision_pattern()` filters accident data based on primary and
#' secondary impact conditions for each party involved in the accident.
#'
#' The values of `primary_impacts` and `secondary_impacts` are following:
#'   * `"no_impact"`   : No impact
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
    "Filtering collision patterns...",
    "Primary impacts: {.val {primary_impacts}}",
    if (all(!is.na(secondary_impacts)))
      ", Secondary impacts: {.val {secondary_impacts}}"
  ))

  # Create condition table for filtering
  person_conditions <- create_condition_table(
    primary_impacts, secondary_impacts
  )

  # Define key columns for accident records
  key_columns <- c(
    "document_type", "report_year", "prefecture_code",
    "police_code", "report_number"
  )

  # Filter party information based on the impact conditions
  filtered_key <- data$person_info %>%
    dplyr::inner_join(person_conditions, by = "party_order") %>%
    dplyr::filter(
      .data$primary_impact == .data$primary_impact_condition,
      dplyr::coalesce(
        .data$secondary_impact == .data$secondary_impact_condition,
        TRUE
      )
    ) %>%
    dplyr::filter(
      dplyr::n() == nrow(person_conditions),
      .by = key_columns
    ) %>%
    dplyr::select(dplyr::all_of(key_columns)) %>%
    dplyr::distinct()

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

validate_conditions <- function(primary_impacts, secondary_impacts) {
  valid_values <- c(
    "no_impact", "front", "right", "rear", "left",
    "front_right", "rear_right", "rear_left", "front_left"
  )

  invalid_primary <- !(primary_impacts %in% valid_values)
  invalid_secondary <- !is.na(secondary_impacts) &
    !(secondary_impacts %in% valid_values)

  if (any(invalid_primary) || any(invalid_secondary)) {
    cli::cli_abort(paste0(
      "{.code primary_impacts} and {.code secondary_impacts} must be",
      "one of the following values: {.val {valid_values}}."
    ))
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
