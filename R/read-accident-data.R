#' Read the traffic accident data
#'
#' @description
#' Read the file of traffic accident data.
#'
#' Use the appropriate function for the type of data you want to read:
#'
#'   * `read_accident_main_data()` to read the main data (honhyo).
#'   * `read_accident_sub_data()` to read the sub data (hojuhyo).
#'   * `read_accident_highway_data()` to read the highway data (kosokuhyo).
#'
#' @param file A character string of the file path.
#' @return
#' `read_accident_main_data()` separates traffic accident data
#' and injured party data from the target data and returns them as a list.
#' `read_accident_sub_data()` and `read_accident_highway_data()` return
#' the contents of the target data as a tibble.
#' @examples
#' \dontrun{
#' read_accident_main_data("downloaded-file-path.csv")
#' read_accident_sub_data("downloaded-file-path.csv")
#' read_accident_highway_data("downloaded-file-path.csv")
#' }
#' @name read_accident_data
NULL

#' @rdname read_accident_data
#' @export
read_accident_main_data <- function(file) {
  main_raw_data <- file |>
    read_csv(
      col_types      = cols(.default = "c"),
      locale         = locale(encoding = "Shift_JIS"),
      progress       = FALSE,
      show_col_types = FALSE,
      lazy           = FALSE
    ) |>
    rename(!!column_name_main)

  accident_data <- extract_accident_data(main_raw_data)
  party_a_data <- extract_party_a_data(main_raw_data)
  party_b_data <- extract_party_b_data(main_raw_data)
  party_data <- bind_rows(party_a_data, party_b_data)

  main_data <- list(
    traffic_accidents = accident_data,
    injured_parties   = party_data
  )

  return(main_data)
}

extract_accident_data <- function(main_raw_data) {
  accident_data <- main_raw_data |>
    select(
      "document_type", "prefecture_code", "police_code", "main_id",
      "injury_pattern", "faitality_number", "injury_number", "road_code",
      "kilopost_number", "city_code", "occurrence_year", "occurrence_month",
      "occurrence_day", "occurrence_hour", "occurrence_min", "day_night",
      "sunrise_hour", "sunrise_min", "sunset_hour", "sunset_min",
      "weather", "region_type", "road_surface", "road_shape",
      "traffic_signal", "road_width", "road_alignment", "collision_position",
      "zone_regulation", "center_divider", "road_verge", "impact_type",
      "latitude", "longitude", "day_of_week", "holiday"
    ) |>
    mutate(
      occurrence_time = make_datetime(
        year  = as.integer(.data$occurrence_year),
        month = as.integer(.data$occurrence_month),
        day   = as.integer(.data$occurrence_day),
        hour  = as.integer(.data$occurrence_hour),
        min   = as.integer(.data$occurrence_min),
        tz    = "Asia/Tokyo"
      ),
      latitude = .data$latitude |>
        str_replace("^([0-9]{2})([0-9]{7})$", "\\1.\\2") |>
        convert_deg(),
      longitude = .data$longitude |>
        str_replace("^([0-9]{3})([0-9]{7})$", "\\1.\\2") |>
        convert_deg(),
      .keep = "unused"
    ) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    rename(geom = "geometry")

  return(accident_data)
}

extract_party_a_data <- function(main_raw_data) {
  party_a_data <- main_raw_data |>
    mutate(sub_id = "A") |>
    select(
      "document_type",
      "prefecture_code",
      "police_code",
      "main_id",
      "sub_id",
      stop_sign        = "stop_sign_a",
      stop_mark        = "stop_mark_a",
      age              = "age_a",
      party_type       = "party_type_a",
      use_type         = "use_type_a",
      car_type         = "car_type_a",
      automatic_car    = "automatic_car_a",
      support_car      = "support_car_a",
      speed_limit      = "speed_limit_a",
      impact_part      = "impact_part_a",
      damage_level     = "damage_level_a",
      airbag           = "airbag_a",
      side_airbag      = "side_airbag_a",
      injury_level     = "injury_level_a",
      cognitive_test   = "cognitive_test_a",
      driving_practice = "driving_practice_a"
    )

  return(party_a_data)
}

extract_party_b_data <- function(main_raw_data) {
  party_b_data <- main_raw_data |>
    mutate(sub_id = "B") |>
    select(
      "document_type",
      "prefecture_code",
      "police_code",
      "main_id",
      "sub_id",
      stop_sign        = "stop_sign_b",
      stop_mark        = "stop_mark_b",
      age              = "age_b",
      party_type       = "party_type_b",
      use_type         = "use_type_b",
      car_type         = "car_type_b",
      automatic_car    = "automatic_car_b",
      support_car      = "support_car_b",
      speed_limit      = "speed_limit_b",
      impact_part      = "impact_part_b",
      damage_level     = "damage_level_b",
      airbag           = "airbag_b",
      side_airbag      = "side_airbag_b",
      injury_level     = "injury_level_b",
      cognitive_test   = "cognitive_test_b",
      driving_practice = "driving_practice_b"
    )

  return(party_b_data)
}

#' @rdname read_accident_data
#' @export
read_accident_sub_data <- function(file) {
  sub_data <- file |>
    read_csv(
      col_types      = cols(.default = "c"),
      locale         = locale(encoding = "Shift_JIS"),
      progress       = FALSE,
      show_col_types = FALSE,
      lazy           = FALSE
    ) |>
    rename(!!column_name_sub)

  return(sub_data)
}

#' @rdname read_accident_data
#' @export
read_accident_highway_data <- function(file) {
  highway_data <- file |>
    read_csv(
      col_types      = cols(.default = "c"),
      locale         = locale(encoding = "Shift_JIS"),
      progress       = FALSE,
      show_col_types = FALSE,
      lazy           = FALSE
    ) |>
    rename(!!column_name_highway)

  return(highway_data)
}
