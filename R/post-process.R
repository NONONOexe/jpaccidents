#' Post-process data
#'
#' `post_process()` performs post-processing on different datasets:
#'   - `post_process_main()` handles main data (honhyo) by separating it into
#'     accident and person data.
#'   - `post_process_sub()` handles sub data (hojuhyo), processing
#'     person-related information.
#'   - `post_process_highway()` handles highway data (kosokuhyo), extracting
#'     highway-related information.
#'
#' These functions process the data based on its type, ensuring consistency
#' and proper formatting.
#'
#' @name post_process
#' @param data The data to be processed.
#' @return A processed dataset as a list.
post_process <- function(data) {
  UseMethod("post_process")
}

#' @rdname post_process
#' @export
post_process.accident_data <- function(data) {
  # Extract dataset name attribute
  dataset_name <- attr(data, "dataset_name")

  # Determine the appropriate post-processor function
  post_processor <- switch (
    dataset_name,
    "main_data"    = post_process_main,
    "sub_data"     = post_process_sub,
    "highway_data" = post_process_highway,
    function(data) {
      cli_alert_warning("Unknown dataset name: {dataset_name}")
      return(NULL)
    }
  )

  # Process the data
  processed_data <- post_processor(data)
  attr(processed_data, "dataset_name") <- dataset_name

  return(processed_data)
}

#' @rdname post_process
#' @export
post_process_main <- function(data) {
  # Convert coordinates from DMS to decimal
  data$latitude <- suppressWarnings(convert_deg(data$latitude))
  data$longitude <- suppressWarnings(convert_deg(data$longitude))

  # Filter valid rows based on non-missing coordinates
  valid_rows <- !is.na(data$latitude) & !is.na(data$longitude)
  location_data <- data[valid_rows, ]

  # Convert data to spatial data format (sf object)
  location_data_sf <- st_as_sf(
    location_data,
    coords = c("longitude", "latitude"),
    crs = 4326
  )

  # Alert user if any rows were removed due to invalid coordinates
  removed_rows <- nrow(data) - nrow(location_data_sf)
  file_path <- attr(data, "file_path")
  if (0 < removed_rows) {
    cli_alert_warning("Invalid coordinate format detected in file: {file_path}. {removed_rows} rows have been excluded.")
  }

  # Process accident information
  accident_info <- location_data_sf %>%
    mutate(
      document_type   = translate_codes(.data$document_type, "document_type"),
      occurrence_time = make_datetime(
        year  = as.integer(location_data_sf$occurrence_year),
        month = as.integer(location_data_sf$occurrence_month),
        day   = as.integer(location_data_sf$occurrence_day),
        hour  = as.integer(location_data_sf$occurrence_hour),
        min   = as.integer(location_data_sf$occurrence_min),
        tz    = "Asia/Tokyo"
      ),
      impact_type     = translate_codes(.data$impact_type, "impact_type")
    ) %>%
    extract_schema_columns("accident_info")

  # Process person information
  person_info <- location_data %>%
    pivot_longer(
      cols          = ends_with(c("_a", "_b")),
      names_to      = c(".value", "sub_id"),
      names_pattern = "^(.+)[_]([a|b])$"
    ) %>%
    mutate(
      party_order = case_when(
        .data$sub_id == "a" ~ 1L,
        .data$sub_id == "b" ~ 2L,
        TRUE ~ NA_integer_
      ),
      primary_impact   = substr(.data$impact_part, 1, 1),
      secondary_impact = substr(.data$impact_part, 2, 2)
    ) %>%
    mutate(
      document_type    = translate_codes(.data$document_type, "document_type"),
      primary_impact   = translate_codes(.data$primary_impact, "impact_part"),
      secondary_impact = translate_codes(.data$secondary_impact, "impact_part")
    ) %>%
    set_attr("dataset_name", attr(data, "dataset_name")) %>%
    extract_schema_columns("person_info")

  return(list(
    accident_info = accident_info,
    person_info   = person_info,
    highway_info  = NULL
  ))
}

#' @rdname post_process
#' @export
post_process_sub <- function(data) {
  # Extract and process person information
  person_info <- data %>%
    mutate(party_order = as.integer(.data$sub_id) + 2L) %>%
    extract_schema_columns("person_info")

  return(list(
    accident_info = NULL,
    person_info   = person_info,
    highway_info  = NULL
  ))
}

#' @rdname post_process
#' @export
post_process_highway <- function(data) {
  # Extract highway information
  highway_info <- extract_schema_columns(data, "highway_info")

  return(list(
    accident_info = NULL,
    person_info   = NULL,
    highway_info  = highway_info
  ))
}
