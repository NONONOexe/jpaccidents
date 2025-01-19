#' Post-process data
#'
#' These function perform post-processing operations on different datasets.
#' `post_process_main` processes the main data (honhyo), separating it into
#' accident and person data, and returning them as a list.
#' `post_process_sub` and `post_process_highway` currently do not perform
#' any modifications on sub data and highway data, respectively.
#'
#' @name post_process
#' @param data The data to be processed.
#' @return A processed data.
post_process <- function(data) {
  UseMethod("post_process")
}

#' @rdname post_process
#' @export
post_process.accident_data <- function(data) {
  dataset_name <- attr(data, "dataset_name")
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
  processed_data <- post_processor(data)
  attr(processed_data, "dataset_name") <- dataset_name

  return(processed_data)
}

#' @rdname post_process
#' @export
post_process_main <- function(data) {
  # Convert latitude and longitude from DMS to decimal
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

  # Extract accident related columns from spatial data
  accidents_info <- extract_schema_columns(location_data_sf, "accidents_info")

  # Create datetime from individual columns in the original data
  accidents_info$occurrence_time <- make_datetime(
    year  = as.integer(location_data_sf$occurrence_year),
    month = as.integer(location_data_sf$occurrence_month),
    day   = as.integer(location_data_sf$occurrence_day),
    hour  = as.integer(location_data_sf$occurrence_hour),
    min   = as.integer(location_data_sf$occurrence_min),
    tz    = "Asia/Tokyo"
  )

  # Extract person related columns from the original data
  persons_info <- extract_schema_columns(location_data, "persons_info")

  # Combine process accident and person data into a list
  processed_data <- list(
    accidents_info = accidents_info,
    persons_info = persons_info
  )

  return(processed_data)
}

#' @rdname post_process
#' @export
post_process_sub <- function(data) {
  return(extract_schema_columns(data, "persons_info"))
}

#' @rdname post_process
#' @export
post_process_highway <- function(data) {
  return(extract_schema_columns(data, "highways_info"))
}
