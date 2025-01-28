#' Read accident data from CSV files
#'
#' `read_accident_data()` reads accident data from multiple CSV files,
#' processes each file, and combines the data into three categories:
#'   * `accident_info`: Basic accident information including date, location,
#'     and conditions
#'   * `person_info`  : Information about individuals involved in the accidents
#'   * `highway_info` : Information specific to accidents that occurred on
#'     highways/expressways
#'
#' @param file_paths A character vector of file paths to the CSV files.
#' @return A list containing three data frames (`accident_info`, `person_info`,
#'   and `highway_info`).
#' @export
#' @examples
#' \dontrun{
#' # Read main and supplementary data for 2023
#' read_accident_data(c("example/honhyo_2023.csv", "example/hojuhyo_2023.csv"))
#' }
read_accident_data <- function(file_paths) {
  accident_data <- file_paths %>%
    validate_file_paths() %>%            # Validate input file paths
    purrr::map(read_accident_file) %>%   # Read each accident file
    purrr::map(tidy_accident_data) %>%   # Tidy each accident dataset
    merge_accident_data()                # Merge data into single dataset

  return(accident_data)
}

# Input Validation --------------------------------------------------------

#' Validate input file paths
#'
#' @param file_paths A character vector of file paths
#' @return A character vector of validated file paths
#' @keywords internal
validate_file_paths <- function(file_paths) {
  valid_files <- file_paths %>%
    check_filename_format() %>%
    check_year_range() %>%
    check_file_exists()

  return(valid_files)
}

# Check if file names follow the required format
check_filename_format <- function(file_paths) {
  valid_pattern <- "^.*(honhyo|hojuhyo|kosokuhyo)[_]([0-9]{4})[.]csv$"
  invalid_format <- !stringr::str_detect(file_paths, valid_pattern)

  if (any(invalid_format)) {
    notify_invalid_files(file_paths[invalid_format], "format")
  }

  return(file_paths[!invalid_format])
}

# Verify if the data years are within supported range
check_year_range <- function(file_paths) {
  extracted_years <- extract_years_from_paths(file_paths)
  invalid_years <- extracted_years < 2019 | 2023 < extracted_years

  if (any(invalid_years)) {
    notify_invalid_files(file_paths[invalid_years], "year range (2019-2023)")
  }

  return(file_paths[!invalid_years])
}

# Check if files exist in the specified locations
check_file_exists <- function(file_paths) {
  file_exists <- fs::file_exists(file_paths)

  if (any(!file_exists)) {
    notify_invalid_files(file_paths[!file_exists], "existence")
  }

  return(file_paths[file_exists])
}

extract_years_from_paths <- function(file_paths) {
  extracted_years <- as.integer(stringr::str_match(
    file_paths,
    "^.*(honhyo|hojuhyo|kosokuhyo)[_]([0-9]{4})[.]csv$"
  )[, 3])

  return(extracted_years)
}

notify_invalid_files <- function(files, issue) {
  cli::cli_alert_warning(
    "Skipping file(s) due to invalid {issue}: {.file {files}}."
  )
}

# Data Reading ------------------------------------------------------------

#' Read CSV file with common parameters
#'
#' @param file_path Path to the CSV file
#' @return tibble with raw accident data
#' @keywords internal
read_accident_file <- function(file_path) {
  raw_data <- file_path %>%
    readr::read_csv(
      col_types      = readr::cols(.default = "c"),
      locale         = readr::locale(encoding = "Shift_JIS"),
      show_col_types = FALSE
    ) %>%
    dplyr::rename_with(standardise_column_name)

  # Add file type attribute
  attr(raw_data, "file_type") <- detect_file_types(file_path)
  attr(raw_data, "file_path") <- file_path

  return(raw_data)
}

standardise_column_name <- function(column_names) {
  column_name_map[column_names]
}

detect_file_types <- function(file_paths) {
  data_names <- stringr::str_match(
    file_paths,
    "^.*(honhyo|hojuhyo|kosokuhyo)[_]([0-9]{4})[.]csv$"
  )[, 2]

  file_types <- dplyr::case_when(
    data_names == "honhyo"    ~ "main",
    data_names == "hojuhyo"   ~ "supp",
    data_names == "kosokuhyo" ~ "highway"
  )

  return(file_types)
}


# Data Processing ---------------------------------------------------------

#' Tidy accident data based on file type
#'
#' @param raw_data Raw data frame with file type attribute
#' @return List of processed data frames
#' @keywords internal
tidy_accident_data <- function(raw_data) {
  data_processor <- switch(
    attr(raw_data, "file_type"),
    main    = process_main_data,
    supp    = process_supplementary_data,
    highway = process_highway_data
  )

  return(data_processor(raw_data))
}

# Process main accident records
process_main_data <- function(main_data) {
  processed_main <- main_data %>%
    transform_coordinates() %>%
    create_datetime_field()

  # Split into accident and person information
  return(list(
    accident_info = extract_accident_info(processed_main),
    person_info   = extract_person_info(processed_main),
    highway_info  = NULL
  ))
}

# Process supplementary person records
process_supplementary_data <- function(supp_data) {
  person_info <- supp_data %>%
    dplyr::mutate(party_order = as.integer(.data$supplementary_id) + 2L) %>%
    dplyr::select(dplyr::any_of(person_info_columns)) %>%
    apply_code_labels()

  return(list(
    accident_info = NULL,
    person_info   = person_info,
    highway_info  = NULL
  ))
}

# Process highway-specific records
process_highway_data <- function(highway_data) {
  highway_info <- highway_data %>%
    dplyr::select(dplyr::any_of(highway_info_columns)) %>%
    apply_code_labels()

  return(list(
    accident_info = NULL,
    person_info   = NULL,
    highway_info  = highway_info
  ))
}

transform_coordinates <- function(data) {
  # Validate required columns
  require_columns <- c("latitude", "longitude")
  if (!all(require_columns %in% colnames(data))) {
    cli::cli_abort("Data must contain {.code latitude} and {.code longitude} columns.")
  }

  # Convert DMS to decimal format
  transformed_data <- data %>%
    dplyr::mutate(
      latitude  = transform_dms_to_decimal(.data$latitude),
      longitude = transform_dms_to_decimal(.data$longitude)
    )

  # Identify rows with invalid coordinates
  invalid_rows <- which(
    any(is.na(transformed_data$latitude) | is.na(transformed_data$longitude))
  )

  # Warn about invalid rows if any
  if (0 < length(invalid_rows)) {
    file_path <- attr(main_data, "file_path")
    cli::cli_alert_warning(c(
      "The file {.file file_path} contains invalid or missing coordinate data. ",
      "The following rows are skipped: {toString(skipped_rows)}.}"
    ))
  }

  # Exclude rows with invalid coordinates
  result <- transformed_data %>%
    tidyr::drop_na(latitude, longitude)

  return(result)
}

create_datetime_field <- function(data) {
  data %>%
    dplyr::mutate(
      occurrence_time = lubridate::make_datetime(
        year  = as.integer(.data$occurrence_year),
        month = as.integer(.data$occurrence_month),
        day   = as.integer(.data$occurrence_day),
        hour  = as.integer(.data$occurrence_hour),
        min   = as.integer(.data$occurrence_min)
      )
    )
}

extract_accident_info <- function(data) {
  data %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    dplyr::select(dplyr::any_of(accident_info_columns)) %>%
    apply_code_labels()
}

extract_person_info <- function(data) {
  data %>%
    tidyr::pivot_longer(
      cols          = tidyr::ends_with(c("_a", "_b")),
      names_to      = c(".value", "supplementary_id"),
      names_pattern = "^(.+)[_]([ab])$"
    ) %>%
    dplyr::mutate(
      party_order = dplyr::case_when(
        .data$supplementary_id == "a" ~ 1L,
        .data$supplementary_id == "b" ~ 2L,
        TRUE ~ NA_integer_
      ),
      primary_impact   = substr(.data$impact_part, 1, 1),
      secondary_impact = substr(.data$impact_part, 2, 2)
    ) %>%
    dplyr::select(dplyr::any_of(person_info_columns)) %>%
    apply_code_labels()
}

apply_code_labels <- function(data) {
  data %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of(names(code_label_map)),
      function(codes) code_label_map[[dplyr::cur_column()]]$labels[codes]
    ))
}


# Coordinate Transformation -----------------------------------------------

#' Transform DMS coordinates to decimal degrees
#'
#' @param dms Numeric vector of coordinates in DDMMSS.sss format
#' @return Numeric vector of decimal degrees
#' @keywords internal
transform_dms_to_decimal <- function(dms) {
  dms_str <- format_dms_string(dms)

  # Extract components
  degrees <- extract_dms_components(dms_str, 1, -7)
  minutes <- extract_dms_components(dms_str, -6, -5)
  seconds <- calculate_seconds(dms_str)

  # Validate ranges and convert invalid values to NA
  invalid_minutes <- !is.na(minutes) & (minutes < 0 | 60 <= minutes)
  invalid_seconds <- !is.na(seconds) & (seconds < 0 | 60 <= seconds)

  if (any(invalid_minutes)) {
    cli::cli_alert_warning("Minutes must be between 0 and 60.")
    m[invalid_minutes] <- NA
  }

  if (any(invalid_seconds)) {
    cli::cli_alert_warning("Seconds must be between 0 and 60.")
    s[invalid_seconds] <- NA
  }

  # Calculate decimal degrees
  decimal_degrees <- degrees + minutes / 60 + seconds / 3600

  return(decimal_degrees)
}

format_dms_string <- function(dms) {
  # Convert to character and trim white space
  dms_str <- trimws(format(dms, scientific = FALSE))

  # Validate input format
  valid_format <- grepl("^[0-9]{8,10}$", dms_str)
  if (!all(valid_format)) {
    cli::cli_alert_warning(
      "The input must be a number consisting of 8 to 10 digits."
    )
    dms_str[!valid_format] <- NA
  }

  return(dms_str)
}

extract_dms_components <- function(dms_str, start, end) {
  str_len <- nchar(dms_str)
  start <- ifelse(start < 0, str_len + start, start)
  end <- ifelse(end < 0, str_len + end, end)
  dms_component <- as.numeric(substr(dms_str, start, end))

  return(dms_component)
}

calculate_seconds <- function(dms_str) {
  seconds <- extract_dms_components(dms_str, -4, -3)
  milliseconds <- extract_dms_components(dms_str, -2, nchar(dms_str))

  return(seconds + milliseconds / 1000)
}


# Data Merging ------------------------------------------------------------

merge_accident_data <- function(data_list) {
  merged_data <- data_list %>%
    purrr::transpose() %>%
    purrr::map(dplyr::bind_rows)

  return(merged_data)
}
