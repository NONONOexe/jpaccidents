#' Select post-processor function based on dataset name
#'
#' This function selects the post-processor function based on
#' the given dataset name.
#'
#' @param dataset_name The name of the dataset.
#' @return The post-processing function corresponding to the dataset name.
#' @export
select_post_processor <- function(dataset_name) {
  # Select post-processor
  post_processor <- switch(
    dataset_name,
    "main_data"    = post_process_main,
    "sub_data"     = post_process_sub,
    "highway_data" = post_process_highway,
    warning("Unknown dataset name: ", dataset_name)
  )

  return(post_processor)
}

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
NULL

#' @rdname post_process
#' @export
post_process_main <- function(data) {
  # Filter accident data
  accident_data <- filter_data_by_tag(data, "accident")

  # Create datetime from individual columns in the original data
  accident_data$occurrence_time <- make_datetime(
    year  = data$occurrence_year,
    month = data$occurrence_month,
    day   = data$occurrence_day,
    hour  = data$occurrence_hour,
    min   = data$occurrence_min,
    tz    = "Asia/Tokyo"
  )

  # Convert latitude and longitude from DMS to decimal
  accident_data$latitude  <- suppressWarnings(
    convert_deg(accident_data$latitude)
  )
  accident_data$longitude <- suppressWarnings(
    convert_deg(accident_data$longitude)
  )

  # Filter valid rows based on non-missing coordinates
  valid_rows <- !is.na(accident_data$latitude) & !is.na(accident_data$longitude)
  accident_data <- accident_data[valid_rows, ]

  if (nrow(accident_data) < nrow(data)) {
    warning("Invalid coordinate format detected. Some rows have been excluded.")
  }

  # Function to filter and clean person data
  filter_person_data <- function(data, tag, suf) {
    person_data <- filter_data_by_tag(data, tag)
    names(person_data) <- sub(paste0(suf, "$"), "", names(person_data))

    return(person_data)
  }

  # Filter person data
  person_a_data <- filter_person_data(data, "person_a", "_a")[valid_rows, ]
  person_b_data <- filter_person_data(data, "person_b", "_b")[valid_rows, ]

  # Filter key data
  key_data <- filter_data_by_tag(data, "key")[valid_rows, ]

  # Combine process data
  processed_data <- list(
    accident = st_as_sf(
      cbind(key_data, accident_data),
      coords = c("longitude", "latitude"),
      crs = 4326
    ),
    person = rbind(
      cbind(key_data, person_a_data),
      cbind(key_data, person_b_data)
    )
  )

  # Set dataset name attribute
  attr(processed_data, "dataset_name") <- attr(accident_data, "dataset_name")

  return(processed_data)
}

#' @rdname post_process
#' @export
post_process_sub <- function(data) {
  return(data)
}

#' @rdname post_process
#' @export
post_process_highway <- function(data) {
  return(data)
}
