#' Read accident data from CSV files
#'
#' This function reads accident data from multiple CSV files,
#' processes each file, and combines the data into three categories:
#'   * `accidents_info`
#'   * `persons_info`
#'   * `highways_info`
#'
#' @param file_paths A character vector of file paths to the CSV files.
#' @return A list containing the accident data split into three data frames:
#'   `accidents_info`, `persons_info`, and `highways_info`.
#' @export
#' @examples
#' \dontrun{
#' read_accidents_data(c(
#'   "example/honhyo_2023.csv",
#'   "example/hojuhyo_2023.csv"
#' ))
#' }
read_accidents_data <- function(file_paths) {
  # Process all files and extract valid data
  data_list <- lapply(file_paths, process_single_file)
  valid_data <- Filter(Negate(is.null), data_list)
  skipped_files <- file_paths[vapply(data_list, is.null, logical(1))]

  # Warn about skipped files
  if (0 < length(skipped_files)) {
    cli_alert_warning("The following files were skipped because their format is not supported: {skipped_files}")
  }

  # Post-process valid data
  processed_data <- lapply(valid_data, post_process)

  # Extract data categories
  accidents_info <- combine_data(processed_data, "accidents_info")
  persons_info <- combine_data(processed_data, "persons_info")
  highways_info <- combine_data(processed_data, "highways_info")

  return(list(
    accidents_info = accidents_info,
    persons_info   = persons_info,
    highways_info  = highways_info
  ))
}

# Function to process a single file
process_single_file <- function(file) {
  dataset_name <- detect_dataset_name(file, quiet = TRUE)
  if (is.na(dataset_name)) {
    return(NULL)
  }

  # Read configuration and file with proper encoding
  config <- get_config()
  encoding <- config$file_types[[dataset_name]]$encoding
  accident_data <- read_csv(
    file,
    col_types      = cols(.default = "c"),
    locale         = locale(encoding = encoding),
    show_col_types = FALSE,
  )

  # Rename columns and add attributes
  accident_data <- rename_columns(accident_data)
  attr(accident_data, "file_path") <- file
  attr(accident_data, "dataset_name") <- dataset_name
  class(accident_data) <- c("accident_data", class(accident_data))

  return(accident_data)
}

# Rename columns using lookup table
rename_columns <- function(data) {
  colnames(data) <- lookup_column_names(names(data))
  return(data)
}

# Combine data based on dataset name and attribute
combine_data <- function(data_list, schema_name) {
  filtered_data <- Filter(
    Negate(is.null),
    lapply(data_list, function(x) {
      if (is.data.frame(x) && attr(x, "schema_name") == schema_name) {
        return(x)
      } else {
        return(x[[schema_name]])
      }
      NULL
    })
  )
  return(do.call(bind_rows, filtered_data))
}
