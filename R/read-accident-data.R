#' Read accident data from a CSV file
#'
#' This function reads accident data from a CSV file,
#' renames the columns, adds attributes and class, and applies
#' post-processing if provided.
#'
#' @param file The path to the CSV file.
#' @param post_process A function to apply post-processing to the data.
#' @return A data frame containing the accident data.
#' @export
#' @examples
#' \dontrun{
#' read_accident_data("example/honhyo_2023.csv")
#' }
read_accident_data <- function(
    file,
    post_process = select_post_processor(lookup_dataset_name(file))
  ) {

  # Read file
  accident_data <- read_csv(
    file,
    locale         = locale(encoding = "Shift_JIS"),
    show_col_types = FALSE
  )

  # Get dataset name and configuration
  dataset_name <- lookup_dataset_name(file)

  # Rename columns
  names(accident_data) <- lookup_column_names(
    dataset_name,
    names(accident_data)
  )

  # Add attributes and class
  attr(accident_data, "dataset_name") <- dataset_name
  class(accident_data) <- c("accident_data", class(accident_data))

  # Apply post-processing if provided
  if (is.function(post_process)) {
    accident_data <- post_process(accident_data)
  }

  return(accident_data)
}
