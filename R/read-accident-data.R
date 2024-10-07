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
    post_process = select_post_processor(find_dataset_name(file))
  ) {

  # Get dataset name and configuration
  dataset_name <- find_dataset_name(file)
  config <- get_config()[[dataset_name]]$columns

  # Read file
  accident_data <- read.csv(
    file,
    fileEncoding     = "Shift_JIS",
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )

  # Rename columns
  original_names <- sapply(config, `[[`, "original_name")
  col_names_match <- match(names(accident_data), original_names)
  names(accident_data)[!is.na(col_names_match)] <-
    names(original_names)[col_names_match[!is.na(col_names_match)]]

  # Add attributes and class
  attr(accident_data, "dataset_name") <- dataset_name
  class(accident_data) <- c("accident_data", class(accident_data))

  # Apply post-processing if provided
  if (is.function(post_process)) {
    accident_data <- post_process(accident_data)
  }

  return(accident_data)
}
