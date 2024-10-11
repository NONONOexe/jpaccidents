#' Filter data by tag
#'
#' This function filters data by a specific tag.
#'
#' @param data An accident data object.
#' @param tag A character string specifying the tag to filter by.
#' @return A data frame containing only the columns with the specified tag.
#' @export
#' @examples
#' \dontrun{
#' person_data <- filter_data_by_tag(accident_data, "person")
#' }
filter_data_by_tag <- function(data, tag) {
  # Get all columns for the dataset registered in the configuration
  config <- get_config()
  dataset_name <- attr(data, "dataset_name")
  all_columns <- config[[dataset_name]]$columns

  # Get the columns having the specified tag
  tag_columns <- names(all_columns[sapply(all_columns, `[[`, "tag") %in% tag])

  # Select column from the data that match the tag columns
  selected_cols <- names(data) %in% tag_columns

  # Return the filtered data
  return(data[, selected_cols, drop = FALSE])
}
