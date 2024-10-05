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
  config <- get_config()
  dataset_name <- attr(data, "dataset_name")
  all_cols <- config[[dataset_name]]$columns

  selected_cols <- names(all_cols)[sapply(all_cols, `[[`, "tag") %in% tag]

  return(data[, selected_cols, drop = FALSE])
}
