#' Lookup new column names for an accident dataset
#'
#' This function retreieves the new column names corresponding to
#' the original column names in a specified dataset. The column
#' names are based on the configuration loaded by `load_config()`.
#' This is useful when the original names are in Japanese (2-byte
#' characters), which can be difficult to handle in programs.
#' This function converts them to corresponding English names.
#'
#' @param dataset_name The name of the dataset.
#' @param original_names A vector of original column names.
#' @return A vector of new column names corresponding to the provided
#'   original names.
#' @export
#' @examples
#' lookup_column_names("main_data", c("死者数", "天候"))
lookup_column_names <- function(dataset_name, original_names) {
  # Retrieve column configuration for the specified dataset
  column_config <- get_config()[[dataset_name]]$columns

  # Check if column configuration is available
  if (is.null(column_config)) {
    warning("No column configuration found for dataset: ", dataset_name)
    return(original_names)
  }

  # Extract original names and their corresponding new names
  original_names_list <- sapply(column_config, `[[`, "original_name")
  new_names <- setNames(
    rep(names(original_names_list), lengths(original_names_list)),
    unlist(original_names_list, use.names = FALSE)
  )

  # Return the new names corresponding to the provided original names
  return(unname(new_names[original_names]))
}
