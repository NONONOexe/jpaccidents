#' Filter data by schema
#'
#' This function filters a data frame based on a specified schema, which
#' defines the columns to keep. It also handles special case where data
#' needs to be reshaped to long format before filtering.
#'
#' @param data An accident data object.
#' @param schema_name A character string specifying the schema to use for
#'   filtering.
#' @return A data frame containing only the columns defined in the schema, or
#'   `NULL`.
#' @export
#' @examples
#' \dontrun{
#' persons_info <- extract_schema_columns(accident_data, "persons_info")
#' }
extract_schema_columns <- function(data, schema_name) {
  # Load configuration settings containing to access schema definitions
  config <- get_config()
  schema <- config$schemas[[schema_name]]

  # Validate schema existence
  if (is.null(schema)) {
    cli_alert_danger("The schema does not exist: {schema_name}")
    return(NULL)
  }

  # Retrieve dataset name and validate against schema's supported files
  dataset_name <- attr(data, "dataset_name")
  source_files <- schema$source_files

  if (!dataset_name %in% source_files) {
    cli_alert_warning("This schema does not support the current dataset: dataset_name")
    return(NULL)
  }

  # Handle specific transformation requirements for certain schemas
  if (dataset_name == "main_data" && schema_name == "persons_info") {
    data <- pivot_longer(
      data,
      cols          = ends_with(c("_a", "_b")),
      names_to      = c(".value", "sub_id"),
      names_pattern = "^(.+)[_]([a|b])$"
    )
  }

  # Extract columns based on schema's primary key and data columns
  schema_columns <- c(schema$primary_key, schema$data_columns)
  selected_columns <- names(data) %in% schema_columns
  filtered_data <- data[, selected_columns, drop = FALSE]

  # Attach schema name to the resulting data
  attr(filtered_data, "schema_name") <- schema_name

  return(filtered_data)
}
