#' Find dataset name from file path
#'
#' This function determines the dataset name based on the file path and
#' configuration.
#'
#' @param file A character string specifying the file path.
#' @return Dataset name
#' @export
#' @examples
#' lookup_dataset_name("example/honhyo_2023.csv")
lookup_dataset_name <- function(file) {
  # Get configuration
  config <- get_config()

  # Determine dataset name based on file pattern
  dataset_name <- names(config)[
    sapply(config, function(x) grepl(x$file_pattern, file))
  ]
  if (is.null(dataset_name)) {
    stop("Unknown dataset for file: ", dataset_name)
  }

  return(dataset_name)
}
