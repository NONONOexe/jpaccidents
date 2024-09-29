#' Download the traffic accident data
#'
#' Download the file of traffic accident data provided
#' [the National Police Agency's Web page](https://www.npa.go.jp/publications/statistics/koutsuu/opendata/index_opendata.html).
#'
#' The traffic accident data are divided into the following types:
#'   * "main" (honhyo): Basic data on traffic accidents.
#'   * "sub" (hojuhyo): Additional data related to the "main" data.
#'   * "highway" (kosokuhyo): Data on traffic accidents on highways.
#'
#' You can specify the type of data directly via the `type` argument.
#'
#' @param type Type of data to download: one of "main", "sub", or "highway".
#' @param download_dir A directory where downloaded file is to saved
#' (default: `getwd()`).
#' @param year Year the traffic accident data was recorded.
#' Available from 2019 to 2023 (default: `2023`).
#' @return Path of the downloaded file (invisibly).
#' @export
#' @examples
#' \dontrun{
#' download_accident_data("main", "download-dir-path", 2023)
#' download_accident_data("sub", "download-dir-path", 2023)
#' download_accident_data("highway", "download-dir-path", 2023)
#' }
download_accident_data <- function(type, download_dir = getwd(), year = 2023) {

  # Mapping of type to actual file name part
  type_map <- c(main = "honhyo", sub = "hojuhyo", highway = "kosokuhyo")

  # Validation
  valid_types <- names(type_map)
  if (!type %in% valid_types) {
    stop("Invalid type. Please specify one of: ",
         paste(valid_types, collapse = ', '))
  }

  if (year < 2019 || 2023 < year) {
    stop("The year must be between 2019 and 2023.")
  }

  # Use string manipulation to create the URL
  page <- "https://www.npa.go.jp/publications/statistics/koutsuu/opendata"
  url <- paste0(page, "/", year, "/", type_map[[type]], "_", year, ".csv")

  # Create destination file path
  destfile <- file.path(download_dir, basename(url))

  # Download
  message("Downloading the file to: ", destfile)
  downloaded_file_path <- download.file(url, destfile)
  message("Downloaded ", destfile, " successfly!")

  return(invisible(downloaded_file_path))
}
