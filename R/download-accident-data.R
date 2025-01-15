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
#' @param years Years the traffic accident data were recorded.
#' Available from 2019 to 2023. You can specify a single year or
#' vector of years(default: `2023`).
#' @return Path of the downloaded file (invisibly).
#' @export
#' @examples
#' \dontrun{
#' download_accident_data("main", "download-dir-path", 2023)
#' download_accident_data("sub", "download-dir-path", 2023)
#' download_accident_data("highway", "download-dir-path", 2023)
#' }
download_accident_data <- function(type, download_dir = getwd(), years = 2023) {

  # Mapping of type to actual file name part
  type_map <- c(main = "honhyo", sub = "hojuhyo", highway = "kosokuhyo")

  # Validation
  valid_types <- names(type_map)
  if (!type %in% valid_types) {
    cli_abort("Invalid type. Please specify one of: {valid_types}.")
  }

  if (any(years < 2019 | years > 2023)) {
    invalid_years <- years[years < 2019 | years > 2023]
    cli_abort("The `years` must be between 2019 and 2023. Invalid year(s) provided: {invalid_years}.")
  }

  # Use string manipulation to create the URL
  page <- "https://www.npa.go.jp/publications/statistics/koutsuu/opendata"
  urls <- paste0(page, "/", years, "/", type_map[[type]], "_", years, ".csv")

  # Create destination file path
  destfiles <- file.path(download_dir, basename(urls))

  # Download
  downloaded_file_path <- multi_download(urls, destfiles)
  cli_alert_success("Successfully downloaded {length(destfiles)} files to {download_dir}")

  return(invisible(destfiles))
}
