#' Download the traffic accident data
#'
#' `download_accident_data()` downloads the file of traffic accident data
#' provided the [National Police Agency](https://www.npa.go.jp/publications/statistics/koutsuu/opendata/index_opendata.html).
#'
#' The traffic accident data are divided into the following types:
#'   * `"main"`    : Basic data on traffic accidents (honhyo).
#'   * `"supp"`    : Additional data related to the `"main"` data (hojuhyo).
#'   * `"highway"` : Data on traffic accidents on highways (kosokuhyo).
#'
#' Use the `type` argument to specify which data type to download.
#'
#' @param type A character string specifying the type of data to download:
#'   one of "main", "supp", or "highway".
#' @param download_dir A character string specifying the directory to save the
#'   downloaded files (default: `getwd()`).
#' @param years An integer or vector of integers specifying the years of the
#'   data to download. Available years are 2019 to 2023 (default: `2023`).
#' @return A character vector of file paths for the downloaded files
#'   (invisibly).
#' @export
#' @examples
#' \dontrun{
#' # Download the main data for 2023
#' download_accident_data("main", "download-dir-path", 2023)
#'
#' # Download the supplementary data for 2023
#' download_accident_data("supp", "download-dir-path", 2023)
#'
#' # Download the highway data for 2019 to 2023
#' download_accident_data("highway", "download-dir-path", 2019:2023)
#' }
download_accident_data <- function(type, download_dir = getwd(), years = 2023) {
  # Mapping types to file names
  type_map <- c(main = "honhyo", supp = "hojuhyo", highway = "kosokuhyo")

  # Validate the input type
  valid_types <- names(type_map)
  if (!type %in% valid_types) {
    cli::cli_abort("Invalid type. Please specify one of: {valid_types}.")
  }

  # Validate the input years
  if (any(years < 2019 | 2023 < years)) {
    invalid_years <- years[years < 2019 | 2023 < years]
    cli::cli_abort("The `years` must be between 2019 and 2023. Invalid year(s) provided: {invalid_years}.")
  }

  # Construct URLs for downloading
  page <- "https://www.npa.go.jp/publications/statistics/koutsuu/opendata"
  urls <- stringr::str_glue("{page}/{years}/{type_map[[type]]}_{years}.csv")

  # Log the download URLs
  cli::cli_alert_info("Downloading files from the following URL:")
  cli::cli_ul(urls)

  # Create destination file path
  destination_files <- file.path(download_dir, basename(urls))

  # Download the files
  curl::multi_download(urls, destination_files)
  cli::cli_alert_success(c(
    "Successfully downloaded {length(destination_files)} file(s) to ",
    "{download_dir}"
  ))

  return(invisible(destination_files))
}
