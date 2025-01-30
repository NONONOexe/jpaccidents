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
    cli::cli_abort("The {.code years} must be between 2019 and 2023. Invalid year(s): {invalid_years}.")
  }

  # Base URL for downloading
  base_url <- "https://www.npa.go.jp/publications/statistics/koutsuu/opendata"

  # Construct URLs for downloading
  urls <- stringr::str_glue("{base_url}/{years}/{type_map[[type]]}_{years}.csv")

  # Log the download URLs
  cli::cli_inform(c(i = "Downloading files from the following URL(s): {.url {urls}}."))

  # Create destination file path
  dest_files <- file.path(download_dir, basename(urls))

  # Download the files
  result <- curl::multi_download(urls, dest_files)

  # Extract successful and failed downloads
  success_files <- dest_files[result$status_code == 200]
  failed_urls <- urls[result$status_code != 200]

  # Log the results
  if (0 < length(success_files)) {
    cli::cli_inform(c("v" = "Successfully downloaded {length(success_files)} file(s) to {.path download_dir}."))
  }

  if (0 < length(failed_urls)) {
    cli::cli_abort(c("x" = "Failed to download from URL(s): {.url {failed_urls}}."))
  }

  return(invisible(dest_files))
}
