#' Download the traffic accident data
#'
#' @description
#' Download the file of traffic accident data provided
#' the National Police Agency's Web page
#' (https://www.npa.go.jp/publications/statistics/koutsuu/opendata/index_opendata.html).
#'
#' The traffic accident data are divided into the following types:
#'
#'   * main (honhyo; use `download_accident_main_data()`)
#'   * sub (hojuhyo; use `download_accident_sub_data()`)
#'   * highway (kosokuhyo; use `download_accident_highway_data()`
#'
#' Please use the function corresponding to the type of data
#' you want to download.
#'
#' @param download_dir A directory where downloaded file is to saved
#' (default: `getwd()`).
#' @param year Year the traffic accident data was recorded.
#' Available from 2019 to 2022 (default: `2022`).
#' @return Path of the downloaded file (invisibly).
#' @export
#' @examples
#' \dontrun{
#' download_accident_main_data("download-dir-path", 2021)
#' download_accident_sub_data("download-dir-path", 2021)
#' download_accident_highway_data("download-dir-path", 2021)
#' }
#' @name download_accident_data
NULL

#' @rdname download_accident_data
#' @export
download_accident_main_data <- function(download_dir = getwd(), year = 2022) {
  download_accident_data("honhyo", download_dir, year)
}

#' @rdname download_accident_data
#' @export
download_accident_sub_data <- function(download_dir = getwd(), year = 2022) {
  download_accident_data("hojuhyo", download_dir, year)
}

#' @rdname download_accident_data
#' @export
download_accident_highway_data <- function(download_dir = getwd(), year = 2022) {
  download_accident_data("kosokuhyo", download_dir, year)
}

download_accident_data <- function(type, download_dir, year) {
  if (year < 2019 || 2022 < year) {
    cli_alert_danger("The year must be between 2019 and 2022.")
    return(invisible(NULL))
  }
  page <- "https://www.npa.go.jp/publications/statistics/koutsuu/opendata"
  url <- str_glue("{page}/{year}/{type}_{year}.csv")
  destfile <- file.path(download_dir, basename(url))

  cli_progress_step("Downloading the file {.path {destfile}}",
                    msg_done = "Downloaded the file {.path {destfile}}")
  downloaded_file_path <- curl_download(url, destfile)

  return(invisible(downloaded_file_path))
}
