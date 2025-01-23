.onLoad <- function(libname, pkgname) {
  # Reset settings to default
  reset_config()

  # To disable the progress bar during data loading
  options(readr.show_progress = FALSE)
}
