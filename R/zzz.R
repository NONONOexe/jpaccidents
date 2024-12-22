.onLoad <- function(libname, pkgname) {
  tryCatch({
    option(encoding = "UTF-8") # For R 4.0 (Windows)
    reset_config()
  }, error = function(e) {
    warning(
      "Configuration file could not be loaded. ",
      "Please check the config path."
    )
  })
}
