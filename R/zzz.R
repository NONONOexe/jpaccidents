.onLoad <- function(libname, pkgname) {
  tryCatch({
    reset_config()
  }, error = function(e) {
    warning(
      "Configuration file could not be loaded. ",
      "Please check the config path."
    )
  })
}
