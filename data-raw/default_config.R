library(jpaccidents)
library(here)
library(yaml)
library(usethis)

config_path <- get_default_config_path()
default_config <- read_yaml(config_path)
class(default_config) <- "jpaccidents_config"

use_data(default_config, overwrite = TRUE, internal = TRUE)
