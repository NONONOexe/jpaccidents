library(here)
library(yaml)
library(usethis)

config_path <- here("data-raw", "jpaccidents-config.yaml")
default_config <- read_yaml(config_path)
attr(default_config, "path") <- config_path
class(default_config) <- "jpaccidents_config"

use_data(default_config, overwrite = TRUE, internal = TRUE)
