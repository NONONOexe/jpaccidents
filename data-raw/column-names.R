source("data-raw/column-name-main.R")
source("data-raw/column-name-sub.R")
source("data-raw/column-name-highway.R")

usethis::use_data(
  column_name_main,
  column_name_sub,
  column_name_highway,
  internal = TRUE, overwrite = TRUE
)
