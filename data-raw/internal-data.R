# Pre-process internal data for the jpaccidents package

# Load column name mapping
column_name_map <-
  yaml::read_yaml(here::here("data-raw", "column-name-map.yaml")) %>%
  purrr::pluck("column_names") %>%
  purrr::map(\(name_map) {
    transformed_name <- name_map$transformed
    original_names <- name_map$original
    setNames(
      rep(transformed_name, length(original_names)),
      original_names
    )
  }) %>%
  purrr::reduce(c)

# Load traffic accident data schema
traffic_accident_schema <-
  yaml::read_yaml(here::here("data-raw", "traffic-accident-schema.yaml")) %>%
  purrr::pluck("data_schema")

# Extract primary key columns for accident data
key_columns <- traffic_accident_schema$accident_info$primary_key

# Extract integer fields
integer_fields <- traffic_accident_schema %>%
  purrr::map(\(data_schema) {
    data_schema %>%
      purrr::pluck("columns") %>%
      purrr::keep(\(column_type) column_type == "integer") %>%
      names()
  }) %>%
  purrr::reduce(c)

# Extract empty data frames based on the schema
data_frames <- traffic_accident_schema %>%
  purrr::map(\(data_schema) {
    # Define data types for each column
    data_frame <- data_schema %>%
      purrr::pluck("columns") %>%
      purrr::map(
        \(column)
        switch (
          column,
          string   = character(),
          integer  = integer(),
          datetime = as.POSIXct(character()),
          geometry = sf::st_sfc(crs = 4326),
          character()
        )
      ) %>%
      tibble::as_tibble()
  })
accident_info_frame <- sf::st_sf(data_frames$accident_info)
person_info_frame <- data_frames$person_info
highway_info_frame <- data_frames$highway_info

# Extract column names from each data frame
accident_info_columns <- names(accident_info_frame)
person_info_columns   <- names(person_info_frame)
highway_info_columns  <- names(highway_info_frame)

# Load code-label mapping
code_label_map <-
  yaml::read_yaml(here::here("data-raw", "code-label-map.yaml")) %>%
  purrr::pluck("categories") %>%
  purrr::map(\(category) {
    list(
      labels = category %>%
        purrr::map(\(concept) {
          # Map codes to their corresponding labels
          codes <- concept$code
          label <- concept$label
          setNames(
            rep(label, length(codes)),
            codes
          )
        }) %>%
        purrr::reduce(c)
    )
  })

# Save the processed data internally within the jpaccidents package
usethis::use_data(
  column_name_map,
  key_columns,
  integer_fields,
  accident_info_frame,
  person_info_frame,
  highway_info_frame,
  accident_info_columns,
  person_info_columns,
  highway_info_columns,
  code_label_map,
  overwrite = TRUE,
  internal = TRUE
)
