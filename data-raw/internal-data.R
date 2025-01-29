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

data_frames <- traffic_accident_schema %>%
  purrr::map(\(column_types) {
    # Define data types for each column
    data_schema <- column_types %>%
      purrr::map(
        \(column_type)
        switch (
          column_type,
          string   = character(),
          integer  = integer(),
          datetime = as.POSIXct(character()),
          geometry = sf::st_sfc(crs = 4326),
          character()
        )
      ) %>%
      tibble::as_tibble()

    # Convert to sf object if geometry data is include
    if ("geometry" %in% column_types) sf::st_sf(data_schema) else data_schema
  })

# Extract integer fields
integer_fields <- traffic_accident_schema %>%
  purrr::map(\(column_types) {
    column_types %>%
      purrr::keep(\(column_type) column_type == "integer") %>%
      names()
  }) %>%
  purrr::reduce(c)

# Extract empty data frames based on the schema
accident_info_frame <- data_frames$accident_info
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
