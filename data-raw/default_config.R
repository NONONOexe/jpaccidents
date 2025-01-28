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

# Load traffic accident schema
traffic_accident_schema <-
  yaml::read_yaml(here::here("data-raw", "traffic-accident-schema.yaml")) %>%
  purrr::pluck("data_schema")

# Extract specific column groups from the schema
accident_info_columns <- traffic_accident_schema$accident_info
person_info_columns   <- traffic_accident_schema$person_info
highway_info_columns  <- traffic_accident_schema$highway_info

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

# Save the processed data internally within the package
usethis::use_data(
  column_name_map,
  accident_info_columns,
  person_info_columns,
  highway_info_columns,
  code_label_map,
  overwrite = TRUE,
  internal = TRUE
)
