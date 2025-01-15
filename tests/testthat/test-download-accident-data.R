test_that("`download_accident_data` works with valid inputs", {
  local_mocked_bindings(multi_download = function(urls, destfiles) {
    file.copy(mock_accident_data_path, destfiles, overwrite = TRUE)

    return(destfiles)
  })

  # Test with "main" data type
  downloaded_file_path <- suppressMessages(
    download_accident_data("main", tempdir(), 2022)
  )

  expect_true(file.exists(downloaded_file_path))
  expect_match(downloaded_file_path, "honhyo_2022[.]csv$")

  # Test with "sub" data type
  downloaded_file_path <- suppressMessages(
    download_accident_data("sub", temp_dir, 2021)
  )
  expect_true(file.exists(downloaded_file_path))
  expect_match(downloaded_file_path, "hojuhyo_2021[.]csv$")

  # Test with "highway" data type
  downloaded_file_path <- suppressMessages(
    download_accident_data("highway", temp_dir, 2020)
  )
  expect_true(file.exists(downloaded_file_path))
  cat(downloaded_file_path)
  expect_match(downloaded_file_path, "kosokuhyo_2020[.]csv$")
})

test_that("`download_accident_data` handles invalid data types", {
  expect_error(
    download_accident_data("invalid", temp_dir, 2022),
    "Invalid type. Please specify one of: main, sub, and highway."
  )
})

test_that("`download_accident_data` handles invalid years", {
  expect_error(
    download_accident_data("main", temp_dir, 2018),
    "The `years` must be between 2019 and 2023. Invalid year\\(s\\) provided: 2018."
  )
  expect_error(
    download_accident_data("main", temp_dir, 2024),
    "The `years` must be between 2019 and 2023. Invalid year\\(s\\) provided: 2024."
  )
})
