test_that("`download_accident_data` works with valid inputs", {
  local_mocked_bindings(download.file = function(url, destfile) {
    file.copy(mock_accident_data_path, destfile, overwrite = TRUE)
    return(destfile)
  })

  # Test with "main" data type
  downloaded_file_path <- suppressMessages(
    download_accident_data("main", tempdir(), 2022)
  )

  ## --DEBUG BEGIN--
  cat("Tempdir: ", tempdir(), "\n")
  cat("Downloaded file path: ", downloaded_file_path, "\n")
  ## --DEBUG END--

  expect_true(file.exists(downloaded_file_path))
  expect_match(downloaded_file_path, "honhyo_2022[.]csv$")

  # Test with "sub" data type
  downloaded_file_path <- suppressMessages(
    download_accident_data("sub", tempdir(), 2021)
  )
  expect_true(file.exists(downloaded_file_path))
  expect_match(downloaded_file_path, "hojuhyo_2021[.]csv$")

  # Test with "highway" data type
  downloaded_file_path <- suppressMessages(
    download_accident_data("highway", tempdir(), 2020)
  )
  expect_true(file.exists(downloaded_file_path))
  cat(downloaded_file_path)
  expect_match(downloaded_file_path, "kosokuhyo_2020[.]csv$")
})

test_that("`download_accident_data` handles invalid data types", {
  expect_error(download_accident_data("invalid", tempdir(), 2022),
               "Invalid type. Please specify one of: main, sub, highway")
})

test_that("`download_accident_data` handles invalid years", {
  expect_error(download_accident_data("main", tempdir(), 2018),
               "The year must be between 2019 and 2023.")
  expect_error(download_accident_data("main", tempdir(), 2024),
               "The year must be between 2019 and 2023.")
})
