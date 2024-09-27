test_that("`download_accident_data` works with valid inputs", {
  local_mocked_bindings(curl_download = function(url, destfile) {
    file.copy(mock_accident_data_path, destfile, overwrite = TRUE)
    return(destfile)
  })

  # Test with "main" data type
  downloaded_file_path <- suppressMessages(
    download_accident_data("main", tempdir(), 2022)
  )
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
  expect_match(downloaded_file_path, "kosokuhyo_2020[.]csv$")
})

test_that("`download_accident_data` handles invalid data types", {
  expect_null(suppressMessages(
    download_accident_data("invalid", tempdir(), 2022)
  ))
})

test_that("`download_accident_data` handles invalid years", {
  expect_null(suppressMessages(
    download_accident_data("main", tempdir(), 2018)
  ))
  expect_null(suppressMessages(
    download_accident_data("main", tempdir(), 2024)
  ))
})
