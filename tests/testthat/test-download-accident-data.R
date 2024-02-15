test_that("download accident main data", {
  expect_no_error({
    suppressMessages(download_accident_main_data())
  })
})

test_that("download accident sub data", {
  expect_no_error({
    suppressMessages(download_accident_sub_data())
  })
})

test_that("download accident highway data", {
  expect_no_error({
    suppressMessages(download_accident_highway_data())
  })
})
