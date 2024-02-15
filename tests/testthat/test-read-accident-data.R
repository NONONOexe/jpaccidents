test_that("read accident main data", {
  expect_no_error({
    read_accident_main_data("honhyo_2022.csv")
  })
})

test_that("read accident sub data", {
  expect_no_error({
    read_accident_sub_data("hojuhyo_2022.csv")
  })
})

test_that("read accident highway data", {
  expect_no_error({
    read_accident_highway_data("kosokuhyo_2022.csv")
  })
})
