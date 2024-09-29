test_that("`convert_deg` works with valid input", {
  dms <- c("431007628", "1410328320")
  expect_equal(convert_deg(dms), c(43.168786, 141.057867))
})

test_that("`convert_deg` handles invalid input", {
  dms <- "12345678901"
  expect_warning(convert_deg(dms),
                 "The input must be a number consisting of 8 to 10 digits.")
  expect_equal(suppressWarnings(convert_deg(dms)), NA_real_)

  dms <- c("431007628", "12345678901")
  expect_warning(convert_deg(dms),
                 "The input must be a number consisting of 8 to 10 digits.")
  expect_equal(suppressWarnings(convert_deg(dms)), c(43.168786, NA))
})

test_that("`convert_deg` handles invalid minutes and seconds", {
  dms <- "437007628"
  expect_warning(convert_deg(dms), "Minutes must be between 0 and 60.")
  expect_equal(suppressWarnings(convert_deg(dms)), NA_real_)

  dms <- "431070628"
  expect_warning(convert_deg(dms), "Seconds must be between 0 and 60.")
  expect_equal(suppressWarnings(convert_deg(dms)), NA_real_)
})
