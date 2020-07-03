source("~/dr-consulting_GH/ibi_VizEdit/R/general_utils.R")

test_that("set_file_size_max: returns max file size memory as string", {
  expected <- "200 MB"
  observed <- set_file_size_max(200)
  expect_equal(observed, expected)
})

test_that("set_file_size_max: defaults to 500 MB", {
  observed <- set_file_size_max()
  expect_equal(observed, "500 MB")
})
