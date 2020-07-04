source(system.file("utils", "general_utils.R", package = "ibiVizEdit"))

test_that("set_file_size_max: returns max file size memory as string", {
  expected <- "200 MB"
  observed <- set_file_size_max(200)
  expect_equal(observed, expected)
})

test_that("set_file_size_max: defaults to 500 MB", {
  observed <- set_file_size_max()
  expect_equal(observed, "500 MB")
})

test_that("range01: returns series in which min=0 and max=1", {
  series <- -1000:1000
  transformed_series <- range01(series)

  expect_equal(max(transformed_series), 1)
  expect_equal(min(transformed_series), 0)
})

test_that("range01: preserves NA values", {
  series <- c(NA, -1000:1000, NA)
  transformed_series <- range01(series)

  expect_equal(max(transformed_series, na.rm = TRUE), 1)
  expect_equal(min(transformed_series, na.rm = TRUE), 0)
  expect_equal(sum(is.na(transformed_series)), 2)
})

test_that("estimate_max_density: returns valid approximation of max density", {
  x <- c(rep(1, 3), rep(2, 9), rep(3, 27), rep(4, 9), rep(5, 3))
  x_max_dens <- estimate_max_density(x)

  expect_lte(abs(x_max_dens- 3), .01)
})

test_that("estimate_average_HR: returns valid average", {
  test_data <- read.csv("../testdata/clean_ibi_example.csv")

  # Need to account for default trimming:
  expected <- 1/mean(test_data[["IBI"]][3:(nrow(test_data)-3)])*60
  result <- estimate_average_HR(ibi_data = test_data)

  expect_equal(result, expected)
})

