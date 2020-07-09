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

test_that("estimate_average_HR: ignores uneditable data points", {
  test_data <- read.csv("../testdata/clean_ibi_example.csv")

  uneditable_data <- data.frame(IBI = rep(600, 3),
                                Time = rep(5000, 3),
                                pnt_type = rep("Uneditable", 3))

  test_data_w_uneditable <- rbind(test_data, uneditable_data)
  expected <- estimate_average_HR(ibi_data = test_data, trim = NULL)
  result <- estimate_average_HR(ibi_data = test_data_w_uneditable, trim = NULL)

  expect_equal(result, expected)
})

test_that("estimate_avg_respiration: returns estimated mean and sd for resp", {
  test_data <- read.csv("../testdata/clean_ibi_example.csv")
  AVERAGE_RESPIRATION_BY_AGE <- list(`Adolescent (12 to 18 yrs)`=c(12,16)) # copied portion from global.R

  # manually calculated on the testdata file
  expected <- c(mean = 0.23887402, sd = 0.01311138)

  out_resp <- estimate_avg_respiration(ibi_data = test_data, respiration_cat = "Adolescent (12 to 18 yrs)",
                                       respiration_mapping = AVERAGE_RESPIRATION_BY_AGE)
  expect_equal(out_resp, expected)
  expect_equal(names(out_resp), names(expected))
})

test_that("create_and_return_output_dir: creates output directory and returns filepath", {
  home_dir <- Sys.getenv("HOME")
  tmp_dir <- paste0(home_dir, "/tmp_test")
  result <- create_and_return_output_dir(tmp_dir, "heart", "beat")

  expected <- paste0(tmp_dir, "/heart_beat_output")
  expect_equal(result, expected)
  expect_true(dir.exists(expected))

  # Remove the temporary directory at the end
  unlink(tmp_dir, recursive = TRUE)
  expect_false(dir.exists(tmp_dir)) # ensure the mini-teardown step works.
})

test_that("create_and_return_output_dir: creates output directory and returns filepath w/o optional id", {
  home_dir <- Sys.getenv("HOME")
  tmp_dir <- paste0(home_dir, "/tmp_test")
  result <- create_and_return_output_dir(tmp_dir, "heart")

  expected <- paste0(tmp_dir, "/heart_output")
  expect_equal(result, expected)
  expect_true(dir.exists(expected))

  # Remove the temporary directory at the end
  unlink(tmp_dir, recursive = TRUE)
  expect_false(dir.exists(tmp_dir)) # ensure the mini-teardown step works.
})

test_that("create_and_return_screenshot_dir: creates output directory and returns filepath", {
  home_dir <- Sys.getenv("HOME")
  tmp_dir <- paste0(home_dir, "/tmp_test")
  result <- create_and_return_screenshot_dir(tmp_dir)

  expected <- paste0(tmp_dir, "/screenshots")
  expect_equal(result, expected)
  expect_true(dir.exists(expected))

  # Remove the temporary directory at the end
  unlink(tmp_dir, recursive = TRUE)
  expect_false(dir.exists(tmp_dir)) # ensure the mini-teardown step works.
})

test_that("create_and_return_gp_output_subdir: ...", {
  home_dir <- Sys.getenv("HOME")
  tmp_dir <- paste0(home_dir, "/tmp_test")
  gp_driver <- list(prediction_window = c(100, 200))
  set_sys_time <- Sys.time()
  result <- create_and_return_gp_output_subdir(tmp_dir, gp_driver, set_sys_time)

  expected <- paste0(tmp_dir, "/GP_imputation_output_100_200_", set_sys_time)
  expect_equal(result, expected)
  expect_true(dir.exists(expected))

  # Remove the temporary directory at the end
  unlink(tmp_dir, recursive = TRUE)
  expect_false(dir.exists(tmp_dir)) # ensure the mini-teardown step works.
})

test_that("raise_not_in_range_integer: raises when value is not an integer", {
  expected <- "The input value of 2.5 for triathlons must be an integer"
  expect_warning(raise_not_in_range_integer (2.5, "triathlons"), expected)
  result <- suppressWarnings(raise_not_in_range_integer (2.5, "triathlons"))
  expect_equal(result, expected)
})

test_that("raise_not_in_range_integer: raises when value is out of bounds", {
  # raises & returns when value is not an integer and out of range
  expected <- "The input value of 2.5 for triathlons must be an integer between 3 and 5"
  expect_warning(raise_not_in_range_integer (2.5, "triathlons", 3, 5), expected)
  result <- suppressWarnings(raise_not_in_range_integer (2.5, "triathlons", 3, 5))
  expect_equal(result, expected)

  # raises & returns when the value is an integer and out of range
  expected <- "The input value of 2 for triathlons must be an integer between 3 and 5"
  expect_warning(raise_not_in_range_integer (2, "triathlons", 3, 5), expected)
  result <- suppressWarnings(raise_not_in_range_integer (2, "triathlons", 3, 5))
  expect_equal(result, expected)
})
