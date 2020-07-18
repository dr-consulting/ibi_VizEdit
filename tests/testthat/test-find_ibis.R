# Testing functionality of peak detection algorithm support utilities
source("../testdata/testing_data_utils.R")

test_that("find_ibis: a properly formatted list of data.frames simulated data", {
  # test stub
  sampling_rate <- 1000
  min_time <- -1 
  max_time <- 10 
  hr_freq <- 80/60
  
  sim_ppg <- create_sim_ppg(sampling_rate, min_time, max_time, hr_freq)
  ibi_out <- find_ibis(sim_ppg$PPG, sampling_rate, min_time, time_adjust = 0, peak_iter = 200)
  
  # Basic list properties
  expect_type(ibi_out, "list")
  expect_equal(names(ibi_out), c("IBI_out", "detection_settings"))
  
  # Test that each list element is a data.frame
  expect_s3_class(ibi_out[[1]], "data.frame")
  expect_s3_class(ibi_out[[2]], "data.frame")
  
  # Test each data.frame contains correct colnames
  expect_equal(colnames(ibi_out[[1]]), c("IBI", "Time"))
  expect_equal(colnames(ibi_out[[2]]), c("BW", "SD", "Range", "RMSSD", "AC", "BW(s)"))
  
  # Returns correct IBIs
  expected <- c(.25, rep(.75, 14))
  expect_equal(ibi_out[[1]]$IBI, expected)
  
  # Returns correct Time
  expected <- seq(-.75, 9.75, by=.75)
  expect_equal(ibi_out[[1]]$Time, expected)
})

test_that("sum_rev: returns cumulative sum with fixed value at x[1]", {
  result <- sum_rev(1:10)
  expected <- c(1, 3, 6, 10, 15, 21, 28, 36, 45, 55)
  
  # Basically just a cumulative sum
  expect_equal(result, expected)
})

test_that("time_diff: returns a running difference with fixed value at x[1]", {
  result <- time_diff(c(1, 1.5, 2.75, 3))
  expected <- c(1, .5, 1.25, .25)

  # Basically a running difference   
  expect_equal(result, expected)
})
