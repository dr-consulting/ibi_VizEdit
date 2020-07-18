# Tests the core functions used to support all imputation approaches
source("../testdata/testing_data_utils.R")

test_that("generate_model_ppg_inputs: returns a properly formatted data.frame", {
  time_min <- 24
  time_max <- 96
  hr_freq <- 90/60
  sample_rate <- 1000
  ppg_data <- create_sim_ppg(sample_rate, time_min, time_max, hr_freq)
  
  input_windows <- list(pre = c(44,48), post = c(60, 64))
  
  result <- generate_model_ppg_inputs(time_min = 52, time_max = 56, ppg_data = ppg_data, ds = sample_rate, 
                                      input_windows = input_windows)
  
  # Tests
  expect_s3_class(result, "data.frame")  # returns a data.frame
  expect_equal(colnames(result), c("Time", "PPG"))  # has the correct column names
  
  # subracting 1 and then adjusting the final result by subtraciting 1 adjusts for default inclusive beavhior 
  n_rows_pre <- nrow(ppg_data[dplyr::between(ppg_data[["Time"]], input_windows$pre[1], input_windows$pre[2]), ]) - 1
  n_rows_post <- nrow(ppg_data[dplyr::between(ppg_data[["Time"]], input_windows$post[1], input_windows$post[2]), ]) - 1
  expect_equal(nrow(result)-2, (n_rows_pre + n_rows_post)/(sample_rate/10))
})