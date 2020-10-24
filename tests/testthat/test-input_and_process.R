# Testing functionality of initial importing and processing of inputs
TIMING_FILEPATH_TXT <- "../testdata/timing_file_example.txt"
TIMING_FILEPATH_CSV <- "../testdata/timing_file_example.csv"

test_that("load_timing_data: can load a csv file", {
  res <- load_timing_data(TIMING_FILEPATH_CSV, 'only_numeric')
  expected <- data.frame(case_id = 'only_numeric', 
                         t1_start = 5, 
                         t1_stop = 45, 
                         t2_start = 50, 
                         t2_stop = 100, 
                         t3_start = 105, 
                         t3_stop = 165)
  
  expect_equal(res, expected)
})

test_that("load_timing_data: can load a tab-delimited txt file", {
  res <- load_timing_data(TIMING_FILEPATH_TXT, 'only_numeric')
  expected <- data.frame(case_id = 'only_numeric', 
                         t1_start = 5, 
                         t1_stop = 45, 
                         t2_start = 50, 
                         t2_stop = 100, 
                         t3_start = 105, 
                         t3_stop = 165)
  
  expect_equal(res, expected)
})

test_that("load_timing_data: raises if timing cannot be coerced to numeric", {
  expect_warning(load_timing_data(TIMING_FILEPATH_TXT, 'numeric_uncoerceable'))
  expect_warning(load_timing_data(TIMING_FILEPATH_CSV, 'numeric_uncoerceable'))
})

test_that("load_timing_data: raises if numeric values contained in quotes", {
  expect_warning(load_timing_data(TIMING_FILEPATH_TXT, 'numeric_string'))
  expect_warning(load_timing_data(TIMING_FILEPATH_TXT, 'numeric_string'))
})