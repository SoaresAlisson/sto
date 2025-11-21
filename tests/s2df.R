library(testthat)
# library(tidyverse)

test_that("s2df correctly converts a CSV-like string into a tibble", {
  # Test case 1: Basic functionality
  input_string <- "col1;col2\nbla1;10\nbla2;23\nbla3;32"
  expected_output <- tibble::tribble(
    ~col1  , ~col2 ,
    "bla1" ,    10 ,
    "bla2" ,    23 ,
    "bla3" ,    32
  )
  # expected_output <- data.frame(
  #   col1 = c("bla1", "bla2", "bla3"),
  #   col2 = c(10, 23, 32)
  # )

  result <- s2df(input_string)
  # TODO tibble and data types
  expect_equal(result, expected_output)

  # Test case 2: No header provided
  input_string_no_header <- "bla1;10\nbla2;23\nbla3;32"
  expected_output_no_header <- tibble::tribble(
    ~V1    , ~V2 ,
    "bla1" ,  10 ,
    "bla2" ,  23 ,
    "bla3" ,  32
  )

  result_no_header <- s2df(input_string_no_header, header = FALSE)
  expect_equal(result_no_header, expected_output_no_header)

  # Test case 3: Different separator
  input_string_different_sep <- "col1,col2\nbla1,10\nbla2,23\nbla3,32"
  expected_output_different_sep <- tibble::tribble(
    ~col1  , ~col2 ,
    "bla1" ,    10 ,
    "bla2" ,    23 ,
    "bla3" ,    32
  )

  result_different_sep <- s2df(input_string_different_sep, sep = ",")
  expect_equal(result_different_sep, expected_output_different_sep)

  # Test case 4: Empty input string
  expect_error(s2df(""))
})

# Run the tests
# test_file("path_to_your_test_script.R") # Replace with the actual path to your test script
