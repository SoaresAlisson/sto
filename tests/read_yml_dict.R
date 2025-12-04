# test_read_yml_dict.R

library(testthat)

# Source the function (adjust path as needed)
#source("path/to/your/function.R")

test_that("read_yml_dict basic functionality works", {
  # Test basic YAML structure
  yml_text <- "key1: value1\nkey2: value2"
  result <- read_yml_dict(yml_text)

  expect_type(result, "list")
  expect_named(result, c("key1", "key2"))
  expect_equal(result$key1, "value1")
  expect_equal(result$key2, "value2")
})

test_that("read_yml_dict handles multiple values per key", {
  # Test with multiple values per key (as shown in examples)
  dic_subs <- "WEF: World_Economic_Forum W\\.E\\.F\\.
NWO: New_World_Order N\\.W\\.O\\."

  result <- read_yml_dict(dic_subs)

  expect_named(result, c("WEF", "NWO"))
  expect_equal(result$WEF, c("World Economic Forum", "W\\.E\\.F\\."))
  expect_equal(result$NWO, c("New World Order", "N\\.W\\.O\\."))
})

test_that("read_yml_dict handles empty lines and whitespace", {
  # Test with empty lines and various whitespace
  yml_text <- "
key1: value1

key2: value2

key3: value3"

  result <- read_yml_dict(yml_text)

  expect_named(result, c("key1", "key2", "key3"))
  expect_equal(length(result), 3)
  expect_equal(result$key2, "value2")
})

test_that("read_yml_dict handles tabs and carriage returns", {
  # Test with tabs and carriage returns
  yml_text <- "key1: value1\tkey2: value2"
  result <- read_yml_dict(yml_text)

  expect_named(result, c("key1", "key2"))
  expect_equal(result$key1, "value1")
  expect_equal(result$key2, "value2")
})

test_that("read_yml_dict removes comments", {
  # Test that lines starting with # are removed
  yml_text <- "# This is a comment
key1: value1
# Another comment
key2: value2"

  result <- read_yml_dict(yml_text)

  expect_named(result, c("key1", "key2"))
  expect_equal(length(result), 2)
  expect_false(any(grepl("^#", unlist(result))))
})

# test_that("read_yml_dict handles complex regex patterns", {
#   # Test with complex regex patterns that need escaping, and different separator for vectors
#   yml_text <- "URL: https?:\\/\\/.*\\.com
# EMAIL: [a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"
#
#   result <- read_yml_dict(yml_text, sep = "\\s")
#
#   expect_named(result, c("URL", "EMAIL"))
#   expect_equal(result$URL, "https?:\\/\\/.*\\.com")
#   expect_equal(result$EMAIL, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")
# })

test_that("read_yml_dict handles empty input", {
  # Test with empty string
  result <- read_yml_dict("")
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("read_yml_dict handles single entry", {
  # Test with single key-value pair
  yml_text <- "single_key: single_value"
  result <- read_yml_dict(yml_text, sep = " ", keep_wss = TRUE)

  expect_named(result, "single_key")
  expect_equal(result$single_key, "single_value")
})

test_that("read_yml_dict handles keys with spaces in values", {
  # Test with spaces in values
  yml_text <- "org: World Economic Forum
acronym: W E F"

  result <- read_yml_dict(yml_text)

  expect_named(result, c("org", "acronym"))
  expect_equal(result$org, c("World", "Economic", "Forum"))
  expect_equal(result$acronym, c("W", "E", "F"))
})

test_that("read_yml_dict handles colon in values", {
  # Test when values contain colons
  yml_text <- "time: 12:30:45
url: https://example.com"

  result <- read_yml_dict(yml_text)

  expect_named(result, c("time", "url"))
  expect_equal(result$time, "12:30:45")
  expect_equal(result$url, "https://example.com")
})

test_that("read_yml_dict returns proper structure", {
  yml_text <- "key1: val1\nkey2: val2"
  result <- read_yml_dict(yml_text)

  # Check that it's a named list
  expect_true(is.list(result))
  expect_false(is.null(names(result)))
  expect_true(all(nzchar(names(result))))
})

# Edge case tests
test_that("read_yml_dict handles malformed lines gracefully", {
  # Lines without colons should be filtered out by the processing
  yml_text <- "key1: value1
malformed_line
key2: value2"

  result <- read_yml_dict(yml_text)

  # Should only contain properly formatted key-value pairs
  expect_named(result, c("key1", "key2"))
  expect_length(result, 2)
})

# Run all tests
# test_dir("path/to/tests/")
