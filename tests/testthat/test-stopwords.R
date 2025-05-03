library(testthat)

# show_sw 
testthat::test_that("show_sw() works correctly", {
  # Basic functionality tests
  testthat::expect_silent(show_sw("en"))
  testthat::expect_silent(show_sw("pt"))
  
  # Output type tests
  testthat::expect_type(show_sw("en"), "list")
  testthat::expect_type(show_sw("pt", as_vector = TRUE), "character")
  
  # Test with additional words
  additional_words <- c("word1", "word2")
  result_with_add <- show_sw("en", add = additional_words) |> unlist()
  testthat::expect_true(all(additional_words %in% result_with_add))
  
  # Test vector output
  vec_result <- show_sw("pt", as_vector = TRUE)
  testthat::expect_true(is.character(vec_result))
  testthat::expect_true(length(vec_result) > 0)
  
  # Test empty additional words
  testthat::expect_equal(
    show_sw("en", add = ""),
    show_sw("en")
  )
  
  # Test invalid language input
  testthat::expect_error(show_sw("xx"))
  testthat::expect_error(show_sw(123))
  
  # Test invalid add parameter
  testthat::expect_error(show_sw("en", add = 123))
  
  # Test output contains expected stopwords
  en_stopwords <- show_sw("en") |> unlist()
  testthat::expect_true("the" %in% en_stopwords)
  testthat::expect_true("and" %in% en_stopwords)
  
  pt_stopwords <- show_sw("pt") |> unlist()
  testthat::expect_true("de" %in% pt_stopwords)
  testthat::expect_true("que" %in% pt_stopwords)
  
  # Test that additional words are properly added
  custom_words <- c("custom1", "custom2")
  custom_result <- show_sw("en", add = custom_words) |> unlist()
  testthat::expect_true(all(custom_words %in% custom_result))
})

testthat::test_that("show_sw() edge cases", {
  # Test with multiple additional words
  testthat::expect_equal(
    length(unlist(show_sw("pt", add = c("a", "b", "c")))), 
                        length(unlist(show_sw("pt"))) + 3)
  
  # Test with NA in additional words (should probably fail)
  testthat::expect_error(show_sw("en", add = NA))
  
  # Test with very long additional words
  long_word <- paste(rep("a", 1000), collapse = "")
  testthat::expect_true(long_word %in% show_sw("en", add = long_word))
})


test_that("gen_stopwords function works correctly", {
  
  # Test 1: Default parameters
  testthat::expect_true(is.character(gen_stopwords()))
  testthat::expect_true(length(gen_stopwords()) > 0)
  
  # Test 2: Portuguese language
  pt_words <- gen_stopwords(lang = "pt")
  testthat::expect_true(is.character(pt_words))
  testthat::expect_true(length(pt_words) > 0)
  
  # Test 3: Specific category (Verbs)
  v_words <- gen_stopwords(lang = "pt", categories = "V")
  testthat::expect_true(is.character(v_words))
  testthat::expect_true(length(v_words) > 0)
  
  # Test 4: List output
  list_output <- gen_stopwords(lang = "pt", vec = "list")
  expect_true(is.list(list_output))
  expect_true(length(list_output) > 0)
  
  # Test 5: Named vector output
  n_vec_output <- gen_stopwords(lang = "pt", vec = "n_vec")
  expect_true(is.vector(n_vec_output))
  expect_true(!is.null(names(n_vec_output)))
  
  # Test 6: Multiple categories
  multi_cat <- gen_stopwords(lang = "pt", categories = "V CC")
  expect_true(is.character(multi_cat))
  expect_true(length(multi_cat) > length(v_words)) # Should have more words than just V
  
  # Test 7: Additional words
  custom_words <- c("palavra1", "palavra2")
  custom_output <- gen_stopwords(lang = "pt", add = custom_words)
  expect_true(all(custom_words %in% custom_output))
  
  # Test 8: Invalid vec parameter
  expect_error(gen_stopwords(vec = "invalid"))
  
  # Test 9: Invalid language
  expect_error(gen_stopwords(lang = "xx"))
  
  # Test 10: Invalid categories
  expect_error(gen_stopwords(categories = "XYZ"))
})

test_that("gen_stopwords handles edge cases", {
  # Empty categories
  expect_equal(length(gen_stopwords(categories = "")), 0)
  
  # NULL add parameter
  expect_no_error(gen_stopwords(add = NULL))
  
  # Empty add vector
  expect_equal(gen_stopwords(add = character(0)), gen_stopwords())
})
