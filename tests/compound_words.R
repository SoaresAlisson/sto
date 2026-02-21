library(testthat)

txt <- "ice-cream in Soviet Union or New York?"
s <- "Soviet_Union New_York"

testthat::test_that("compound_words function works correctly", {
  expected_equal(compound_words(txt, s), "ice-cream in Soviet_Union or New_York?")
  compound_words(txt, "") |> expect_error()
})
