#
test_that("generates stopwords", {
  #expect_equal(2 * 2, 4)
  gen_stopwords("en")
  testthat::expect_s3_class()
})
