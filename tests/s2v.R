
testthat::test_that("s2v function works correctly", {
  # Basic functionality
  testthat::expect_equal(s2v("a b c d"), c("a", "b", "c", "d"))
  testthat::expect_equal(s2v("a b c     d"), c("a", "b", "c", "d"))
  
  # Handles different whitespace characters
  testthat::expect_equal(s2v("a b c\nd\te"), c("a", "b", "c", "d", "e"))
  testthat::expect_equal(s2v("a b c\r\nd"), c("a", "b", "c", "d"))
  
  # Custom separator
  testthat::expect_equal(s2v("a|b|c|d e", sep = "\\|"), c("a", "b", "c", "d e"))
  testthat::expect_equal(s2v("a,b;c d", sep = "[,;]"), c("a", "b", "c d"))
  
  # Whitespace substitution
  testthat::expect_equal(s2v("a_b c", wss = "_"), c("a b", "c"))
  testthat::expect_equal(s2v("a-b c", wss = "-"), c("a b", "c"))
  
  # Edge cases
  testthat::expect_equal(s2v(""), character(0))
  testthat::expect_equal(s2v("   "), character(0))
  testthat::expect_equal(s2v(" a "), "a")
  testthat::expect_equal(s2v("a"), "a")
  
  # Print option
  testthat::expect_equal(s2v("a b c", print = TRUE), "c('a', 'b', 'c')")
  testthat::expect_equal(s2v("a", print = TRUE), "c('a')")
  testthat::expect_equal(s2v("", print = TRUE), "c('')")
  
  # Mixed cases
  testthat::expect_equal(
    s2v("apple banana\ncherry\tdate,elderberry;fig"),
    c("apple", "banana", "cherry", "date", "elderberry", "fig")
  )
  testthat::expect_equal(
    s2v("first_second third", wss = "_"),
    c("first second", "third")
  )
})

testthat::test_that("s2v handles special cases", {
  # Multiple consecutive separators
  testthat::expect_equal(s2v("a  b   c"), c("a", "b", "c"))
  testthat::expect_equal(s2v("a,,b;;c"), c("a", "b", "c"))
  
  # Custom separator with special regex characters
  testthat::expect_equal(s2v("a.b.c", sep = "\\."), c("a", "b", "c"))
  testthat::expect_equal(s2v("a+b+c", sep = "\\+"), c("a", "b", "c"))
  
  # Strings with only separators
  testthat::expect_equal(s2v(",,,,"), character(0))
  testthat::expect_equal(s2v("\n\n\t"), character(0))
})

testthat::test_that("s2v print option works correctly", {
  # Verify the output is suitable for direct R code use
  testthat::expect_equal(s2v("x y z", print = TRUE), "c('x', 'y', 'z')")
  testthat::expect_equal(s2v("1 2 3", print = TRUE), "c('1', '2', '3')")
  testthat::expect_equal(s2v("a_b_c", wss = "_", print = TRUE), "c('a b c')")
})
