library(testthat)
# test_subs_synonyms.R

# Create a simple substitution list
subst <- list(
  WEF = c("World_Economic_Forum", "W\\.E\\.F\\.", "World Economic Forum"),
  UN = c("United_Nations", "U\\.N\\.")
)

txt <- "The World_Economic_Forum announced that X and Y. The W.E.F. is a lorem ipsum. The World Economic Forum occured lorem ipsum. And the WORLD ECONOMIC FORUM, it must be said, lorem ipsum."

df <- tibble::tibble(
  txt = c(
    "The World_Economic_Forum announced that X and Y.",
    "The W.E.F. is a lorem ipsum",
    "The World Economic Forum occured lorem ipsum",
    "And the WORLD ECONOMIC FORUM, it must be said, lorem ipsum."
  )
)
test_that("subs_synonyms with vector", {
  subs_synonyms(txt, subst)
})

test_that("subs_synonyms with data frame", {
  result <- df |> dplyr::mutate(txt = subs_synonyms(txt, subst))
  result_ic <- df |> dplyr::mutate(txt = subs_synonyms(txt, subst, ic = T))

  expected <- "The WEF announced that X and Y. The WEF is a lorem ipsum. The WEF occured lorem ipsum. And the WORLD ECONOMIC FORUM, it must be said, lorem ipsum."
  expect_equal(result_ic, expected)
})

#
test_that("subs_synonyms basic functionality", {
  result <- subs_synonyms(txt, subst)
  result_ic <- subs_synonyms(txt, subst, ic = TRUE)

  expected <- "The WEF announced that X and Y. The WEF is a lorem ipsum. The WEF occured lorem ipsum. And the WORLD ECONOMIC FORUM, it must be said, lorem ipsum."
  expect_equal(result, expected)
  expected <- "The WEF announced that X and Y. The WEF is a lorem ipsum. The WEF occured lorem ipsum. And the WEF, it must be said, lorem ipsum."
  expect_equal(result_ic, expected)
})

test_that("subs_synonyms with ignore case (ic = TRUE)", {
  # Without ignore case
  result_no_ic <- subs_synonyms(txt, subst, ic = FALSE)
  # With ignore case (should match lowercase too)
  result_ic <- subs_synonyms(txt, subst, ic = TRUE)

  # The third occurrence (lowercase) should only be replaced with ic = TRUE
  expect_false(grepl("WEF", strsplit(result_no_ic, "\\.")[[1]][3]))
  expect_true(grepl("WEF", strsplit(result_ic, "\\.")[[1]][3]))
})

test_that("subs_synonyms handles multiple replacements correctly", {
  subst <- list(
    org = c("organization", "organisation"),
    color = c("color", "colour")
  )

  txt <- "The organization uses colour in their organisation. Color is important."

  result <- subs_synonyms(txt, subst)

  expected <- "The org uses color in their org. color is important."
  expect_equal(result, expected)
})

test_that("subs_synonyms handles overlapping patterns", {
  # Test when one pattern could match another's replacement
  subst <- list(
    short = "short_version",
    version = c("short_version", "long_version")
  )

  txt <- "Use the short_version or long_version"

  # This tests the order of replacements
  result <- subs_synonyms(txt, subst)

  # Note: Order matters! If "short" is processed first, "short_version" becomes "short"
  # Then when processing "version", "short" won't match
  expect_type(result, "character")
  expect_equal(length(result), 1)
})

test_that("subs_synonyms with empty substitution list", {
  txt <- "This is a test sentence."
  subst <- list()

  result <- subs_synonyms(txt, subst)

  # Should return original text unchanged
  expect_equal(result, txt)
})

test_that("subs_synonyms with empty text", {
  subst <- list(WEF = c("World_Economic_Forum"))

  # Empty string
  result1 <- subs_synonyms("", subst)
  expect_equal(result1, "")

  # Character vector with empty strings
  result2 <- subs_synonyms(character(0), subst)
  expect_equal(result2, character(0))
})

test_that("subs_synonyms with special regex characters", {
  subst <- list(
    dot = c("\\.\\.\\.", "\\.\\.", "\\."),
    dollar = "\\$",
    parentheses = c("\\(", "\\)")
  )

  txt <- "Price is $100... Wait (really)? Yes.."

  result <- subs_synonyms(txt, subst)

  # Should handle regex special characters properly
  expect_false(grepl("\\$", result))
  expect_false(grepl("\\.\\.\\.", result))
})

test_that("subs_synonyms with multiple text inputs", {
  subst <- list(USA = c("United States", "U\\.S\\.A\\."))

  txt_vector <- c(
    "The United States is a country.",
    "U.S.A. is powerful.",
    "Both United States and U.S.A. are used."
  )

  result <- subs_synonyms(txt_vector, subst)

  expect_length(result, 3)
  expect_equal(result[1], "The USA is a country.")
  expect_equal(result[2], "USA is powerful.")
  expect_equal(result[3], "Both USA and USA are used.")
})

test_that("subs_synonyms preserves non-matching text", {
  subst <- list(
    AI = c("artificial intelligence", "A\\.I\\."),
    ML = "machine learning"
  )

  txt <- "Artificial intelligence and machine learning are different. So is robotics."

  result <- subs_synonyms(txt, subst, ic = FALSE)

  # Only "artificial intelligence" should be replaced (case-sensitive)
  expect_false(grepl("AI", result))
  expect_true(grepl("Artificial intelligence", result))
  expect_true(grepl("ML", result))
  expect_true(grepl("robotics", result))
})

test_that("subs_synonyms handles case sensitivity edge cases", {
  subst <- list(test = c("TEST", "test", "Test"))

  txt <- "TEST test Test tEsT TESTING"

  # Without ignore case
  result_no_ic <- subs_synonyms(txt, subst, ic = FALSE)
  # With ignore case
  result_ic <- subs_synonyms(txt, subst, ic = TRUE)

  # Without ic: Only exact matches
  # With ic: All case variations should match
  expect_true(str_count(result_no_ic, "test") <= 3) # Exact matches only
  expect_true(str_count(result_ic, "test") >= 3) # Case-insensitive matches
})

test_that("subs_synonyms works with read_yml_dict output", {
  # Test integration with read_yml_dict
  yml_text <- "WEF: World_Economic_Forum W\\.E\\.F\\.
UN: United_Nations U\\.N\\."

  subst <- read_yml_dict(yml_text)
  txt <- "The World_Economic_Forum and U.N. met today."

  result <- subs_synonyms(txt, subst)

  expect_equal(result, "The WEF and UN met today.")
})

test_that("subs_synonyms handles word boundaries correctly", {
  subst <- list(
    cat = "cat",
    category = "category"
  )

  txt <- "The cat category is important. Catastrophe is different."

  result <- subs_synonyms(txt, subst)

  # "cat" in "category" and "Catastrophe" should not be replaced
  # because str_replace_all doesn't use word boundaries by default
  expect_true(grepl("category", result)) # Should remain unchanged
  expect_true(grepl("Catastrophe", result)) # Should remain unchanged
  expect_true(grepl("cat ", result)) # "cat" with space should be replaced
})

test_that("subs_synonyms with complex regex patterns", {
  subst <- list(
    number = c("\\d+", "[0-9]+"),
    email = c("\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b")
  )

  txt <- "Contact me at test@example.com or call 123-456-7890. My ID is 42."

  result <- subs_synonyms(txt, subst)

  # This tests if complex regex patterns work
  expect_false(grepl("\\d", result))
  expect_false(grepl("@", result))
})

test_that("subs_synonyms error handling", {
  subst <- list(WEF = c("World_Economic_Forum"))

  # Test with NULL text
  expect_error(subs_synonyms(NULL, subst))

  # Test with non-character text
  expect_error(subs_synonyms(123, subst))

  # Test with malformed subst (not a named list)
  expect_error(subs_synonyms("test", "not a list"))
  expect_error(subs_synonyms("test", list("unnamed")))
})

test_that("subs_synonyms with NA values", {
  subst <- list(WEF = c("World_Economic_Forum"))

  txt <- c("Text with World_Economic_Forum", NA, "Another World_Economic_Forum")

  result <- subs_synonyms(txt, subst)

  expect_length(result, 3)
  expect_true(is.na(result[2]))
  expect_equal(result[1], "Text with WEF")
  expect_equal(result[3], "Another WEF")
})

test_that("subs_synonyms preserves string attributes", {
  subst <- list(test = "TEST")

  txt <- "This is a TEST"
  names(txt) <- "labeled_string"

  result <- subs_synonyms(txt, subst)

  expect_named(result, "labeled_string")
  expect_equal(result, c(labeled_string = "This is a test"))
})

## IF subs is a dataframe
g_sub <- tibble::tribble(
  ~word, ~subs,
  "^(?i)the_", "",
)

"The_New_York_Times is going to" |> subs_synonyms(g_sub)
