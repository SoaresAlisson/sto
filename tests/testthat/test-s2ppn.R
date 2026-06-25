library(testthat)

test_that("s2ppn transforms text into proper name style", {
  expect_equal(s2ppn("augusto dos anjos", prep = "dos"), "Augusto dos Anjos")
  expect_equal(s2ppn("AUGUSTO DOS ANJOS", prep = "dos"), "Augusto dos Anjos")
  expect_equal(
    s2ppn("AUGUSTO Dos anjos", prep = s2v("da de di do das dos")),
    "Augusto dos Anjos"
  )
})

test_that("s2ppn works with default prepositions", {
  expect_equal(s2ppn("augusto dos anjos"), "Augusto dos Anjos")
})

test_that("s2ppn handles multiple prepositions", {
  expect_equal(
    s2ppn("jose da silva de souza", prep = c("da", "de")),
    "Jose da Silva de Souza"
  )
})

test_that("s2ppn does not match prepositions inside words", {
  expect_equal(s2ppn("Daileon", prep = "da"), "Daileon")
  expect_equal(s2ppn("DAILEON", prep = "da"), "Daileon")
  expect_equal(s2ppn("adelino", prep = "da"), "Adelino")
  expect_equal(s2ppn("delano", prep = "de"), "Delano")
  expect_equal(s2ppn("dorothy", prep = "do"), "Dorothy")
  expect_equal(s2ppn("dasd", prep = "das"), "Dasd")
})

test_that("s2ppn matches prepositions as standalone words", {
  expect_equal(s2ppn("augusto da silva", prep = "da"), "Augusto da Silva")
  expect_equal(s2ppn("jose de souza", prep = "de"), "Jose de Souza")
})

test_that("s2ppn handles text with no matching prepositions", {
  expect_equal(s2ppn("augusto anjos", prep = "dos"), "Augusto Anjos")
})

test_that("s2ppn handles empty string", {
  expect_equal(s2ppn(""), "")
})

test_that("s2ppn handles single word", {
  expect_equal(s2ppn("augusto"), "Augusto")
})

test_that("s2ppn preserves mixed case input", {
  expect_equal(s2ppn("aUgUsTo dOs aNjOs", prep = "dos"), "Augusto dos Anjos")
})

test_that("s2ppn handles multiple occurrences of same preposition", {
  expect_equal(s2ppn("de de de", prep = "de"), "de de de")
})

test_that("s2ppn handles prepositions at start of text", {
  expect_equal(s2ppn("dos anjos", prep = "dos"), "dos Anjos")
})
