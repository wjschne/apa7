library(apa7)
library(testthat)

test_that("tagger wraps non-empty strings", {
  expect_equal(tagger("hello", "<b>"), "<b>hello</b>")
  expect_equal(tagger("hello", "<span>"), "<span>hello</span>")
})

test_that("tagger leaves empty strings empty", {
  expect_equal(tagger("", "<b>"), "")
})

test_that("tagger returns NA for NA input", {
  expect_equal(tagger(NA_character_, "<b>"), NA_character_)
})

test_that("tagger handles vectors", {
  x <- c("hello", "", NA)
  result <- tagger(x, "<b>")
  expect_equal(result, c("<b>hello</b>", "", NA_character_))
})

test_that("tagger accepts explicit right_tag", {
  expect_equal(tagger("hi", "[", "]"), "[hi]")
})

test_that("bold_md wraps with **", {
  expect_equal(bold_md("text"), "**text**")
  expect_equal(bold_md(""), "")
  expect_equal(bold_md(NA_character_), NA_character_)
})

test_that("italic_md wraps with *", {
  expect_equal(italic_md("text"), "*text*")
  expect_equal(italic_md(""), "")
  expect_equal(italic_md(NA_character_), NA_character_)
})

test_that("superscript_md wraps with ^", {
  expect_equal(superscript_md("2"), "^2^")
  expect_equal(superscript_md(""), "")
  expect_equal(superscript_md(NA_character_), NA_character_)
})

test_that("subscript_md wraps with ~", {
  expect_equal(subscript_md("i"), "~i~")
  expect_equal(subscript_md(""), "")
  expect_equal(subscript_md(NA_character_), NA_character_)
})

test_that("header_md produces level-1 header by default", {
  expect_equal(header_md("Introduction"), "# Introduction")
})

test_that("header_md respects level argument", {
  expect_equal(header_md("Methods", 2), "## Methods")
  expect_equal(header_md("Results", 3), "### Results")
})

test_that("header_md produces no trailing tag", {
  result <- header_md("Discussion")
  expect_false(grepl("#$", result))
})
