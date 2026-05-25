library(apa7)
library(testthat)

# is_numeric_like ----

test_that("is_numeric_like returns TRUE for numeric-like strings", {
  expect_true(is_numeric_like(c("1", "2.5", "-3")))
  expect_true(is_numeric_like(c("-9", " 2.0", "-1.0 ")))
  expect_true(is_numeric_like(c("9", -1.2, "10")))
})

test_that("is_numeric_like returns FALSE when any element is non-numeric", {
  expect_false(is_numeric_like(c("9-", -1, "10")))
  expect_false(is_numeric_like(c("abc", "1")))
})

test_that("is_numeric_like elementwise returns per-element result", {
  result <- is_numeric_like(c("1", "abc", "3.0"), elementwise = TRUE)
  expect_equal(result, c(TRUE, FALSE, TRUE))
})

test_that("is_numeric_like treats NA as acceptable (non-disqualifying)", {
  expect_true(is_numeric_like(c("1", NA)))
})

test_that("is_numeric_like treats empty string as acceptable", {
  expect_true(is_numeric_like(c("1", "")))
})

# num_pad ----

test_that("num_pad prepends padding character to shorter string on left", {
  result <- num_pad(c("a", "bb"))
  expect_true(startsWith(result[1], "&numsp;"))
  expect_false(startsWith(result[2], "&numsp;"))
})

test_that("num_pad appends padding character to shorter string on right", {
  result <- num_pad(c("a", "bb"), pad_left = FALSE)
  expect_true(endsWith(result[1], "&numsp;"))
  expect_false(endsWith(result[2], "&numsp;"))
})

test_that("num_pad pads every element to the same visual length", {
  x <- c("1", "22", "333")
  result <- num_pad(x)
  # Each result should start with as many &numsp; entities as needed
  pad_counts <- nchar(result) - nchar(x)
  # The longest string should have no padding; others should have some
  expect_equal(pad_counts[3], 0L)
  expect_gt(pad_counts[1], 0L)
  expect_gt(pad_counts[2], 0L)
})

test_that("num_pad replaces NA with empty string and still pads", {
  # num_pad replaces NA with NA_value before padding, so the padded slot
  # ends up as pad_character(s) prepended to NA_value
  result <- num_pad(c("a", NA), NA_value = "")
  expect_true(grepl("^(&numsp;)*$", result[2]))
})

# hanging_indent ----

test_that("hanging_indent returns a character string", {
  result <- hanging_indent(
    "Hello Darkness, my old friend. I've come to talk with you again."
  )
  expect_type(result, "character")
  expect_length(result, 1L)
})

test_that("hanging_indent returns a character string", {
  result <- hanging_indent(
    "Hello Darkness, my old friend. I've come to talk with you again.",
    wrap_equal_width = TRUE
  )
  expect_type(result, "character")
  expect_length(result, 1L)
})

test_that("hanging_indent contains newline continuation", {
  result <- hanging_indent(
    "Hello Darkness, my old friend.",
    width = 10
  )
  expect_true(grepl("\n", result))
})

test_that("hanging_indent handles NA", {
  result <- hanging_indent(NA_character_)
  expect_true(is.na(result) || nchar(result) == 0 || result == "NA")
})

# str_wrap_equal ----

test_that("str_wrap_equal returns a character string", {
  result <- str_wrap_equal("This is a long sentence that should be wrapped.")
  expect_type(result, "character")
  expect_length(result, 1L)
})

test_that("str_wrap_equal wraps at max_width", {
  result <- str_wrap_equal(
    "word1 word2 word3 word4 word5 word6",
    max_width = 10
  )
  lines <- strsplit(result, "\n")[[1]]
  expect_true(all(nchar(lines) <= 15)) # some tolerance for word wrap
})

test_that("str_wrap_equal handles NA", {
  expect_equal(str_wrap_equal(NA_character_), NA_character_)
})

test_that("str_wrap_equal handles short strings without wrapping", {
  result <- str_wrap_equal("Short", max_width = 40)
  expect_false(grepl("\n", result))
})

test_that("str_wrap_equal is vectorized", {
  result <- str_wrap_equal(
    c("Hello world.", "A much longer sentence that needs wrapping."),
    max_width = 15
  )
  expect_length(result, 2L)
})

# star_balance ----

test_that("star_balance prepends padding for starred strings", {
  result <- star_balance(".05^\\*\\*^")
  expect_type(result, "character")
  expect_true(nchar(result) > nchar(".05^\\*\\*^"))
})

test_that("star_balance leaves non-starred strings unchanged in content", {
  x <- "no stars here"
  result <- star_balance(x)
  expect_equal(result, x)
})

test_that("star_balance preserves NA", {
  expect_equal(star_balance(NA_character_), NA_character_)
})

test_that("star_balance works without superscript", {
  result <- star_balance("\\*\\*", superscript = FALSE)
  expect_false(grepl("\\^", result))
})

# column_format ----
test_that("methods for column_format", {
  R2 <- column_format(
    "R2",
    header = "*R*^2^",
    latex = "$R^2$",
    formatter = \(x, accuracy = the$accuracy, ...) {
      align_chr(x, accuracy = accuracy, trim_leading_zeros = TRUE, ...)
    }
  )

  expect_equal(
    colnames(tibble::as_tibble(R2)),
    c("name", "header", "latex", "formatter")
  )
  # str() prints to stdout and uses cli for headers; capture both to keep
  # the test console clean while still verifying the return value.
  invisible(capture.output(suppressMessages(sR2 <- str(R2))))
  expect_identical(sR2, R2)

  my_formatter <- column_formats()
  expect_equal(class(my_formatter@get_header_rename), "character")
  expect_equal(class(my_formatter@get_header_rename_latex), "character")
  expect_error(
    column_formats(custom_columns = c(4)),
    "custom_columns must be a named list"
  )
  expect_no_error(
    column_formats(
      custom_columns = list(x = column_format("x", "x", "x", mean))
    )
  )
  expect_no_error(
    invisible(capture.output(suppressMessages(str(column_formats()))))
  )
  expect_no_error(tibble::as_tibble(column_formats()))
  invisible(
    capture.output(
      suppressMessages(
        expect_equal(print(column_formats()), column_formats())
      )
    )
  )
})

# apa7_defaults ----

test_that("apa7_defaults", {
  expect_no_error(
    apa7_defaults(
      accuracy = .001,
      font_family = "Arial",
      intercept_text = "b0",
      column_formats = column_formats(),
      number_formatter = align_chr,
      trim_leading_zero = TRUE
    )
  )
  expect_no_error(apa7_defaults(reset = TRUE))
})

# apa_format_columns ----
test_that("apa_format_columns", {
  d <- tibble::tibble(`t(45)` = 2, `CI` = .95, `CI_low` = 45, `CI_high` = 89) |>
    apa_format_columns()
  expect_equal(colnames(d), c("*t*(45)", "95% CI"))

  cf <- column_formats()
  cf$CI_percent <- NULL

  d <- tibble::tibble(
    `95% CI` = "[0,0.001]",
    t = 2,
    df_error = 45,
    p = ".04"
  )

  d1 <- apa_format_columns(
    d,
    accuracy = 0.1,
    column_formats = cf,
    no_format_columns = "p"
  )
  expect_equal(colnames(d1), c("95% CI", "*t*", "*p*"))
  cf$t@latex <- "t"
  d2 <- apa_format_columns(
    d,
    accuracy = 0.1,
    column_formats = cf,
    latex_headers = TRUE,
    rename_headers = TRUE
  )

  expect_equal(colnames(d2), c("95% CI", "t", "$p$"))
})
