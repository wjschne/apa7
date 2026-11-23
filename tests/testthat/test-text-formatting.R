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

# variable_name_formatter ----

test_that("variable_name_formatter renders exponents as superscripts", {
  vnf <- apa7:::variable_name_formatter
  # I(x^2) terms are labeled "x^2" by parameters
  expect_equal(vnf(c("Days^2", "x^3", "x^2^", "x^12")), c("Days^2^", "X^3^", "X^2^", "X^12^"))
  # poly() terms
  expect_equal(
    vnf(c("hp [1st degree]", "hp [2nd degree]", "hp [3rd degree]", "hp [4th degree]")),
    c("Hp", "Hp^2^", "Hp^3^", "Hp^4^")
  )
  expect_equal(vnf(c("x [linear]", "x [quadratic]")), c("X", "X^2^"))

  fit <- lm(mpg ~ wt + I(wt^2), data = mtcars)
  expect_equal(apa_parameters(fit)$Variable, c("Constant", "Wt", "Wt^2^"))
})

# sci_md ----

test_that("sci_md uses fixed notation within bounds and scientific outside", {
  expect_equal(
    sci_md(c(0.8, 1.04, 0.991, 24567857.45, .0045)),
    c("0.80", "1.04", "0.99", "2.46×10^7^", "4.50×10^−3^")
  )
  expect_equal(sci_md(c(NA, 0.8)), c(NA, "0.80"))
})

test_that("Exp_Coefficient shows odds ratios below 1 correctly", {
  fit <- glm(am ~ wt, data = mtcars, family = binomial)
  or <- apa_parameters(
    fit,
    predictor_parameters = c("Parameter", "Exp_Coefficient")
  )[["*OR*"]]
  expect_equal(
    as.numeric(sub("×.*$", "", gsub("[ -  ]", "", or[2]))),
    round(exp(coef(fit)[["wt"]]), 2)
  )
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

test_that("num_pad counts visible characters, not markdown markup", {
  # Superscript stars: ^\*\*^ renders as two characters
  result <- num_pad(c("1^\\*\\*^", "123"), padding_character = "_")
  expect_equal(result, c("1^\\*\\*^", "123"))
  # Bold markers are not counted
  expect_equal(num_pad(c("**1**", "12"), padding_character = "_"), c("_**1**", "12"))
  # Word endings are not subtracted (old "\\>" regex bug)
  expect_equal(num_pad(c("1,234", "5"), padding_character = "_"), c("1,234", "____5"))
  # Entities count as one character
  expect_equal(num_pad(c("&minus;1", "12"), padding_character = "_"), c("&minus;1", "12"))
})

test_that("num_pad pads to equal rendered width in an installed font", {
  skip_if_not(apa7:::font_available("Times New Roman"))
  tnr <- "Times New Roman"
  w <- \(x) apa7:::md_text_width(x, tnr)

  # Minus sign (0.564 em) is wider than a figure space (0.5 em)
  result <- num_pad(c("−1", "1"), font_family = tnr)
  expect_true(startsWith(result[2], " "))
  expect_lt(abs(diff(w(result))), 0.025)

  # Superscripts are measured at reduced size
  result <- num_pad(c("1^\\*^", "1^\\*\\*^", "1"), pad_left = FALSE, font_family = tnr)
  expect_lt(diff(range(w(result))), 0.05)

  # Commas are narrower than digits
  result <- num_pad(c("1,234", "5"), font_family = tnr)
  expect_lt(abs(diff(w(result))), 0.025)

  # Unknown fonts fall back to character counts
  expect_equal(
    num_pad(c("a", "bb"), padding_character = "_", font_family = "No Such Font apa7"),
    c("_a", "bb")
  )
})

test_that("align_chr aligns decimals by rendered width", {
  skip_if_not(apa7:::font_available("Times New Roman"))
  apa7_defaults(font_family = "Times New Roman", width_aware_padding = TRUE)
  x <- align_chr(c(-2.63, 2.53, 18.87))
  left <- sub("\\..*$", "", x)
  w <- apa7:::md_text_width(left, "Times New Roman")
  expect_lt(diff(range(w)), 0.025)

  # Turning width-aware padding off restores figure-space padding
  apa7_defaults(width_aware_padding = FALSE)
  withr::defer(apa7_defaults(width_aware_padding = TRUE))
  expect_equal(align_chr(c(-.01, .51)), c("−0.01", " 0.51"))
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
    c("name", "header", "latex", "formatter", "abbreviation")
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

test_that("simple settings are R options, which apa7_defaults() sets", {
  withr::defer(apa7_defaults(reset = TRUE))
  apa7_defaults(reset = TRUE)
  expect_true(bindingIsActive("accuracy", apa7:::the))

  # Options set directly (e.g., in .Rprofile) are used
  withr::local_options(apa7.page_width = "a4", apa7.intercept_text = "Intercept")
  expect_equal(apa7:::the$page_width, 210 / 25.4 - 2)
  expect_equal(apa7:::the$intercept_text, "Intercept")
  expect_equal(apa_parameters(lm(mpg ~ wt, data = mtcars))$Variable[1], "Intercept")

  # apa7_defaults() writes the options
  apa7_defaults(page_width = 6, print = "tibble", citations = "text", font_family = "Arial")
  expect_equal(getOption("apa7.page_width"), 6)
  expect_equal(getOption("apa7.print"), "tibble")
  expect_equal(getOption("apa7.citations"), "text")
  expect_equal(apa7:::the$font_family, "Arial")

  # reset removes them, so the package defaults apply
  apa7_defaults(reset = TRUE)
  expect_null(getOption("apa7.page_width"))
  expect_null(getOption("apa7.print"))
  expect_equal(apa7:::the$page_width, 6.5)
  expect_equal(apa7:::the$font_family, "Times New Roman")
  expect_identical(apa7:::the$pvalue_formatter, apa7:::default_pvalue_formatter)
})

test_that("apa7_defaults() returns the previous values, which restore them", {
  withr::defer(apa7_defaults(reset = TRUE))
  apa7_defaults(reset = TRUE)
  old <- apa7_defaults(accuracy = .001, page_width = "a4", print = "flextable")
  expect_equal(old, list(accuracy = .01, page_width = 6.5, print = "auto"))
  expect_equal(apa7:::the$accuracy, .001)

  second <- apa7_defaults(page_width = "us-letter")
  expect_equal(second, list(page_width = "a4"))

  do.call(apa7_defaults, old)
  expect_equal(apa7:::the$accuracy, .01)
  expect_equal(apa7:::the$page_width, 6.5)
  expect_equal(getOption("apa7.print"), "auto")

  all_old <- apa7_defaults(reset = TRUE)
  expect_setequal(
    names(all_old),
    c("accuracy", "font_family", "font_size", "line_spacing", "intercept_text", "column_formats", "number_formatter",
      "trim_leading_zero", "width_aware_padding", "page_width", "print", "citations")
  )
  expect_true(S7::S7_inherits(all_old$column_formats, column_formats))
})

test_that("apa7_defaults() checks its values before changing anything", {
  withr::defer(apa7_defaults(reset = TRUE))
  apa7_defaults(reset = TRUE)
  expect_error(apa7_defaults(accuracy = .001, print = "html"), "print")
  expect_equal(apa7:::the$accuracy, .01)
  expect_error(apa7_defaults(citations = "bib"), "citations")
  expect_error(apa7_defaults(accuracy = -1), "positive number")
  expect_error(apa7_defaults(font_family = 3), "character string")
  expect_error(apa7_defaults(width_aware_padding = "yes"), "TRUE")
})

test_that("the default column formats follow the accuracy setting", {
  withr::defer(apa7_defaults(reset = TRUE))
  apa7_defaults(reset = TRUE)
  fit <- lm(mpg ~ wt, data = mtcars)
  b <- coef(fit)[["wt"]]
  b_text <- function() gsub("[\u2002-\u200a ]", "", apa_parameters(fit)$`*B*`[2])
  minus <- function(x) sub("-", "\u2212", x)

  expect_equal(b_text(), minus(sprintf("%.2f", b)))
  apa7_defaults(accuracy = .001)
  expect_equal(b_text(), minus(sprintf("%.3f", b)))
  expect_equal(apa7:::the$column_formats@accuracy, .001)

  # Set directly as an option
  withr::with_options(list(apa7.accuracy = .1), {
    expect_equal(b_text(), minus(sprintf("%.1f", b)))
  })

  # Column formats set without accuracy keep their own accuracy
  apa7_defaults(column_formats = column_formats(accuracy = .0001))
  expect_equal(getOption("apa7.accuracy"), .0001)
  expect_equal(b_text(), minus(sprintf("%.4f", b)))

  # With both, accuracy wins
  apa7_defaults(column_formats = column_formats(accuracy = .0001), accuracy = .01)
  expect_equal(b_text(), minus(sprintf("%.2f", b)))

  # A column_formats argument is used as it is
  apa7_defaults(reset = TRUE)
  explicit <- apa_parameters(fit, column_formats = column_formats(accuracy = .001))
  expect_equal(gsub("[\u2002-\u200a ]", "", explicit$`*B*`[2]), minus(sprintf("%.3f", b)))
})

test_that("font_size sets the font size of all tables", {
  withr::defer(apa7_defaults(reset = TRUE))
  apa7_defaults(reset = TRUE)
  fit <- lm(mpg ~ wt, data = mtcars)
  size_of <- function(ft) unique(as.vector(ft$body$styles$text$font.size$data))

  expect_equal(size_of(apa_flextable(apa_parameters(fit))), 12)

  old <- apa7_defaults(font_size = 10)
  expect_equal(old, list(font_size = 12))
  expect_equal(getOption("apa7.font_size"), 10)
  ft <- apa_flextable(apa_parameters(fit), note = TRUE)
  expect_equal(size_of(ft), 10)
  expect_equal(unique(as.vector(ft$footer$styles$text$font.size$data)), 10)
  expect_equal(size_of(apa_cor(mtcars[, c("mpg", "wt", "hp")])), 10)
  expect_equal(size_of(apa_chisq(data.frame(a = mtcars$am, b = mtcars$vs))), 10)

  # An explicit argument is used instead
  expect_equal(size_of(apa_flextable(apa_parameters(fit), font_size = 11)), 11)

  # Set directly as an option
  withr::with_options(list(apa7.font_size = 14), {
    expect_equal(size_of(apa_flextable(apa_parameters(fit))), 14)
  })

  apa7_defaults(reset = TRUE)
  expect_equal(size_of(apa_flextable(apa_parameters(fit))), 12)
  expect_error(apa7_defaults(font_size = "big"), "positive number")
})

test_that("line_spacing sets the spacing of all tables, but not of notes", {
  withr::defer(apa7_defaults(reset = TRUE))
  apa7_defaults(reset = TRUE)
  fit <- lm(mpg ~ wt, data = mtcars)
  # Spacing is drawn with vertical padding: (line_spacing - 1) * font_size * 2/3
  pad <- function(ft, part = "body") unique(as.vector(ft[[part]]$styles$pars$padding.top$data))

  ft <- apa_flextable(apa_parameters(fit), note = TRUE)
  expect_equal(pad(ft), 8)
  footer_padding <- pad(ft, "footer")

  old <- apa7_defaults(line_spacing = 1)
  expect_equal(old, list(line_spacing = 2))
  ft <- apa_flextable(apa_parameters(fit), note = TRUE)
  expect_equal(pad(ft), 0)
  expect_equal(pad(ft, "header"), 0)
  expect_equal(pad(ft, "footer"), footer_padding)
  expect_equal(pad(apa_cor(mtcars[, c("mpg", "wt", "hp")])), 0)
  expect_equal(pad(apa_chisq(data.frame(a = mtcars$am, b = mtcars$vs))), 0)

  # An explicit argument is used instead
  expect_equal(pad(apa_flextable(apa_parameters(fit), line_spacing = 1.5)), 4)

  withr::with_options(list(apa7.line_spacing = 1.5), {
    expect_equal(pad(apa_flextable(apa_parameters(fit))), 4)
  })

  apa7_defaults(reset = TRUE)
  expect_equal(pad(apa_flextable(apa_parameters(fit))), 8)
  expect_error(apa7_defaults(line_spacing = .5), "at least 1")
})
