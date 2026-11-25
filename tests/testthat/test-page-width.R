library(apa7)
library(testthat)

total_width <- function(ft) sum(ft$body$colwidths)

test_that("page_width accepts inches and Typst paper sizes", {
  withr::defer(apa7_defaults(reset = TRUE))
  expect_equal(apa7:::page_text_width(6), 6)
  expect_equal(apa7:::page_text_width("us-letter"), 6.5)
  expect_equal(apa7:::page_text_width("a4"), 210 / 25.4 - 2)
  expect_equal(apa7:::page_text_width(" A4 "), 210 / 25.4 - 2)
  expect_equal(apa7:::page_text_width("fr-carré"), 450 / 25.4 - 2)
  expect_length(apa7:::typst_paper_widths, 107)

  expect_error(apa7_defaults(page_width = "a44"), "Did you mean \"a4\"")
  expect_error(apa7_defaults(page_width = "letter"), "us-letter")
  expect_error(apa7_defaults(page_width = -2), "positive number")
  expect_error(apa7_defaults(page_width = c(5, 6)), "positive number")
  expect_error(apa7_defaults(page_width = "a10"), "too narrow")
})

test_that("tables fill the page width set with apa7_defaults()", {
  skip_on_cran()
  withr::defer(apa7_defaults(reset = TRUE))
  fit <- lm(mpg ~ wt + hp, data = mtcars)

  expect_equal(total_width(apa_flextable(apa_parameters(fit))), 6.5, tolerance = 1e-6)

  apa7_defaults(page_width = "a4")
  a4 <- 210 / 25.4 - 2
  expect_equal(total_width(apa_flextable(apa_parameters(fit))), a4, tolerance = 1e-6)
  half <- apa_flextable(apa_parameters(fit), table_width = .5)
  expect_equal(total_width(half), a4 / 2, tolerance = 1e-6)
  expect_equal(half$properties$width, .5)
  expect_equal(total_width(apa_cor(mtcars[, c("mpg", "wt", "hp")])), a4, tolerance = 1e-6)
  pw <- pretty_widths(flextable::flextable(head(mtcars[, 1:3])))
  expect_equal(total_width(pw), a4, tolerance = 1e-6)

  apa7_defaults(page_width = 5)
  expect_equal(total_width(apa_flextable(apa_parameters(fit))), 5, tolerance = 1e-6)

  apa7_defaults(reset = TRUE)
  expect_equal(total_width(apa_flextable(apa_parameters(fit))), 6.5, tolerance = 1e-6)
})

test_that("landscape and column widths follow the paper size and apaquarto's journal mode", {
  withr::defer(apa7_defaults(reset = TRUE))
  expect_length(apa7:::typst_paper_heights, 107)
  expect_identical(names(apa7:::typst_paper_heights), names(apa7:::typst_paper_widths))
  # Letter-size paper (also when page_width is the default 6.5 inches)
  expect_equal(apa7:::landscape_width_in(), 9)
  expect_equal(apa7:::column_width_in(), (8.5 - 1.5) * .48)
  apa7_defaults(page_width = "us-letter")
  expect_equal(apa7:::landscape_width_in(), 9)
  expect_equal(apa7:::column_width_in(), 3.36)
  apa7_defaults(page_width = "a4")
  expect_equal(apa7:::landscape_width_in(), 297 / 25.4 - 2)
  expect_equal(apa7:::column_width_in(), (210 / 25.4 - 1.5) * .48)
  # Set directly, and restored with NA
  old <- apa7_defaults(landscape_width = 8, column_width = 3)
  expect_equal(apa7:::landscape_width_in(), 8)
  expect_equal(apa7:::column_width_in(), 3)
  expect_true(is.na(old$landscape_width))
  do.call(apa7_defaults, old)
  expect_equal(apa7:::landscape_width_in(), 297 / 25.4 - 2)
  expect_error(apa7_defaults(column_width = -1), "positive number")
  expect_error(apa7_defaults(landscape_width = "wide"), "positive number")
})

test_that("table_width takes proportions, named widths, and units", {
  withr::defer(apa7_defaults(reset = TRUE))
  w <- function(x, ...) {
    r <- apa7:::resolve_table_width(x, ...)
    c(inches = r$inches, proportion = r$proportion)
  }
  col <- (8.5 - 1.5) * .48
  expect_equal(w(NULL), c(inches = 6.5, proportion = 1))
  expect_equal(w(.5), c(inches = 3.25, proportion = .5))
  expect_equal(w(0), c(inches = 0, proportion = 0))
  expect_equal(w("page"), c(inches = 6.5, proportion = 1))
  expect_equal(w("Portrait"), c(inches = 6.5, proportion = 1))
  expect_equal(w("landscape"), c(inches = 9, proportion = 1))
  expect_equal(w("column"), c(inches = col, proportion = 1))
  expect_equal(w(c(landscape = .8)), c(inches = 7.2, proportion = .8))
  expect_equal(w(c(column = .5)), c(inches = col / 2, proportion = .5))
  expect_equal(w(c(inches = 5)), c(inches = 5, proportion = 5 / 6.5))
  expect_equal(w(c(`in` = 5)), c(inches = 5, proportion = 5 / 6.5))
  expect_equal(w(c(cm = 10)), c(inches = 10 / 2.54, proportion = 10 / 2.54 / 6.5))
  expect_equal(w(c(mm = 150)), c(inches = 150 / 25.4, proportion = 150 / 25.4 / 6.5))
  # Wider than the page: the table fills it, with a warning
  expect_warning(wide <- w(c(mm = 200)), "wider than the page")
  expect_equal(wide, c(inches = 6.5, proportion = 1))
  # An unnamed number above 1 is read as inches (or in unit), with a warning
  expect_warning(eight <- w(8), "c(inches = 8)", fixed = TRUE)
  expect_equal(eight, c(inches = 6.5, proportion = 1))
  expect_warning(cm <- w(15, unit = "cm"), "c(cm = 15)", fixed = TRUE)
  expect_equal(cm[["inches"]], 15 / 2.54)
  # Errors
  expect_error(w(-1), "at least 0")
  expect_error(w("wide"), "must name a width")
  expect_error(w(c(landscape = 2)), "from 0 to 1")
  expect_error(w(c(feet = 1)), "unknown width or unit")
  expect_error(w(c(.5, .6)), "single number")

  # The default width set with apa7_defaults(): proportions are of it
  apa7_defaults(table_width = "column")
  expect_equal(w(NULL), c(inches = col, proportion = 1))
  expect_equal(w(.5), c(inches = col / 2, proportion = .5))
  expect_equal(w("page"), c(inches = 6.5, proportion = 1))
  expect_warning(w(c(inches = 5)), "wider than the column")
  expect_error(apa7_defaults(table_width = "wide"), "must name a width")
})

test_that("every table function takes table_width in the same forms", {
  skip_on_cran()
  withr::defer(apa7_defaults(reset = TRUE))
  d <- head(mtcars[1:3])
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  col <- (8.5 - 1.5) * .48

  land <- apa_flextable(apa_parameters(fit), table_width = "landscape")
  expect_equal(total_width(land), 9, tolerance = 1e-6)
  expect_equal(land$properties$width, 1)
  cm <- apa_flextable(apa_parameters(fit), table_width = c(cm = 10))
  expect_equal(total_width(cm), 10 / 2.54, tolerance = 1e-6)
  expect_equal(cm$properties$width, 10 / 2.54 / 6.5)

  cor_col <- apa_cor(mtcars[, c("mpg", "wt", "hp")], table_width = "column")
  expect_equal(total_width(cor_col), col, tolerance = 1e-6)

  pw <- pretty_widths(flextable::flextable(d), table_width = c(landscape = .5))
  expect_equal(total_width(pw), 4.5, tolerance = 1e-6)
  expect_equal(pw$properties$width, .5)
  pw_cm <- pretty_widths(flextable::flextable(d), unit = "cm", table_width = c(cm = 10))
  expect_equal(total_width(pw_cm), 10 / 2.54, tolerance = 1e-6)

  expect_equal(apa_style(flextable::flextable(d))$properties$width, 1)
  expect_equal(apa_style(flextable::flextable(d), table_width = 0)$properties$width, 0)
  expect_equal(apa_style(flextable::flextable(d), table_width = "column")$properties$width, 1)

  # Inches without a name are read with a warning everywhere
  expect_warning(apa_flextable(d, table_width = 4), "c(inches = 4)", fixed = TRUE)
  expect_warning(apa_style(flextable::flextable(d), table_width = 4), "c(inches = 4)", fixed = TRUE)
  expect_warning(apa_chisq(mtcars[c("am", "vs")], table_width = 4), "c(inches = 4)", fixed = TRUE)
  expect_error(apa_flextable(d, table_width = "wide"), "must name a width")

  # A two-column document
  apa7_defaults(table_width = "column")
  expect_equal(total_width(apa_flextable(apa_parameters(fit))), col, tolerance = 1e-6)
  expect_equal(total_width(apa_flextable(apa_parameters(fit), table_width = "page")), 6.5, tolerance = 1e-6)
})
