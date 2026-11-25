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
  expect_error(apa_flextable(d, table_width = "wide"), "must name a width")

  # A two-column document
  apa7_defaults(table_width = "column")
  expect_equal(total_width(apa_flextable(apa_parameters(fit))), col, tolerance = 1e-6)
  expect_equal(total_width(apa_flextable(apa_parameters(fit), table_width = "page")), 6.5, tolerance = 1e-6)
})

test_that("the code chunk's apa7-width option sets the width of its tables", {
  withr::defer(apa7_defaults(reset = TRUE))
  w <- function(x = NULL) {
    r <- apa7:::resolve_table_width(x)
    c(inches = r$inches, proportion = r$proportion)
  }
  in_chunk <- function(opts, code) {
    withr::local_options(knitr.in.progress = TRUE)
    old <- knitr::opts_current$get()
    withr::defer(knitr::opts_current$restore(old))
    knitr::opts_current$set(opts)
    code
  }
  col <- (8.5 - 1.5) * .48
  # Outside knitr, chunk options are ignored
  knitr::opts_current$set(`apa7-width` = "landscape")
  expect_equal(w(), c(inches = 6.5, proportion = 1))
  knitr::opts_current$restore()

  expect_equal(in_chunk(list(`apa7-width` = "landscape"), w()), c(inches = 9, proportion = 1))
  expect_equal(in_chunk(list(apa7.width = "column"), w()), c(inches = col, proportion = 1))
  # A YAML map arrives as a list
  expect_equal(in_chunk(list(`apa7-width` = list(landscape = .8)), w()), c(inches = 7.2, proportion = .8))
  expect_equal(in_chunk(list(`apa7-width` = .5), w()), c(inches = 3.25, proportion = .5))
  # An argument given to the function wins
  expect_equal(in_chunk(list(`apa7-width` = "landscape"), w("column")), c(inches = col, proportion = 1))
  # apaquarto's apa-twocolumn spans the page in a two-column document
  apa7_defaults(table_width = "column")
  expect_equal(in_chunk(list(`apa-twocolumn` = TRUE), w()), c(inches = 6.5, proportion = 1))
  expect_equal(in_chunk(list(`apa-twocolumn` = FALSE), w()), c(inches = col, proportion = 1))
  # A proportion in the chunk option is of the default width
  expect_equal(in_chunk(list(`apa7-width` = .5), w()), c(inches = col / 2, proportion = .5))
  expect_error(in_chunk(list(`apa7-width` = "wide"), w()), "apa7-width")

  # Tables printed automatically use it too
  skip_on_cran()
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  ft <- in_chunk(list(`apa7-width` = "landscape"), apa_flextable(apa_parameters(fit)))
  expect_equal(total_width(ft), 9, tolerance = 1e-6)
})

test_that("paper sizes written as in Pandoc and LaTeX are matched to Typst's names", {
  name <- apa7:::typst_paper_name
  expect_equal(name("letter"), "us-letter")
  expect_equal(name("letterpaper"), "us-letter")
  expect_equal(name("USLetter"), "us-letter")
  expect_equal(name("us-letter"), "us-letter")
  expect_equal(name("a4paper"), "a4")
  expect_equal(name("A4"), "a4")
  expect_equal(name("legal"), "us-legal")
  expect_equal(name("b5"), "iso-b5")
  expect_equal(name("a3"), "a3")
  expect_null(name("napkin"))
  expect_null(name(NULL))
})

test_that("widths follow the paper and documentmode of the document being rendered", {
  skip_if_not_installed("jsonlite")
  withr::defer(apa7_defaults(reset = TRUE))
  render <- function(metadata, code) {
    info <- withr::local_tempfile(fileext = ".json")
    jsonlite::write_json(list(format = list(metadata = metadata)), info, auto_unbox = TRUE)
    withr::local_envvar(QUARTO_EXECUTE_INFO = info)
    withr::local_options(knitr.in.progress = TRUE)
    code
  }
  widths <- function() {
    c(
      page = apa7:::page_width_in(),
      landscape = apa7:::landscape_width_in(),
      column = apa7:::column_width_in(),
      default = apa7:::resolve_table_width()$inches
    )
  }
  a4 <- 210 / 25.4
  expect_equal(render(list(papersize = "a4"), widths()), c(page = a4 - 2, landscape = 297 / 25.4 - 2, column = (a4 - 1.5) * .48, default = a4 - 2))
  # Journal mode: 0.75-inch margins, and tables are a column wide
  expect_equal(render(list(documentmode = "jou"), widths()), c(page = 7, landscape = 9, column = 3.36, default = 3.36))
  expect_equal(render(list(documentmode = "journal", papersize = "a4paper"), widths())[["default"]], (a4 - 1.5) * .48)
  # Thesis mode: a 1.5-inch left margin
  expect_equal(render(list(documentmode = "thesis"), widths())[["page"]], 6)
  expect_equal(render(list(documentmode = "man"), widths()), c(page = 6.5, landscape = 9, column = 3.36, default = 6.5))
  # Nothing about the layout: the package defaults
  expect_equal(render(list(title = "A"), widths()), c(page = 6.5, landscape = 9, column = 3.36, default = 6.5))
  # Settings made with apa7_defaults() win over the document
  apa7_defaults(page_width = 6.5, table_width = 1)
  expect_equal(render(list(documentmode = "jou", papersize = "a4"), widths())[c("page", "default")], c(page = 6.5, default = 6.5))
  apa7_defaults(reset = TRUE)
  # Outside a render, the document is not consulted
  info <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(format = list(metadata = list(documentmode = "jou"))), info, auto_unbox = TRUE)
  withr::local_envvar(QUARTO_EXECUTE_INFO = info)
  expect_equal(apa7:::resolve_table_width()$inches, 6.5)
})
