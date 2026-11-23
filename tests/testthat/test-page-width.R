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
