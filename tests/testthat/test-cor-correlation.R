library(apa7)
library(testthat)

d_cor <- mtcars[, c("mpg", "wt", "hp", "qsec")]

footer_lines <- function(ft) {
  chunks <- flextable::information_data_chunk(ft)
  chunks <- chunks[chunks$.part == "footer", ]
  as.vector(tapply(chunks$txt, chunks$.row_id, paste, collapse = ""))
}

test_that("apa_cor() formats correlation::correlation() output like data", {
  skip_on_cran()
  skip_if_not_installed("correlation")
  r <- correlation::correlation(d_cor, p_adjust = "none")
  from_cor <- apa_cor(r, output = "tibble")
  from_data <- apa_cor(d_cor, output = "tibble")
  # Same correlations, descriptives, and (unadjusted) stars
  expect_equal(
    tibble::as_tibble(from_cor),
    tibble::as_tibble(from_data),
    ignore_attr = TRUE
  )
  expect_equal(footer_lines(apa_cor(r)), footer_lines(apa_cor(d_cor)))
})

test_that("the note names the kind of correlation and the p-value adjustment", {
  skip_on_cran()
  skip_if_not_installed("correlation")
  holm <- footer_lines(apa_cor(correlation::correlation(d_cor)))
  expect_equal(holm[1], "Note. p-values are adjusted for multiple comparisons with Holm’s method (Holm, 1979).")
  expect_match(holm[2], "p.<.[.]05")

  spearman <- correlation::correlation(d_cor, method = "spearman", p_adjust = "none")
  tbl <- apa_cor(spearman, output = "tibble")
  rho <- cor(d_cor$mpg, d_cor$wt, method = "spearman")
  expect_equal(
    tbl$`1`[2],
    signs::signs(rho, accuracy = .01, trim_leading_zeros = TRUE)
  )
  expect_equal(footer_lines(apa_cor(spearman))[1], "Note. Values are Spearman correlations.")

  partial <- footer_lines(apa_cor(
    correlation::correlation(d_cor, partial = TRUE, p_adjust = "bonferroni"),
    bold_significant = TRUE
  ))
  expect_length(partial, 1)
  expect_match(partial, "partial Pearson correlations, each controlling", fixed = TRUE)
  expect_match(partial, "the Bonferroni correction", fixed = TRUE)
  expect_match(partial, "are bolded.$")

  # A custom note replaces the generated one
  expect_equal(
    footer_lines(apa_cor(correlation::correlation(d_cor), note = "Custom.")),
    "Note. Custom."
  )
})

test_that("unsupported correlation output gives clear errors", {
  skip_if_not_installed("correlation")
  expect_error(
    apa_cor(correlation::correlation(d_cor[1:2], d_cor[3:4])),
    "every pair"
  )
  grouped <- dplyr::group_by(mtcars[, c("am", "mpg", "wt")], am)
  expect_error(apa_cor(correlation::correlation(grouped)), "by group")
  # Without the data, the table has no summary columns
  r <- correlation::correlation(d_cor)
  attr(r, "data") <- NULL
  tbl <- apa_cor(r, output = "tibble")
  expect_false(any(c("M", "SD") %in% colnames(tbl)))
  expect_equal(tbl$Variable, colnames(d_cor))
})

test_that("correlation_note_sentences() names other adjustments", {
  expect_match(
    correlation_note_sentences(p_adjust = "fdr"),
    "Benjamini–Hochberg",
    fixed = TRUE
  )
  expect_length(correlation_note_sentences(p_adjust = "none"), 0)
})
