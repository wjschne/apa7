library(apa7)
library(testthat)

d_cor <- mtcars[, c("mpg", "wt", "hp", "qsec")]

# Lines of the note below the table (apa_cor() tables get their note from
# apa_note(), through apa_flextable(note = TRUE))
footer_lines <- function(ft) {
  if (!inherits(ft, "flextable")) {
    ft <- apa_flextable(ft, note = TRUE)
  }
  chunks <- flextable::information_data_chunk(ft)
  chunks <- chunks[chunks$.part == "footer", ]
  as.vector(tapply(chunks$txt, chunks$.row_id, paste, collapse = ""))
}

test_that("apa_cor() formats correlation::correlation() output like data", {
  skip_on_cran()
  skip_if_not_installed("correlation")
  r <- correlation::correlation(d_cor, p_adjust = "none")
  from_cor <- apa_cor(r)
  from_data <- apa_cor(d_cor)
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
  expect_length(holm, 1)
  expect_match(holm, "^Note. p-values are adjusted for multiple comparisons with Holm’s method [(]Holm, 1979[)]. ")
  expect_match(holm, "p.\u2264.[.]05")

  spearman <- correlation::correlation(d_cor, method = "spearman", p_adjust = "none")
  tbl <- apa_cor(spearman)
  rho <- cor(d_cor$mpg, d_cor$wt, method = "spearman")
  expect_equal(
    tbl$`1`[2],
    signs::signs(rho, accuracy = .01, trim_leading_zeros = TRUE)
  )
  expect_match(footer_lines(apa_cor(spearman)), "^Note. Values are Spearman correlations. ")

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
    footer_lines(apa_flextable(apa_cor(correlation::correlation(d_cor)), note = "Custom.")),
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
  tbl <- apa_cor(r)
  expect_false(any(c("*M*", "*SD*") %in% colnames(tbl)))
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

test_that("apa_cor() computes Pearson correlations with unadjusted p-values, as before", {
  tbl <- apa_cor(d_cor)
  ct <- attr(tbl, "cortest")
  expect_equal(unname(ct$r), unname(cor(d_cor)))
  p <- psych::corTest(d_cor)$p
  expect_equal(ct$p[lower.tri(ct$p)], p[lower.tri(p)])
  # Pairwise deletion, as psych::corTest() did
  d_missing <- d_cor
  d_missing$mpg[c(1, 5, 9)] <- NA
  ct <- attr(apa_cor(d_missing), "cortest")
  expect_equal(unname(ct$r), unname(cor(d_missing, use = "pairwise")))
})

test_that("apa_cor() keeps variable names with spaces for every kind of correlation", {
  d <- d_cor
  names(d) <- c("Miles per gallon", "Weight (1000 lbs)", "Gross horsepower", "Quarter-mile time")
  partial <- apa_cor(d, partial = TRUE, method = "spearman")
  expect_equal(partial$Variable, names(d))
  expect_match(footer_lines(partial)[1], "Values are partial Spearman correlations", fixed = TRUE)
  expect_equal(
    attr(apa_cor(d, partial = TRUE), "cortest")$r,
    attr(apa_cor(d_cor, partial = TRUE), "cortest")$r,
    ignore_attr = TRUE
  )
  holm <- footer_lines(apa_cor(d, p_adjust = "holm"))
  expect_match(holm[1], "Holm", fixed = TRUE)
})

test_that("apa_cor() accepts the old psych::corTest() arguments with a warning", {
  expect_warning(
    tbl <- apa_cor(d_cor, adjust = "holm"),
    "p_adjust"
  )
  expect_identical(
    attr(tbl, "cortest"),
    attr(apa_cor(d_cor, p_adjust = "holm"), "cortest")
  )
  d_missing <- d_cor
  d_missing$mpg[1] <- NA
  expect_warning(
    tbl <- apa_cor(d_missing, use = "complete"),
    "na.omit"
  )
  expect_equal(unname(attr(tbl, "cortest")$r), unname(cor(stats::na.omit(d_missing))))
  expect_error(apa_cor(d_cor, y = d_cor), "two sets")
  expect_error(apa_cor(d_cor[1]), "at least two")
})

test_that("apa_cor() correlates logical variables as 0 and 1", {
  d <- data.frame(x = c(TRUE, FALSE, TRUE, TRUE, FALSE), y = 1:5, z = c(2, 1, 4, 3, 5))
  ct <- attr(apa_cor(d), "cortest")
  expect_equal(unname(ct$r), unname(cor(sapply(d, as.numeric))))
})

test_that("correlations line up on the decimal point without star columns", {
  for (args in list(list(star_significant = FALSE), list(bold_significant = TRUE))) {
    ft <- apa_flextable(do.call(apa_cor, c(list(d_cor), args)))
    values <- ft$body$dataset$`1`
    plain <- gsub("**", "", values, fixed = TRUE)
    # Positive values and the dash are padded where negative values have a
    # minus sign, so the decimal points share one position
    expect_true(all(grepl("^[ - ]+", plain[c(1, 4)])))
    expect_true(all(startsWith(plain[2:3], "−")))
    # Cells are padded to equal width and centered, like the summary columns
    # (to within the width of the narrowest padding space, in ems). Without
    # the font, padding falls back to character counts.
    if (apa7:::font_available("Times New Roman")) {
      expect_lt(diff(range(apa7:::md_text_width(values, "Times New Roman"))), .05)
    }
    expect_equal(
      unname(ft$body$styles$pars$text.align$data[2, c("1", "2", "3")]),
      rep("center", 3)
    )
  }
  # With stars, values are right-aligned against their star columns instead
  ft <- apa_flextable(apa_cor(d_cor))
  expect_false(any(grepl("[ - ]", ft$body$dataset$`1`)))
})

test_that("summary columns line up on the decimal point", {
  ft <- apa_flextable(apa_cor(
    mtcars[, c("mpg", "hp", "wt")],
    summary_functions = c("M", "SD", "Skewness", "Kurtosis")
  ))
  for (k in c("*M*", "*SD*", "Skewness", "Kurtosis")) {
    values <- ft$body$dataset[[k]]
    expect_lt(diff(range(apa7:::md_text_width(values, "Times New Roman"))), .05, label = k)
  }
  # Negative and positive skewness share the decimal position
  d <- data.frame(x = c(1, 2, 3, 10), y = c(10, 9, 8, 1), z = c(1, 3, 2, 4))
  sk <- apa_cor(d, summary_functions = c("Skewness"))$Skewness
  expect_true(any(startsWith(sk, "−")))
  expect_true(any(grepl("^[ - ]", sk)))
})
