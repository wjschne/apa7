skip_if_not_installed("nFactors")
skip_on_cran()

d_items <- na.omit(psych::bfi[, 1:10])
nf <- suppressWarnings(parameters::n_factors(d_items))

test_that("apa_n_factors() makes a row per method", {
  result <- apa_n_factors(nf)
  expect_s3_class(result, "apa7_tbl")
  expect_identical(colnames(result), c("Method", "Factors"))
  expect_equal(nrow(result), nrow(as.data.frame(nf)))
  # Method names are spelled out and the labels are not re-formatted
  expect_true("Velicer's minimum average partial (MAP)" %in% result$Method)
  expect_false("Velicer's MAP" %in% result$Method)
  expect_identical(attr(result, "no_format_columns"), "Method")
  # The numbers supported by the most methods are bolded
  counts <- table(as.data.frame(nf)$n_Factors)
  consensus <- names(counts)[counts == max(counts)]
  bolded <- unique(gsub("\\*", "", result$Factors[grepl("^\\*\\*", result$Factors)]))
  expect_setequal(bolded, consensus)
  expect_s3_class(apa_flextable(result), "flextable")
})

test_that("apa_n_factors() keeps method names and drops bolding on request", {
  result <- apa_n_factors(nf, format_method_names = FALSE, bold_consensus = FALSE)
  expect_identical(result$Method, as.data.frame(nf)$Method)
  expect_false(any(grepl("\\*\\*", result$Factors)))
  expect_false(grepl("bolded", apa_note(result)))
})

test_that("apa_n_factors() makes a row per suggested number of factors", {
  result <- apa_n_factors(nf, table = "agreement")
  expect_identical(colnames(result), c("Factors", "Methods", "Cumulative variance"))
  counts <- table(as.data.frame(nf)$n_Factors)
  expect_equal(result$Factors, as.integer(names(counts)))
  expect_equal(result$Methods, as.integer(counts))
  expect_equal(sum(result$Methods), nrow(as.data.frame(nf)))
  expect_true(all(grepl("^\\s*\\.\\d\\d", result[["Cumulative variance"]])))
  note <- apa_note(result)
  expect_match(note, "Methods = number of methods")
  expect_match(note, "Cumulative variance = proportion of variance")
})

test_that("apa_n_factors() notes the number supported by the most methods", {
  result <- apa_n_factors(nf)
  note <- apa_note(result)
  counts <- table(as.data.frame(nf)$n_Factors)
  consensus <- names(counts)[counts == max(counts)]
  expect_match(note, paste0("of ", nrow(as.data.frame(nf)), " methods"))
  expect_true(all(vapply(consensus, \(k) grepl(k, note, fixed = TRUE), logical(1))))
  expect_match(note, "bolded")
})

test_that("apa_n_factors() runs n_factors() on a data frame", {
  result <- suppressWarnings(apa_n_factors(d_items, package = "psych"))
  expect_true(all(grepl("Very simple structure|minimum average partial|BIC", result$Method)))
  # Components with type = "PCA"
  pca <- suppressWarnings(apa_n_factors(d_items, type = "PCA", package = "psych"))
  expect_identical(colnames(pca), c("Method", "Components"))
  expect_match(apa_note(pca), "components")
  expect_identical(colnames(apa_n_factors(nf, components = TRUE)), c("Method", "Components"))
})

test_that("apa_n_factors() checks its arguments", {
  expect_error(apa_n_factors(1:3), "data frame")
  expect_error(apa_n_factors(nf, table = "rows"), "should be one of")
  expect_error(apa_n_factors(nf, bold_consensus = NA), "bold_consensus")
  expect_error(apa_n_factors(nf, components = "yes"), "components")
})

test_that("n_factors_note_sentences() writes ties and single numbers", {
  single <- n_factors_note_sentences(list(
    unit = "factors", consensus = 5L, consensus_methods = c("A", "B"),
    n_methods = 4, layout = "methods", bolded = TRUE
  ))
  expect_identical(
    single,
    c(
      "The number of factors supported by the most methods is 5, by 2 of 4 methods: A and B.",
      "The number of factors supported by the most methods is bolded."
    )
  )
  tie <- n_factors_note_sentences(list(
    unit = "factors", consensus = c(3L, 5L), consensus_methods = c("A", "B", "C", "D"),
    n_methods = 6, layout = "methods", bolded = FALSE
  ))
  expect_identical(tie, "The numbers of factors supported by the most methods are 3 and 5, each by 2 of 6 methods.")
})
