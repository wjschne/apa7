library(apa7)
library(testthat)

skip_if_not_installed("pscl")

unpad <- function(x) gsub("[\u2002-\u200a ]", "", x)
em <- "\u2003"

biochemists <- function() {
  e <- new.env()
  utils::data("bioChemists", package = "pscl", envir = e)
  e$bioChemists
}

test_that("zero-inflated models get count and zero-inflation blocks", {
  d <- biochemists()
  fit <- pscl::zeroinfl(art ~ fem + kid5 + ment | ment, data = d, dist = "negbin")
  result <- apa_parameters(fit)
  expect_s3_class(result, "apa7_tbl")
  expect_equal(colnames(result), c("Variable", "*B*", "*SE*", "*IRR*", "*z*", "*p*"))
  expect_equal(result$Variable[c(1, 6)], c("Count model", "Zero-inflation model"))
  expect_equal(result$Variable[2], paste0(em, "Constant"))

  b <- coef(fit)
  expect_equal(unpad(result$`*IRR*`[3]), sprintf("%.2f", exp(b[["count_femWomen"]])))
  # The zero model is not exponentiated
  expect_equal(unpad(result$`*IRR*`[7:8]), c("", ""))
  expect_equal(
    unpad(result$`*B*`[8]),
    sub("-", "\u2212", sprintf("%.2f", b[["zero_ment"]]))
  )

  note <- apa_note(result)
  expect_match(note, "The count model is negative binomial with dispersion parameter *\u03b8* = 2.70", fixed = TRUE)
  expect_match(note, "Zero-inflation coefficients are log odds of an excess zero", fixed = TRUE)
  expect_match(note, "*IRR* = incidence rate ratio", fixed = TRUE)
  expect_equal(apa_flextable(result)$body$dataset[[1]], result$Variable)

  raw <- apa_parameters(fit, exponentiate = FALSE)
  expect_false("*IRR*" %in% colnames(raw))
})

test_that("hurdle models get a zero hurdle block and their own note", {
  d <- biochemists()
  fit <- pscl::hurdle(art ~ fem + kid5 + ment | ment, data = d)
  result <- apa_parameters(fit)
  expect_equal(result$Variable[c(1, 6)], c("Count model", "Zero hurdle model"))
  note <- apa_note(result)
  expect_match(note, "The count model is zero-truncated Poisson, and", fixed = TRUE)
  expect_match(note, "Zero hurdle coefficients are log odds of a count greater than zero.", fixed = TRUE)
  expect_no_match(note, "\u03b8", fixed = TRUE)
})

test_that("performance and comparisons of zero-inflated models use LR tests", {
  d <- biochemists()
  fit1 <- pscl::zeroinfl(art ~ fem + kid5 | ment, data = d, dist = "negbin")
  fit2 <- pscl::zeroinfl(art ~ fem + kid5 + ment | ment, data = d, dist = "negbin")

  perf <- suppressMessages(apa_performance(fit2))
  expect_equal(colnames(perf), c("*R*^2^", "*AIC*"))

  comparison <- suppressMessages(apa_performance_comparison(list(fit1, fit2)))
  expect_true("*&chi;*^2^" %in% colnames(comparison))
  lrt <- 2 * (as.numeric(logLik(fit2)) - as.numeric(logLik(fit1)))
  expect_equal(unpad(comparison$`*&chi;*^2^`[2]), sprintf("%.2f", lrt))

  both <- suppressMessages(apa_parameters(list(fit1, fit2)))
  note <- apa_note(both)
  expect_match(note, "The count model is negative binomial, and", fixed = TRUE)
})
