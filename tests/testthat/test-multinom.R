library(apa7)
library(testthat)

skip_if_not_installed("nnet")
skip_if_not_installed("carData")

unpad <- function(x) gsub("[\u2002-\u200a ]", "", x)
em <- "\u2003"

womenlf <- function() {
  d <- carData::Womenlf
  d$partic <- factor(
    d$partic,
    levels = c("not.work", "parttime", "fulltime"),
    labels = c("Not working", "Part time", "Full time")
  )
  d
}

test_that("multinomial models get a block of coefficients for each category", {
  d <- womenlf()
  fit <- nnet::multinom(partic ~ hincome + children, data = d, trace = FALSE)
  result <- apa_parameters(fit)
  expect_s3_class(result, "apa7_tbl")
  expect_equal(colnames(result), c("Variable", "*B*", "*SE*", "*OR*", "*z*", "*p*"))
  expect_equal(result$Variable[c(1, 5)], c("Part time vs. Not working", "Full time vs. Not working"))
  expect_equal(result$Variable[2], paste0(em, "Constant"))
  expect_equal(result$`*B*`[1], "")

  b <- coef(fit)
  expect_equal(unpad(result$`*B*`[7]), sub("-", "\u2212", sprintf("%.2f", b["Full time", "hincome"])))
  expect_equal(unpad(result$`*OR*`[8]), sprintf("%.2f", exp(b["Full time", "childrenpresent"])))

  note <- apa_note(result)
  expect_match(
    note,
    "Coefficients compare each category of Partic with the reference category (Not working).",
    fixed = TRUE
  )
  expect_match(note, "*OR* = odds ratio (relative to the reference category)", fixed = TRUE)
  expect_equal(apa_flextable(result)$body$dataset[[1]], result$Variable)
})

test_that("category labels instead of headings, CIs, and no exponentiation", {
  d <- womenlf()
  fit <- nnet::multinom(partic ~ hincome + children, data = d, trace = FALSE)
  flat <- apa_parameters(
    fit,
    category_headings = FALSE,
    predictor_parameters = c("Parameter", "Coefficient", "SE", "Exp_Coefficient", "Exp_CI", "z", "p")
  )
  expect_equal(nrow(flat), 6)
  expect_equal(flat$Variable[1], "Part time: Constant")
  expect_true("95% CI for *OR*" %in% colnames(flat))

  raw <- apa_parameters(fit, exponentiate = FALSE)
  expect_false("*OR*" %in% colnames(raw))
  expect_match(apa_note(raw), "on the log-odds scale", fixed = TRUE)

  # Two categories give one block
  d2 <- droplevels(d[d$partic != "Full time", ])
  two <- apa_parameters(nnet::multinom(partic ~ hincome, data = d2, trace = FALSE))
  expect_equal(two$Variable[1], "Part time vs. Not working")
  expect_equal(nrow(two), 3)
})

test_that("performance and comparisons of multinomial models use McFadden's R2 and LR tests", {
  d <- womenlf()
  fit1 <- nnet::multinom(partic ~ hincome, data = d, trace = FALSE)
  fit2 <- nnet::multinom(partic ~ hincome + children, data = d, trace = FALSE)

  perf <- suppressMessages(apa_performance(fit2))
  expect_equal(colnames(perf), c("*R*^2^~McFadden~", "*AIC*"))

  comparison <- suppressMessages(apa_performance_comparison(list(fit1, fit2)))
  expect_true(all(c("*R*^2^~McFadden~", "*&chi;*^2^") %in% colnames(comparison)))
  lrt <- anova(fit1, fit2)
  expect_equal(unpad(comparison$`*&chi;*^2^`[2]), sprintf("%.2f", lrt$`LR stat.`[2]))

  both <- suppressMessages(apa_parameters(list(fit1, fit2)))
  expect_match(apa_note(both), "reference category (Not working)", fixed = TRUE)
})
