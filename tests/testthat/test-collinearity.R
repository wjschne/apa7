library(apa7)
library(testthat)

unpad <- function(x) gsub("[\u2002-\u200a ]", "", x)

test_that("apa_collinearity shows VIF, its CI, and tolerance", {
  fit <- lm(mpg ~ wt + hp + disp + factor(cyl), data = mtcars)
  result <- apa_collinearity(fit)
  expect_s3_class(result, "apa7_tbl")
  expect_equal(colnames(result), c("Variable", "*VIF*", "95% CI for *VIF*", "Tolerance"))
  expect_equal(result$Variable, c("Wt", "Hp", "Disp", "Cyl"))

  cc <- performance::check_collinearity(fit)
  expect_equal(unpad(result$`*VIF*`), sprintf("%.2f", cc$VIF))
  expect_equal(unpad(result$Tolerance[1]), sub("^0", "", sprintf("%.2f", cc$Tolerance[1])))

  note <- apa_note(result)
  expect_match(note, "For Cyl, which has more than 1 *df*, *VIF* is the generalized *VIF*.", fixed = TRUE)
  expect_match(note, "Tolerance = 1/*VIF*", fixed = TRUE)
  expect_match(note, "*VIF* = variance inflation factor; CI = confidence interval.", fixed = TRUE)
  expect_no_match(note, "bolded", fixed = TRUE)
  expect_equal(apa_flextable(result)$body$dataset[[1]], result$Variable)
})

test_that("columns, flags, interactions, and names", {
  fit <- lm(mpg ~ wt * hp, data = mtcars)
  result <- apa_collinearity(
    fit,
    columns = c("VIF", "SE_factor", "Tolerance_CI"),
    flag = 15
  )
  expect_equal(colnames(result), c("Variable", "*VIF*", "&radic;*VIF*", "95% CI for tolerance"))
  expect_equal(result$Variable[3], "Wt \u00d7 Hp")
  expect_false(grepl("**", result$`*VIF*`[1], fixed = TRUE))
  expect_true(all(grepl("**", result$`*VIF*`[2:3], fixed = TRUE)))
  note <- apa_note(result)
  expect_match(note, "*VIF*s of 15 or more are bolded.", fixed = TRUE)
  expect_match(note, "interaction terms and their components", fixed = TRUE)
  expect_match(note, "Tolerance =", fixed = TRUE)
  expect_match(note, "CI = confidence interval", fixed = TRUE)
  vif_only <- apa_note(apa_collinearity(fit, columns = "VIF"))
  expect_no_match(vif_only, "Tolerance =", fixed = TRUE)
  expect_no_match(vif_only, "CI = confidence interval", fixed = TRUE)

  # Centered predictors: the interaction does not inflate the VIFs
  centered <- transform(mtcars, wt = wt - mean(wt), hp = hp - mean(hp))
  low <- apa_collinearity(lm(mpg ~ wt * hp, data = centered))
  expect_no_match(apa_note(low), "interaction", fixed = TRUE)

  raw <- apa_collinearity(fit, format_variable_names = FALSE)
  expect_equal(raw$Variable, c("wt", "hp", "wt:hp"))

  # Factors with more than 2 levels use GVIF^(1/(2df))
  g <- apa_collinearity(lm(mpg ~ wt + factor(cyl), data = mtcars), columns = c("VIF", "SE_factor"))
  expect_equal(colnames(g)[3], "*VIF*^1/(2*df*)^")
  expect_match(apa_note(g), "comparable across terms with different *df*", fixed = TRUE)

  # check_collinearity() output is accepted
  cc <- performance::check_collinearity(lm(mpg ~ wt + hp, data = mtcars))
  expect_equal(apa_collinearity(cc)$Variable, c("Wt", "Hp"))
})

test_that("models with several parts get headings", {
  skip_if_not_installed("pscl")
  e <- new.env()
  utils::data("bioChemists", package = "pscl", envir = e)
  fit <- pscl::zeroinfl(art ~ fem + kid5 + ment | ment + kid5, data = e$bioChemists)
  result <- apa_collinearity(fit)
  expect_equal(result$Variable[c(1, 5)], c("Count model", "Zero-inflation model"))
  expect_equal(result$Variable[6], "\u2003Ment")
})

test_that("apa_collinearity gives clear errors", {
  expect_error(apa_collinearity(lm(mpg ~ wt, data = mtcars)), "at least two predictors")
  expect_error(apa_collinearity(lm(mpg ~ wt + hp, data = mtcars), columns = "zz"), "not \"zz\"")
  expect_error(apa_collinearity(lm(mpg ~ wt + hp, data = mtcars), flag = "high"), "single number")
})
