library(apa7)
library(testthat)

tmb_data <- function() {
  e <- new.env()
  utils::data("Salamanders", package = "glmmTMB", envir = e)
  e$Salamanders
}

test_that("apa_parameters.glmmTMB matches glmer-style tables for one-part models", {
  skip_if_not_installed("glmmTMB")
  fit <- glmmTMB::glmmTMB(
    count ~ mined + (1 | site),
    family = glmmTMB::nbinom2,
    data = tmb_data()
  )
  result <- apa_parameters(fit)
  expect_equal(
    colnames(result),
    c("Variable", "*B*", "*SE*", "*IRR*", "*z*", "*p*")
  )
  expect_equal(result$Variable, c("Constant", "Mined–No", "Site: *SD* (Constant)"))
  b <- glmmTMB::fixef(fit)$cond
  expect_equal(as_num(result$`*B*`[1:2]), unname(round(b, 2)))
  expect_equal(as_num(result$`*IRR*`[1:2]), unname(round(exp(b), 2)))
  # The unmodeled dispersion parameter is not shown
  expect_false(any(grepl("Dispersion", result$Variable)))
  expect_match(apa_note(result), "*IRR* = incidence rate ratio", fixed = TRUE)
})

test_that("zero-inflation and dispersion parts get headings and notes", {
  skip_if_not_installed("glmmTMB")
  fit_zi <- glmmTMB::glmmTMB(
    count ~ mined + (1 | site),
    ziformula = ~ mined,
    family = poisson,
    data = tmb_data()
  )
  result <- apa_parameters(fit_zi)
  expect_equal(result$Variable[c(1, 5)], c("Conditional model", "Zero-inflation model"))
  expect_true(all(startsWith(result$Variable[c(2:4, 6:7)], " ")))
  # Only the conditional model is exponentiated
  expect_true(all(trimws(result$`*IRR*`[6:7]) == ""))
  zi <- glmmTMB::fixef(fit_zi)$zi
  expect_equal(as_num(result$`*B*`[6:7]), unname(round(zi, 2)))
  expect_match(apa_note(result), "Zero-inflation coefficients are log odds", fixed = TRUE)

  fit_disp <- glmmTMB::glmmTMB(
    count ~ mined + (1 | site),
    dispformula = ~ mined,
    family = glmmTMB::nbinom2,
    data = tmb_data()
  )
  result_disp <- apa_parameters(fit_disp)
  expect_true("Dispersion model" %in% result_disp$Variable)
  expect_match(apa_note(result_disp), "Dispersion coefficients are on the log scale.", fixed = TRUE)
})

test_that("Gaussian glmmTMB models show random effects and no exponentiated column", {
  skip_if_not_installed("glmmTMB")
  fit <- glmmTMB::glmmTMB(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  result <- apa_parameters(fit)
  expect_false("*IRR*" %in% colnames(result))
  expect_true(all(c("Subject: *r* (Constant, Days)", "Residual *SD*") %in% result$Variable))
})

test_that("apa_performance and model comparisons accept glmmTMB models", {
  skip_if_not_installed("glmmTMB")
  fit1 <- glmmTMB::glmmTMB(count ~ mined + (1 | site), family = glmmTMB::nbinom2, data = tmb_data())
  fit2 <- stats::update(fit1, . ~ . + spp)
  expect_equal(
    colnames(apa_performance(fit1)),
    c("*R*^2^~marginal~", "*R*^2^~conditional~", "ICC")
  )
  cmp <- apa_performance_comparison(fit1, fit2)
  expect_true(all(c("*&chi;*^2^", "*R*^2^~conditional~") %in% colnames(cmp)))
  tbl <- apa_parameters(list(fit1, fit2))
  expect_true("*R*^2^~conditional~" %in% colnames(tbl))
})

test_that("apa_flextable keeps mixed-model labels and part headings as made", {
  skip_if_not_installed("glmmTMB")
  fit_zi <- glmmTMB::glmmTMB(
    count ~ mined + (1 | site),
    ziformula = ~ mined,
    family = poisson,
    data = tmb_data()
  )
  tbl <- apa_parameters(fit_zi)
  expect_equal(apa_flextable(tbl)$body$dataset[[1]], tbl$Variable)

  fit_lmer <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  labels <- apa_flextable(apa_parameters(fit_lmer))$body$dataset[[1]]
  expect_true("Subject: *r* (Constant, Days)" %in% labels)
  # Also in lists of models
  fit_lmer0 <- lme4::lmer(Reaction ~ 1 + (1 | Subject), data = lme4::sleepstudy)
  labels_list <- apa_flextable(apa_parameters(list(fit_lmer0, fit_lmer)))$body$dataset[[2]]
  expect_true("Subject: *SD* (Constant)" %in% labels_list)
})
