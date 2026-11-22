library(apa7)
library(testthat)

test_that("apa_parameters.lme shows fixed effects with nlme's df and random effects", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(Reaction ~ Days, random = ~ Days | Subject, data = lme4::sleepstudy)
  result <- apa_parameters(fit)
  expect_equal(colnames(result), c("Variable", "*B*", "*SE*", "*t*(161)", "*p*"))
  expect_equal(
    result$Variable,
    c(
      "Fixed effects",
      paste0("\u2003", c("Constant", "Days")),
      "Random effects (*SD*)",
      "\u2003Subject",
      paste0("\u2003\u2003", c("Constant", "Days", "*r* (Constant, Days)")),
      "\u2003Residual"
    )
  )
  tt <- summary(fit)$tTable
  expect_equal(as_num(result$`*B*`[2:3]), unname(round(tt[, "Value"], 2)))
  expect_equal(as_num(result$`*t*(161)`[2:3]), unname(round(tt[, "t-value"], 2)))
  expect_match(apa_note(result), "computed by nlme from the grouping levels", fixed = TRUE)
  # Labels are not formatted again in flextables
  expect_equal(apa_flextable(result)$body$dataset[[1]], result$Variable)
})

test_that("residual correlation structures get rows and a note", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(
    distance ~ age + Sex,
    random = ~ 1 | Subject,
    correlation = nlme::corAR1(),
    data = nlme::Orthodont
  )
  result <- apa_parameters(fit)
  # df differ across fixed effects, so they get a column
  expect_true("*df*" %in% colnames(result))
  phi <- stats::coef(fit$modelStruct$corStruct, unconstrained = FALSE)
  row <- result$Variable == paste0("\u2003", "Residual autocorrelation (*φ*)")
  expect_equal(as_num(result$`*B*`[row]), unname(round(phi, 2)))
  expect_match(
    apa_note(result),
    "Residuals within Subject have a first-order autoregressive (AR(1)) correlation structure.",
    fixed = TRUE
  )
})

test_that("variance functions get rows and a note", {
  skip_if_not_installed("nlme")
  fit_ident <- nlme::lme(
    distance ~ age + Sex,
    random = ~ 1 | Subject,
    weights = nlme::varIdent(form = ~ 1 | Sex),
    data = nlme::Orthodont
  )
  result <- apa_parameters(fit_ident)
  ratio <- stats::coef(fit_ident$modelStruct$varStruct, unconstrained = FALSE)
  row <- result$Variable == paste0("\u2003", "Residual *SD* ratio: Female")
  expect_equal(as_num(result$`*B*`[row]), unname(round(ratio, 2)))
  expect_match(apa_note(result), "The residual *SD* is that of Male", fixed = TRUE)

  fit_power <- nlme::lme(
    distance ~ age + Sex,
    random = ~ 1 | Subject,
    weights = nlme::varPower(form = ~ age),
    correlation = nlme::corCompSymm(form = ~ 1 | Subject),
    data = nlme::Orthodont
  )
  result_power <- apa_parameters(fit_power)
  expect_true(all(
    paste0("\u2003", c("Residual correlation (*ρ*)", "Residual *SD* power (*δ*)")) %in%
      result_power$Variable
  ))
  note <- apa_note(result_power)
  expect_match(note, "compound-symmetric correlation structure", fixed = TRUE)
  expect_match(note, "proportional to a power (*δ*) of Age", fixed = TRUE)
})

test_that("apa_performance and comparisons accept lme models", {
  skip_if_not_installed("nlme")
  m0 <- nlme::lme(distance ~ age, random = ~ 1 | Subject, data = nlme::Orthodont)
  m1 <- nlme::lme(distance ~ age + Sex, random = ~ 1 | Subject, data = nlme::Orthodont)
  expect_equal(
    colnames(apa_performance(m1)),
    c("*R*^2^~marginal~", "*R*^2^~conditional~", "ICC")
  )
  cmp <- apa_performance_comparison(m0, m1)
  expect_true(all(c("*&chi;*^2^", "*R*^2^~conditional~") %in% colnames(cmp)))
  expect_true("*R*^2^~conditional~" %in% colnames(apa_parameters(list(m0, m1))))
})

test_that("glmmPQL models get a clear error", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("MASS")
  fit <- MASS::glmmPQL(
    y ~ trt,
    random = ~ 1 | ID,
    family = binomial,
    data = MASS::bacteria,
    verbose = FALSE
  )
  expect_error(apa_parameters(fit), "glmer")
})
