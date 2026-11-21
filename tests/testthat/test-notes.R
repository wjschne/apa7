# apa_note for apa_parameters (regression) tables. ANOVA notes are tested in
# test-anova.R and factor analysis notes in test-loadings.R.

fit_lm <- lm(mpg ~ cyl + wt, data = mtcars)
star_note <- paste0(apa_p_star_note(), ".")

test_that("apa_note describes lm tables", {
  expect_equal(
    apa_note(apa_parameters(fit_lm)),
    "&beta; = standardized coefficient."
  )

  starred <- apa_note(apa_parameters(fit_lm, starred = "p"), collapse = NULL)
  expect_equal(starred, c("&beta; = standardized coefficient.", star_note))

  bolded <- apa_note(apa_parameters(fit_lm, bolded = "Parameter"), collapse = NULL)
  expect_match(bolded[2], "^Bold values are statistically significant")

  # Without the standardized coefficient, nothing needs defining
  no_std <- apa_parameters(
    fit_lm,
    predictor_parameters = c("Parameter", "Coefficient", "SE", "t", "df_error", "p")
  )
  expect_equal(apa_note(no_std), "")
  expect_length(apa_note(no_std, collapse = NULL), 0)
})

test_that("apa_note describes robust standard errors and intervals", {
  skip_if_not_installed("sandwich")
  x <- apa_parameters(
    fit_lm,
    predictor_parameters = c("Parameter", "Coefficient", "SE", "CI", "t", "df_error", "p"),
    vcov = "HC3"
  )
  expect_equal(
    apa_note(x, collapse = NULL),
    c(
      "Standard errors are heteroskedasticity-consistent (HC3).",
      "CI = confidence interval."
    )
  )
})

test_that("describe_vcov names covariance estimators", {
  expect_null(describe_vcov(NULL))
  expect_equal(describe_vcov("HC1"), "Standard errors are heteroskedasticity-consistent (HC1).")
  expect_equal(describe_vcov("vcovHC"), "Standard errors are heteroskedasticity-consistent.")
  expect_equal(describe_vcov("CR2"), "Standard errors are cluster-robust (CR2).")
  expect_equal(describe_vcov("vcovCL"), "Standard errors are cluster-robust.")
  expect_match(describe_vcov("vcovHAC"), "autocorrelation-consistent", fixed = TRUE)
  expect_equal(describe_vcov("vcovBS"), "Standard errors are bootstrapped.")
  expect_equal(describe_vcov("other"), "Standard errors are robust (other).")
  expect_match(describe_vcov(diag(2)), "user-supplied", fixed = TRUE)
})

test_that("apa_note defines exponentiated coefficients", {
  fit_logit <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  x <- apa_parameters(
    fit_logit,
    predictor_parameters = c("Parameter", "Coefficient", "SE", "Exp_Coefficient", "Exp_CI", "z", "p")
  )
  expect_equal(apa_note(x), "*OR* = odds ratio; CI = confidence interval.")

  fit_pois <- glm(carb ~ wt, data = mtcars, family = poisson)
  expect_equal(apa_note(apa_parameters(fit_pois)), "*IRR* = incidence rate ratio.")

  fit_probit <- glm(am ~ wt, data = mtcars, family = binomial(link = "probit"))
  expect_equal(apa_note(apa_parameters(fit_probit, exponentiate = TRUE)), "Exp(*B*) = exponentiated coefficient.")

  expect_equal(exp_coefficient_definition("*RR*"), "risk ratio")
})

test_that("apa_note describes ordinal models", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("carData")
  fit <- MASS::polr(poverty ~ gender + age, data = carData::WVS, Hess = TRUE)
  expect_equal(
    apa_note(apa_parameters(fit), collapse = NULL),
    c(
      "Thresholds (cutpoints) are on the logit scale and are not tested.",
      "*OR* = cumulative odds ratio."
    )
  )
  expect_equal(
    apa_note(apa_parameters(fit, thresholds = FALSE)),
    "*OR* = cumulative odds ratio."
  )

  fit_probit <- MASS::polr(poverty ~ gender + age, data = carData::WVS, Hess = TRUE, method = "probit")
  expect_match(apa_note(apa_parameters(fit_probit)), "on the probit scale", fixed = TRUE)
})

test_that("apa_note describes mixed models", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("pbkrtest")
  m1 <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  expect_equal(
    apa_note(apa_parameters(m1)),
    paste0("Degrees of freedom are Kenward", intToUtf8(8211), "Roger approximations.")
  )

  m0 <- lme4::lmer(Reaction ~ 1 + (1 | Subject), data = lme4::sleepstudy)
  notes <- apa_note(apa_parameters(list(m0, m1)), collapse = NULL)
  expect_match(notes[1], "Kenward", fixed = TRUE)
  expect_match(notes[2], "^[*]R[*]\\^2\\^ = variance explained by fixed effects;")
  expect_match(notes[2], "variance explained by fixed and random effects", fixed = TRUE)
  expect_equal(notes[3], star_note)
})

test_that("apa_note describes satterthwaite df", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("lmerTest")
  m1 <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  expect_equal(
    apa_note(apa_parameters(m1, ci_method = "satterthwaite")),
    "Degrees of freedom are Satterthwaite approximations."
  )
})

test_that("apa_note describes lists of models", {
  f1 <- lm(mpg ~ wt, data = mtcars)
  f2 <- lm(mpg ~ wt + hp, data = mtcars)
  expect_equal(
    apa_note(apa_parameters(list(f1, f2)), collapse = NULL),
    c("&beta; = standardized coefficient; &Delta;*R*^2^ = change in *R*^2^.", star_note)
  )
  # No model comparison columns, no stars
  expect_equal(
    apa_note(apa_parameters(list(f1, f2), performance_parameters = NULL, starred = NA)),
    "&beta; = standardized coefficient."
  )
})

test_that("ANOVA tables keep their own notes", {
  fit <- aov(mpg ~ factor(cyl), data = mtcars)
  expect_match(apa_note(apa_anova(fit)), "^Sums of squares are sequential")
})
