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

  fit_rr <- structure(list(family = binomial(link = "log")), class = c("glm", "lm"))
  expect_equal(exp_coefficient_label(fit_rr)[["abbreviation"]], "risk ratio")
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
    paste0(
      "Degrees of freedom are Kenward", intToUtf8(8211), "Roger approximations. ",
      "*SD* = standard deviation; *r* = correlation."
    )
  )

  m0 <- lme4::lmer(Reaction ~ 1 + (1 | Subject), data = lme4::sleepstudy)
  notes <- apa_note(apa_parameters(list(m0, m1)), collapse = NULL)
  expect_match(notes[1], "Kenward", fixed = TRUE)
  expect_match(notes[2], "^[*]SD[*] = standard deviation; [*]r[*] = correlation; [*]R[*]\\^2\\^ = variance explained by fixed effects;")
  expect_match(notes[2], "variance explained by fixed and random effects", fixed = TRUE)
  expect_equal(notes[3], star_note)
})

test_that("apa_note describes satterthwaite df", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("lmerTest")
  m1 <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  expect_equal(
    apa_note(apa_parameters(m1, ci_method = "satterthwaite")),
    "Degrees of freedom are Satterthwaite approximations. *SD* = standard deviation; *r* = correlation."
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

# apa_flextable(note = ...) ----
footer_text <- function(ft) {
  chunks <- flextable::information_data_chunk(ft)
  paste(chunks$txt[chunks$.part == "footer"], collapse = "")
}

test_that("apa_flextable adds the generated note with note = TRUE", {
  tbl <- apa_parameters(fit_lm, starred = "p")
  ft <- apa_flextable(tbl, note = TRUE)
  expect_equal(flextable::nrow_part(ft, "footer"), 1)
  text <- footer_text(ft)
  expect_true(startsWith(text, "Note. "))
  expect_match(text, "standardized coefficient", fixed = TRUE)
  expect_match(text, "p.<.[.]05")

  # The note does not change column widths
  expect_equal(ft$body$colwidths, apa_flextable(tbl)$body$colwidths)

  # The note is left aligned
  expect_equal(unique(ft$footer$styles$pars$text.align$data[1, ]), "left")

  # The note is single-spaced, even when the table is double-spaced
  expect_equal(unique(ft$footer$styles$pars$line_spacing$data[1, ]), 1)
})

test_that("apa_flextable uses custom note text", {
  tbl <- apa_parameters(fit_lm)
  ft <- apa_flextable(tbl, note = "Data are from *mtcars*.")
  expect_equal(footer_text(ft), "Note. Data are from mtcars.")

  # Works for any data frame, and several strings are joined
  ft2 <- apa_flextable(mtcars[1:3, 1:3], note = c("First.", "Second."))
  expect_equal(footer_text(ft2), "Note. First. Second.")
})

test_that("apa_flextable adds no note by default or when there is nothing to say", {
  tbl <- apa_parameters(fit_lm)
  expect_equal(flextable::nrow_part(apa_flextable(tbl), "footer"), 0)
  expect_equal(flextable::nrow_part(apa_flextable(tbl, note = FALSE), "footer"), 0)
  expect_equal(flextable::nrow_part(apa_flextable(tbl, note = ""), "footer"), 0)

  # The generated note is empty when no column needs a definition
  no_std <- apa_parameters(
    fit_lm,
    predictor_parameters = c("Parameter", "Coefficient", "SE", "t", "df_error", "p")
  )
  expect_equal(flextable::nrow_part(apa_flextable(no_std, note = TRUE), "footer"), 0)
})

test_that("apa_flextable note errors are informative", {
  expect_error(apa_flextable(mtcars[1:3, 1:3], note = TRUE), "needs a table made by")
  expect_error(apa_flextable(apa_parameters(fit_lm), note = 3), "must be")
})

test_that("apa_flextable notes work for ANOVA and loadings tables", {
  fit <- aov(mpg ~ cyl * am, data = transform(mtcars, cyl = factor(cyl), am = factor(am)))
  expect_match(footer_text(apa_flextable(apa_anova(fit), note = TRUE)), "^Note[.] Sums of squares")

  fit_fa <- psych::fa(Harman74.cor$cov, 4, fm = "pa", rotate = "varimax")
  tbl <- apa_loadings(fit_fa, variance = TRUE)
  ft <- apa_flextable(tbl, note = TRUE)
  expect_match(footer_text(ft), "Principal axis factoring with varimax rotation.", fixed = TRUE)
  # The rule above the variance rows is still drawn
  expect_true(ft$body$styles$cells$border.width.bottom$data[24, 1] > 0)
})

# column_format abbreviations ----
test_that("column formats carry abbreviations", {
  cf <- column_formats()
  expect_equal(cf$Std_Coefficient@abbreviation, "standardized coefficient")
  expect_equal(cf$p@abbreviation, character(0))
  expect_equal(cf@get_abbreviations[["CI"]], "confidence interval")
  expect_equal(cf@get_abbreviations[["*AIC*"]], "Akaike information criterion")
  expect_error(
    column_format("x", header = "x", latex = "x", formatter = identity, abbreviation = c("a", "b")),
    "single string"
  )
  expect_equal(
    tibble::as_tibble(cf$Std_Coefficient)$abbreviation,
    "&beta; = standardized coefficient"
  )
})

test_that("column_abbreviations names terms by header and drops duplicates", {
  cf <- column_formats()
  a <- column_abbreviations(c("Parameter", "Std_Coefficient", "CI", "Exp_CI", "p"), cf)
  expect_equal(names(a), c("&beta;", "CI"))
  expect_equal(abbreviation_sentence(a), "&beta; = standardized coefficient; CI = confidence interval.")
  expect_null(abbreviation_sentence(character(0)))
})

test_that("custom abbreviations appear in notes for any table type", {
  cf <- column_formats()
  cf$SE@abbreviation <- "standard error"
  cf$Std_Coefficient@abbreviation <- "standardized regression coefficient"
  x <- apa_parameters(fit_lm, column_formats = cf)
  expect_equal(
    apa_note(x),
    "*SE* = standard error; &beta; = standardized regression coefficient."
  )

  # A named abbreviation defines a term other than the header
  cf2 <- column_formats()
  cf2$t@abbreviation <- c(`*t*` = "Student's *t*")
  x2 <- apa_parameters(fit_lm, column_formats = cf2)
  expect_match(apa_note(x2), "*t* = Student's *t*", fixed = TRUE)

  # ANOVA
  d <- transform(mtcars, cyl = factor(cyl), am = factor(am))
  cf3 <- column_formats()
  cf3$Sum_Squares@abbreviation <- "sum of squares"
  tbl <- apa_anova(
    aov(mpg ~ cyl * am, data = d),
    anova_parameters = c("Parameter", "Sum_Squares", "df", "F", "p", "Effect_Size"),
    column_formats = cf3
  )
  expect_match(apa_note(tbl), "*SS* = sum of squares; *&eta;*^2^~p~ = partial eta squared.", fixed = TRUE)
})

test_that("loadings notes use column format abbreviations", {
  fit_fa <- psych::fa(Harman74.cor$cov, 2, fm = "pa", rotate = "varimax")
  tbl <- apa_loadings(fit_fa, communality = TRUE, complexity = TRUE)
  expect_match(
    apa_note(tbl),
    "*h*^2^ = communality; Complexity = Hofmann's complexity index.",
    fixed = TRUE
  )
})

test_that("table notes are single-spaced in all table functions", {
  footer_spacing <- function(ft) {
    unique(as.vector(ft$footer$styles$pars$line_spacing$data))
  }
  # apa_chisq() note, styled by apa_style()
  expect_equal(footer_spacing(apa_chisq(mtcars[, c("am", "vs")])), 1)
  # apa_cor() note
  expect_equal(footer_spacing(apa_cor(mtcars[, 1:3])), 1)
  # apa_style() footer added by the user
  ft <- flextable::flextable(head(mtcars[, 1:3])) |>
    flextable::add_footer_lines("A note.") |>
    apa_style(line_spacing = 2)
  expect_equal(footer_spacing(ft), 1)
})
