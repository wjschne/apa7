library(apa7)
library(testthat)

# apa_chisq ----

test_that("apa_chisq returns an apaflextable for a valid 2-column data frame", {
  skip_on_cran()
  result <- apa_chisq(mtcars[, c("am", "gear")])
  expect_true(inherits(result, "flextable"))
  expect_warning(
    apa_chisq(
      mtcars[, c("am", "gear")],
      suppress_warnings = FALSE,
      note = "hello"
    )
  )
})

test_that("apa_chisq errors on non-data.frame input", {
  expect_error(apa_chisq(matrix(1:4, 2, 2)), "data.frame")
})

test_that("apa_chisq errors when data has more or fewer than 2 columns", {
  expect_error(apa_chisq(mtcars[, c("am", "gear", "cyl")]))
  expect_error(apa_chisq(mtcars[, "am", drop = FALSE]))
})

test_that("apa_chisq errors when contingency table is smaller than 2x2", {
  d <- data.frame(a = c(1, 1), b = c(2, 2))
  expect_error(apa_chisq(d), "2 by 2")
})

# apa_cor ----

test_that("apa_cor returns a flextable by default", {
  result <- apa_cor(
    mtcars[, c("mpg", "am", "gear")],
    keep_empty_star_columns = FALSE
  )
  expect_true(inherits(result, "flextable"))
})

test_that("apa_cor returns a tibble when output = 'tibble'", {
  result <- apa_cor(
    mtcars[, c("mpg", "am", "gear")],
    output = "tibble",
    star_significant = FALSE
  )
  expect_true(tibble::is_tibble(result) || is.data.frame(result))
})

test_that("apa_cor tibble output has expected variable column", {
  skip_on_cran()
  result <- apa_cor(
    mtcars[, c("mpg", "am", "gear")],
    output = "tibble",
    bold_significant = TRUE
  )
  expect_true("Variable" %in% colnames(result))
  result <- apa_cor(
    mtcars[, c("mpg", "am", "gear")],
    bold_significant = TRUE
  )
  expect_no_error(result)

  result <- apa_cor(
    mtcars[, c("mpg", "am", "gear")],
    bold_significant = TRUE,
    note = "My note"
  )
  expect_no_error(result)
})

test_that("apa_cor tibble output has one row per variable", {
  skip_on_cran()
  vars <- c("mpg", "am", "gear", "carb")
  result <- apa_cor(
    mtcars[, vars],
    output = "tibble",
    star_significant = FALSE
  )
  expect_equal(nrow(result), length(vars))

  expect_no_error(
    apa_cor(
      mtcars[, vars],
      keep_empty_star_columns = TRUE
    )
  )

  expect_equal(
    apa_cor(
      mtcars[, vars],
      output = "tibble",
      star_significant = FALSE,
      summary_functions = c("Mean")
    ) |>
      ncol(),
    7
  )

  expect_equal(
    apa_cor(
      mtcars[, vars],
      output = "tibble",
      star_significant = FALSE,
      summary_functions = NULL,
      keep_empty_star_columns = TRUE
    ) |>
      ncol(),
    6
  )
})

# apa_parameters ----

test_that("apa_parameters.lm returns a tibble-like object", {
  fit <- lm(mpg ~ cyl + wt, data = mtcars)
  result <- apa_parameters(fit, starred = "t", bolded = "t")
  expect_true(is.data.frame(result))
  expect_true("Variable" %in% colnames(result))
  expect_equal(nrow(result), 3L)
  fit <- list(
    lm(mpg ~ cyl, data = mtcars),
    lm(mpg ~ cyl + wt, data = mtcars)
  )
  result <- apa_parameters(fit, starred = "t", bolded = "t")
  expect_no_error(result)
})

test_that("apa_parameters.glm reports odds ratios and z for logistic models", {
  fit <- suppressWarnings(
    glm(am ~ wt + hp, data = mtcars, family = binomial)
  )
  result <- apa_parameters(fit, starred = c("Coefficient", "z"), bolded = "p")
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3L)
  expect_equal(
    colnames(result),
    c("Variable", "*B*", "*SE*", "*OR*", "*z*", "*p*")
  )
  expect_no_error(apa_flextable(result))
})

test_that("apa_parameters.glm labels exponentiated coefficients by link", {
  skip_if_not_installed("MASS")
  pois <- glm(carb ~ wt, data = mtcars, family = poisson)
  expect_true("*IRR*" %in% colnames(apa_parameters(pois)))

  nb <- suppressWarnings(MASS::glm.nb(carb ~ wt, data = mtcars))
  expect_true("*IRR*" %in% colnames(apa_parameters(nb)))

  gamma_log <- glm(mpg ~ wt, data = mtcars, family = Gamma("log"))
  expect_true("Exp(*B*)" %in% colnames(apa_parameters(gamma_log)))

  probit <- suppressWarnings(
    glm(am ~ wt, data = mtcars, family = binomial("probit"))
  )
  probit_cols <- colnames(apa_parameters(probit))
  expect_false(any(c("*OR*", "Exp(*B*)") %in% probit_cols))
  expect_true("Exp(*B*)" %in% colnames(apa_parameters(probit, exponentiate = TRUE)))
  expect_false("*IRR*" %in% colnames(apa_parameters(pois, exponentiate = FALSE)))
})

test_that("apa_parameters.glm uses t with df for estimated dispersion", {
  fit <- glm(carb ~ wt, data = mtcars, family = quasipoisson)
  result <- apa_parameters(fit)
  expect_true("*t*(30)" %in% colnames(result))
  expect_false("*z*" %in% colnames(result))
  expect_true("*IRR*" %in% colnames(result))

  gaussian <- glm(mpg ~ wt, data = mtcars)
  expect_false(any(c("*OR*", "Exp(*B*)") %in% colnames(apa_parameters(gaussian))))
})

test_that("apa_parameters.glm exponentiates the raw coefficients", {
  fit <- glm(carb ~ wt, data = mtcars, family = poisson)
  result <- apa_parameters(fit, predictor_parameters = c("Parameter", "Exp_Coefficient"))
  expect_equal(
    as.numeric(gsub("[  ]", "", result[["*IRR*"]])),
    round(exp(unname(coef(fit))), 2)
  )
})

test_that("apa_parameters.list aligns values across models", {
  skip_if_not(apa7:::font_available("Times New Roman"))
  apa7_defaults(font_family = "Times New Roman")
  # Model 1 has a negative coefficient; Model 2 has none
  fits <- list(
    lm(mpg ~ wt, data = mtcars),
    lm(mpg ~ hp, data = transform(mtcars, hp = -hp))
  )
  result <- apa_parameters(fits, performance_parameters = NULL)
  b <- result[["*B*"]][!is.na(result[["*B*"]]) & nzchar(result[["*B*"]])]
  left <- sub("\\..*$", "", b)
  w <- apa7:::md_text_width(left, "Times New Roman")
  expect_lt(diff(range(w)), 0.025)
})

test_that("apa_parameters.list handles lists of glm models", {
  skip_on_cran()
  fits <- suppressWarnings(list(
    glm(am ~ wt, data = mtcars, family = binomial),
    glm(am ~ wt + hp, data = mtcars, family = binomial)
  ))
  result <- apa_parameters(fits, performance_parameters = NULL)
  expect_true(all(c("*OR*", "*z*") %in% colnames(result)))
  expect_equal(nrow(result), 5L)

  # Default performance parameters use the pseudo-R2
  result <- apa_parameters(fits)
  expect_true(
    all(c("*R*^2^~Tjur~", "&Delta;*R*^2^~Tjur~") %in% colnames(result))
  )
  expect_no_error(apa_flextable(result))
})

# apa_performance ----

test_that("apa_performance.lm returns a data frame", {
  fit <- lm(mpg ~ cyl + wt, data = mtcars)
  result <- apa_performance(fit)
  expect_true(is.data.frame(result))
  expect_equal(ncol(apa_performance(fit, metrics = "all")), 7L)

  m1 <- lm(mpg ~ cyl, data = mtcars)
  m2 <- lm(mpg ~ cyl + wt, data = mtcars)
  expect_equal(
    ncol(apa_performance_comparison(list(`Model 1` = m1, `Model 3` = m2))),
    5L
  )

  expect_equal(
    ncol(
      apa_performance_comparison(
        list(`Model 1` = m1, `Model 3` = m2),
        metrics = "all"
      )
    ),
    12L
  )

  expect_equal(
    ncol(
      apa_performance_comparison(
        list(`Model 1` = m1, `Model 3` = m2),
        starred = "deltaR2"
      )
    ),
    5L
  )
})

test_that("apa_performance.glm uses a labeled pseudo-R2", {
  fit <- suppressWarnings(
    glm(am ~ wt + hp, data = mtcars, family = binomial)
  )
  result <- apa_performance(fit)
  expect_equal(colnames(result), c("*R*^2^~Tjur~", "*AIC*"))
  expect_equal(result[[1]], ".81")

  gaussian <- glm(mpg ~ wt, data = mtcars)
  expect_true("*R*^2^" %in% colnames(apa_performance(gaussian)))
})

test_that("apa_performance_comparison uses LRT for fixed-dispersion GLMs", {
  skip_if_not_installed("MASS")
  fits <- suppressWarnings(list(
    glm(am ~ wt, data = mtcars, family = binomial),
    glm(am ~ wt + hp, data = mtcars, family = binomial)
  ))
  result <- apa_performance_comparison(fits)
  expect_equal(
    colnames(result),
    c("Model", "*R*^2^~Tjur~", "&Delta;*R*^2^~Tjur~", "*&chi;*^2^", "*df*", "*p*")
  )
  lrt <- performance::test_likelihoodratio(fits)
  expect_equal(result[["*&chi;*^2^"]][2], sprintf("%.2f", lrt$Chi2[2]))
  expect_equal(trimws(result[["*df*"]][2]), "1")
  expect_false("&sigma;~*e*~" %in% colnames(
    apa_performance_comparison(fits, metrics = "all")
  ))

  pois <- list(
    glm(carb ~ wt, data = mtcars, family = poisson),
    glm(carb ~ wt + hp, data = mtcars, family = poisson)
  )
  expect_true(
    all(c("*R*^2^~Nagelkerke~", "*&chi;*^2^") %in%
      colnames(apa_performance_comparison(pois)))
  )

  nb <- suppressWarnings(list(
    MASS::glm.nb(carb ~ wt, data = mtcars),
    MASS::glm.nb(carb ~ wt + hp, data = mtcars)
  ))
  expect_true("*&chi;*^2^" %in% colnames(apa_performance_comparison(nb)))
})

test_that("apa_performance_comparison uses F for estimated-dispersion GLMs", {
  qp <- list(
    glm(carb ~ wt, data = mtcars, family = quasipoisson),
    glm(carb ~ wt + hp, data = mtcars, family = quasipoisson)
  )
  result <- apa_performance_comparison(qp)
  expect_true(all(c("*R*^2^~Nagelkerke~", "*F*") %in% colnames(result)))
  expect_false("*&chi;*^2^" %in% colnames(result))

  gaussian <- list(glm(mpg ~ wt, data = mtcars), glm(mpg ~ wt + hp, data = mtcars))
  expect_equal(
    colnames(apa_performance_comparison(gaussian)),
    c("Model", "*R*^2^", "&Delta;*R*^2^", "*F*", "*p*")
  )
})

test_that("apa_flextable handles Model as the row title column", {
  d <- tibble::tibble(
    Model = paste("Model", c(1, 1, 2, 2, 2)),
    Predictor = c("Constant", "SES", "Constant", "SES", "Age"),
    b = c(-4.5, 1.23, -5.1, 1.45, -.23),
    p = c(.0001, .0245, .0001, .0341, .544)
  )
  cf <- column_formats()
  cf$Predictor <- column_format(
    name = "Predictor",
    header = "Variable",
    latex = "Variable",
    formatter = stringr::str_to_upper
  )
  ft <- apa_flextable(d, row_title_column = Model, column_formats = cf)
  expect_s3_class(ft, "flextable")
  expect_false("Model" %in% ft$col_keys)
})

# ordinal models ----

test_that("apa_parameters.polr reports ORs, z tests, and thresholds", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  skip_if_not_installed("carData")
  fit <- MASS::polr(poverty ~ gender + age, data = carData::WVS, Hess = TRUE)
  result <- apa_parameters(fit)
  expect_s3_class(result, "apa7_tbl")
  expect_equal(
    colnames(result),
    c("Variable", "*B*", "*SE*", "*OR*", "*z*", "*p*")
  )
  # Predictors first, then the two thresholds
  expect_equal(nrow(result), 4L)
  expect_equal(
    result$Variable[3:4],
    c("Too Little | About Right", "About Right | Too Much")
  )
  # Thresholds have no OR, z, or p
  expect_true(all(is.na(result[["*OR*"]][3:4]) | result[["*OR*"]][3:4] == ""))
  expect_true(all(is.na(result[["*p*"]][3:4]) | result[["*p*"]][3:4] == ""))
  # p values are from the normal distribution
  z <- summary(fit)$coefficients["gendermale", "t value"]
  expect_equal(
    gsub("[ -  ]", "", result[["*p*"]][1]),
    apa_p(2 * pnorm(-abs(z)), inline = FALSE)
  )

  expect_equal(nrow(apa_parameters(fit, thresholds = FALSE)), 2L)
  expect_false("*OR*" %in% colnames(apa_parameters(fit, exponentiate = FALSE)))
})

test_that("apa_parameters.clm matches apa_parameters.polr", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  skip_if_not_installed("carData")
  f <- poverty ~ gender + age
  p <- apa_parameters(MASS::polr(f, data = carData::WVS, Hess = TRUE))
  c <- apa_parameters(ordinal::clm(f, data = carData::WVS))
  expect_equal(colnames(c), colnames(p))
  expect_equal(c$Variable, p$Variable)
  expect_equal(c[["*OR*"]], p[["*OR*"]])
})

test_that("lists of ordinal models are compared with likelihood ratio tests", {
  skip_on_cran()
  skip_if_not_installed("MASS")
  skip_if_not_installed("carData")
  m1 <- MASS::polr(poverty ~ gender, data = carData::WVS, Hess = TRUE)
  m2 <- update(m1, . ~ . + age)
  comp <- apa_performance_comparison(list(m1, m2))
  expect_true(all(c("*R*^2^~Nagelkerke~", "*&chi;*^2^") %in% colnames(comp)))
  expect_no_error(apa_flextable(apa_parameters(list(m1, m2))))
})

# named lists of models ----

test_that("named lists label models by name", {
  skip_on_cran()
  fits <- list(
    Cylinders = lm(mpg ~ cyl, data = mtcars),
    `Cylinders and weight` = lm(mpg ~ cyl + wt, data = mtcars)
  )
  labels <- c("Cylinders", "Cylinders and weight")

  expect_equal(apa_performance_comparison(fits)$Model, labels)
  expect_equal(apa_performance(fits)$Model, labels)

  # Group rows use the names, and fit statistics join to them
  result <- apa_parameters(fits)
  model_rows <- result$Model[nzchar(result$Model)]
  expect_equal(model_rows, labels)
  r2 <- result[["*R*^2^"]][nzchar(result$Model)]
  expect_true(all(nzchar(r2)))
  expect_no_error(apa_flextable(result))

  # Unnamed elements fall back to "Model i"
  partly <- list(A = fits[[1]], fits[[2]])
  expect_equal(apa_performance_comparison(partly)$Model, c("A", "Model 2"))
  expect_equal(
    apa_performance_comparison(unname(fits))$Model,
    c("Model 1", "Model 2")
  )
  # Named arguments work too
  expect_equal(
    apa_performance_comparison(First = fits[[1]], Second = fits[[2]])$Model,
    c("First", "Second")
  )
})

# mixed models ----

test_that("apa_parameters.merMod shows fixed and random effects", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("pbkrtest")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  result <- apa_parameters(fit)
  expect_s3_class(result, "apa7_tbl")
  # Kenward-Roger df (17) in the t header
  expect_equal(colnames(result), c("Variable", "*B*", "*SE*", "*t*(17)", "*p*"))
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
  # Without headings, the effects are not indented
  expect_equal(
    apa_parameters(fit, effect_headings = FALSE)$Variable[c(1, 3)],
    c("Constant", "Subject: *SD* (Constant)")
  )
  # Random-effect rows show only the estimate; correlations lack a leading zero
  expect_true(all(result[["*p*"]][5:9] %in% c("", NA)))
  expect_equal(gsub("[ -  ]", "", result[["*B*"]][8]), ".07")

  # Without random effects, there are no headings
  expect_equal(nrow(apa_parameters(fit, random = FALSE)), 2L)
  # Other ci_methods pass through
  expect_true("*t*(174)" %in% colnames(apa_parameters(fit, ci_method = "wald")))
})

test_that("apa_parameters.merMod shows a df column when df differ", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("pbkrtest")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  result <- apa_parameters(fit, random = FALSE)
  expect_true(all(c("*t*", "*df*") %in% colnames(result)))
})

test_that("apa_parameters.merMod reports odds ratios for glmer", {
  skip_if_not_installed("lme4")
  fit <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = lme4::cbpp,
    family = binomial
  )
  result <- apa_parameters(fit)
  expect_true(all(c("*OR*", "*z*") %in% colnames(result)))
  expect_equal(
    result$Variable[6:8],
    c("Random effects (*SD*)", "\u2003Herd", "\u2003\u2003Constant")
  )
  expect_equal(
    as.numeric(gsub("[ -  ]", "", result[["*OR*"]][3])),
    round(exp(lme4::fixef(fit)[["period2"]]), 2)
  )
})

test_that("mixed-model fit and comparisons", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("pbkrtest")
  m0 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  m1 <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  expect_equal(
    colnames(apa_performance(m1)),
    c("*R*^2^~marginal~", "*R*^2^~conditional~", "ICC")
  )
  comp <- suppressMessages(apa_performance_comparison(list(m0, m1)))
  expect_equal(
    colnames(comp),
    c(
      "Model",
      "*R*^2^~marginal~",
      "&Delta;*R*^2^~marginal~",
      "*R*^2^~conditional~",
      "&Delta;*R*^2^~conditional~",
      "*&chi;*^2^",
      "*df*",
      "*p*"
    )
  )
  # The random slope changes the conditional, not the marginal, R2
  r2c <- vapply(
    list(m0, m1),
    \(m) performance::r2_nakagawa(m)$R2_conditional,
    numeric(1)
  )
  expect_equal(
    comp[["&Delta;*R*^2^~conditional~"]][2],
    apa7:::align_chr(diff(r2c), trim_leading_zeros = TRUE)
  )
  result <- suppressMessages(apa_parameters(list(m0, m1)))
  expect_true(all(
    c(
      "*R*^2^~marginal~",
      "&Delta;*R*^2^~marginal~",
      "*R*^2^~conditional~",
      "&Delta;*R*^2^~conditional~"
    ) %in%
      colnames(result)
  ))
  # Neither change is shown for the first model
  first <- result[result$Model == "Model 1", ]
  expect_true(first[["&Delta;*R*^2^~marginal~"]] %in% c("", NA))
  expect_true(first[["&Delta;*R*^2^~conditional~"]] %in% c("", NA))
  expect_no_error(apa_flextable(result))
  # An explicit choice is respected
  result <- suppressMessages(
    apa_parameters(list(m0, m1), performance_parameters = c("R2", "deltaR2"))
  )
  expect_false("*R*^2^~conditional~" %in% colnames(result))
})

# Confidence intervals ----

# Parse "[LL, UL]" strings into a two-column matrix
parse_ci <- function(x) {
  x <- gsub("−", "-", gsub("[^0-9.,−-]", "", x))
  do.call(rbind, lapply(strsplit(x, ","), as.numeric))
}

lm_ci_parameters <- c("Parameter", "Coefficient", "SE", "CI", "t", "df_error", "p")
exp_ci_parameters <- c(
  "Parameter", "Coefficient", "SE", "CI", "Exp_Coefficient", "Exp_CI", "z", "p"
)

test_that("apa_parameters does not show confidence intervals by default", {
  result <- apa_parameters(lm(mpg ~ wt, data = mtcars))
  expect_false(any(grepl("CI", colnames(result))))
  result <- apa_parameters(glm(am ~ wt, data = mtcars, family = binomial))
  expect_false(any(grepl("CI", colnames(result))))
})

test_that("apa_parameters.lm shows confidence intervals", {
  fit <- lm(mpg ~ cyl + wt, data = mtcars)
  result <- apa_parameters(fit, predictor_parameters = lm_ci_parameters)
  expect_equal(
    colnames(result),
    c("Variable", "*B*", "*SE*", "95% CI", "*t*(29)", "*p*")
  )
  expect_match(result[["95% CI"]], "^\\[.+, .+\\]$")
  mp <- parameters::model_parameters(fit)
  expect_equal(
    parse_ci(result[["95% CI"]]),
    unname(round(cbind(mp$CI_low, mp$CI_high), 2))
  )

  # The CI level comes from the ci argument
  result <- apa_parameters(fit, predictor_parameters = lm_ci_parameters, ci = .90)
  expect_true("90% CI" %in% colnames(result))

  # Renamed column
  result <- apa_parameters(
    fit,
    predictor_parameters = c(Parameter = "Parameter", Interval = "CI")
  )
  expect_equal(ncol(result), 2L)

  expect_no_error(apa_flextable(apa_parameters(fit, predictor_parameters = lm_ci_parameters)))
})

test_that("apa_parameters.lm passes vcov to model_parameters (robust SEs)", {
  skip_if_not_installed("sandwich")
  fit <- lm(mpg ~ cyl + wt, data = mtcars)
  classic <- apa_parameters(fit, predictor_parameters = lm_ci_parameters)
  robust <- apa_parameters(fit, predictor_parameters = lm_ci_parameters, vcov = "HC3")
  mp <- parameters::model_parameters(fit, vcov = "HC3")

  expect_equal(robust[["*B*"]], classic[["*B*"]])
  for (column in c("*SE*", "95% CI", "*t*(29)")) {
    expect_false(identical(robust[[column]], classic[[column]]))
  }
  expect_equal(
    as.numeric(gsub("[^0-9.]", "", robust[["*SE*"]])),
    unname(round(mp$SE, 2))
  )
  expect_equal(
    parse_ci(robust[["95% CI"]]),
    unname(round(cbind(mp$CI_low, mp$CI_high), 2))
  )
  expect_equal(
    gsub("[^0-9.<]", "", robust[["*p*"]]),
    apa_p(mp$p, inline = FALSE)
  )
})

test_that("apa_parameters.glm shows exponentiated confidence intervals", {
  skip_on_cran()
  fit <- glm(carb ~ wt, data = mtcars, family = poisson)
  result <- apa_parameters(fit, predictor_parameters = exp_ci_parameters)
  expect_equal(
    colnames(result),
    c("Variable", "*B*", "*SE*", "95% CI", "*IRR*", "95% CI for *IRR*", "*z*", "*p*")
  )
  mp <- parameters::model_parameters(fit)
  expect_equal(
    parse_ci(result[["95% CI for *IRR*"]]),
    unname(round(exp(cbind(mp$CI_low, mp$CI_high)), 2))
  )

  # Logistic models: CI for OR
  fit_logit <- glm(am ~ wt, data = mtcars, family = binomial)
  result <- apa_parameters(fit_logit, predictor_parameters = exp_ci_parameters)
  expect_true("95% CI for *OR*" %in% colnames(result))

  # Not exponentiated: no Exp_CI column, and no stray CI level column
  result <- apa_parameters(
    fit,
    predictor_parameters = c("Parameter", "Coefficient", "Exp_CI"),
    exponentiate = FALSE
  )
  expect_equal(colnames(result), c("Variable", "*B*"))

  # User-customized header is kept
  cf <- column_formats()
  cf$Exp_CI@header <- "{round(ci * 100)}% CI for ratio"
  result <- apa_parameters(
    fit,
    predictor_parameters = exp_ci_parameters,
    column_formats = cf
  )
  expect_true("95% CI for ratio" %in% colnames(result))

  expect_no_error(
    apa_flextable(apa_parameters(fit, predictor_parameters = exp_ci_parameters))
  )
})

test_that("apa_parameters.glm passes vcov to model_parameters (robust SEs)", {
  skip_if_not_installed("sandwich")
  fit <- glm(carb ~ wt, data = mtcars, family = poisson)
  classic <- apa_parameters(fit, predictor_parameters = exp_ci_parameters)
  robust <- apa_parameters(fit, predictor_parameters = exp_ci_parameters, vcov = "HC3")
  mp <- parameters::model_parameters(fit, vcov = "HC3")

  for (column in c("*SE*", "95% CI", "95% CI for *IRR*", "*z*", "*p*")) {
    expect_false(identical(robust[[column]], classic[[column]]))
  }
  expect_equal(
    as.numeric(gsub("[^0-9.]", "", robust[["*SE*"]])),
    unname(round(mp$SE, 2))
  )
  expect_equal(
    parse_ci(robust[["95% CI for *IRR*"]]),
    unname(round(exp(cbind(mp$CI_low, mp$CI_high)), 2))
  )
})

test_that("ordinal models show Wald CIs, with blank exponentiated CIs for thresholds", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  skip_if_not_installed("carData")
  f <- poverty ~ gender + age
  fit <- MASS::polr(f, data = carData::WVS, Hess = TRUE)
  result <- apa_parameters(fit, predictor_parameters = exp_ci_parameters)
  expect_true(all(c("95% CI", "95% CI for *OR*") %in% colnames(result)))
  expect_equal(result[["95% CI for *OR*"]][3:4], c("", ""))
  expect_match(result[["95% CI"]], "^\\[.+, .+\\]$")

  # Wald z intervals
  s <- summary(fit)$coefficients[c("gendermale", "age"), ]
  wald <- exp(s[, "Value"] + outer(s[, "Std. Error"], qnorm(c(.025, .975))))
  expect_equal(
    parse_ci(result[["95% CI for *OR*"]][1:2]),
    unname(round(wald, 2))
  )

  # clm gives the same intervals
  result_clm <- apa_parameters(
    ordinal::clm(f, data = carData::WVS),
    predictor_parameters = exp_ci_parameters
  )
  expect_equal(result_clm[["95% CI for *OR*"]], result[["95% CI for *OR*"]])
})

test_that("apa_parameters.list re-aligns confidence intervals across models", {
  skip_if_not(apa7:::font_available("Times New Roman"))
  apa7_defaults(font_family = "Times New Roman")
  fits <- list(
    lm(mpg ~ wt, data = mtcars),
    lm(mpg ~ hp, data = transform(mtcars, hp = -hp))
  )
  result <- apa_parameters(
    fits,
    predictor_parameters = lm_ci_parameters,
    performance_parameters = NULL
  )
  ci <- result[["95% CI"]]
  expect_match(ci, "^\\[.+, .+\\]$")
  # Lower bound decimals and commas line up; upper bound decimals line up
  # relative to the comma
  width <- \(pattern) {
    apa7:::md_text_width(stringr::str_extract(ci, pattern), "Times New Roman")
  }
  expect_lt(diff(range(width("^[^.]*"))), 0.025)
  expect_lt(diff(range(width("^[^,]*"))), 0.025)
  expect_lt(diff(range(width("^[^,]*,[^.]*") - width("^[^,]*"))), 0.025)
})

# align_padded_columns ----

test_that("apa_flextable left-aligns padded columns and centers the block", {
  skip_on_cran()
  glm1 <- suppressWarnings(glm(am ~ wt + hp, data = mtcars, family = binomial))
  d <- apa_parameters(
    glm1,
    predictor_parameters = c("Parameter", "Exp_Coefficient", "z", "p")
  )
  ft <- apa_flextable(d)
  align <- ft$body$styles$pars$text.align$data
  pad <- ft$body$styles$pars$padding.left$data
  expect_true(all(align[, "*z*"] == "left"))
  expect_true(all(pad[, "*z*"] > 3))
  # Uniform padding within the column
  expect_equal(length(unique(pad[, "*z*"])), 1L)
  # A nearly equal right padding keeps the block centered where columns are
  # sized to their contents (HTML), and the block still fits the column
  pad_right <- ft$body$styles$pars$padding.right$data
  expect_equal(unique(pad_right[, "*z*"]), unique(pad[, "*z*"]) - 3)
  width <- ft$body$colwidths[[match("*z*", ft$col_keys)]] * 72
  expect_lte(unique(pad[, "*z*"]) + unique(pad_right[, "*z*"]), width)

  # Unpadded columns stay centered
  d2 <- tibble::tibble(Variable = c("a", "b"), x = c("one", "two"))
  ft2 <- apa_flextable(d2)
  expect_true(all(ft2$body$styles$pars$text.align$data[, "x"] == "center"))
})

# apa7_tbl ----

test_that("apa tables have class apa7_tbl and remain tibbles", {
  skip_on_cran()
  fit <- lm(mpg ~ cyl + wt, data = mtcars)
  m1 <- lm(mpg ~ cyl, data = mtcars)
  glms <- suppressWarnings(list(
    glm(am ~ wt, data = mtcars, family = binomial),
    glm(am ~ wt + hp, data = mtcars, family = binomial)
  ))
  objs <- list(
    apa_parameters(fit),
    apa_parameters(list(m1, fit)),
    apa_parameters(glms),
    apa_performance(fit),
    apa_performance_comparison(list(m1, fit))
  )
  for (o in objs) {
    expect_s3_class(o, "apa7_tbl")
    expect_s3_class(o, "data.frame")
  }
  # Single-model tables are tibbles (lists of models are grouped data frames)
  expect_s3_class(objs[[1]], "tbl_df")

  # Class survives dplyr post-processing
  d <- apa_parameters(fit) |>
    dplyr::filter(Variable != "Constant")
  expect_s3_class(d, "apa7_tbl")
  expect_equal(nrow(d), 2L)
  expect_s3_class(apa_flextable(d), "flextable")
})

test_that("apa7.print option controls knit_print and print", {
  d <- apa_parameters(lm(mpg ~ cyl + wt, data = mtcars))

  withr::with_options(list(apa7.print = "auto"), {
    expect_s3_class(knit_print.apa7_tbl(d), "knit_asis")
    expect_output(print(d), "tibble")
  })

  withr::with_options(list(apa7.print = "tibble"), {
    expect_false(inherits(knit_print.apa7_tbl(d), "knit_asis") &&
      grepl("<table", paste(knit_print.apa7_tbl(d), collapse = "")))
    expect_output(print(d), "tibble")
  })
})

# add_list_column ----
test_that("add_list_column", {
  d <- data.frame(x = letters[1:5], y = letters[2:6])
  # default is first column
  expect_identical(colnames(add_list_column(d))[1], "xapa7listcolumn")
  # select any column
  expect_identical(colnames(add_list_column(d, y))[2], "yapa7listcolumn")
  expect_identical(
    add_list_column(d, type = "a", sep = ") ")[, "xapa7listcolumn"][1],
    "a) "
  )
  expect_identical(
    add_list_column(d, type = "A", sep = ") ")[, "xapa7listcolumn"][1],
    "A) "
  )
  expect_identical(
    add_list_column(d, type = "I", sep = ") ")[, "xapa7listcolumn"][1],
    "I) "
  )
  expect_identical(
    add_list_column(d, type = "i", sep = ") ")[, "xapa7listcolumn"][1],
    "i) "
  )
  # merge list column
  expect_identical(ncol(add_list_column(d, merge = TRUE)), 2L)
})

# add_break_columns ----
test_that("add_break_columns", {
  d <- data.frame(x_n = 3, x_mean = 4, y_n = 5, y_mean = 6, z_n = 4, z_mean = 4)
  expect_equal(colnames(add_break_columns(d, x_mean))[3], "apa7breakcolumn1")
  expect_equal(
    colnames(add_break_columns(d, c("y_n", "z_n"), .before = TRUE))[c(3, 6)],
    c("apa7breakcolumn1", "apa7breakcolumn2")
  )

  expect_equal(
    colnames(add_break_columns(d, "y_n", .before = TRUE))[3],
    "apa7breakcolumn1"
  )

  expect_equal(
    colnames(add_break_columns(d, dplyr::ends_with("_mean"), omit_last = TRUE))[
      c(3, 6)
    ],
    c("apa7breakcolumn1", "apa7breakcolumn2")
  )

  expect_equal(add_break_columns(d, "asdf"), d)
})

# rep_letters ----
test_that("rep_letters", {
  expect_identical(apa7:::rep_letters(0), character(0))
  expect_identical(apa7:::rep_letters(1:3), letters[1:3])
  expect_identical(apa7:::rep_letters(1:3, type = "A"), LETTERS[1:3])
  expect_identical(
    apa7:::rep_letters(1:52, type = "A"),
    c(LETTERS[1:26], paste0("A", LETTERS[1:26]))
  )
})

# add_star_column ----
test_that("add_star_column", {
  d <- data.frame(b = c(1.4, 2.2), p = c(.54, .02))
  expect_identical(
    colnames(add_star_column(d, b, p))[c(2, 4)],
    paste0(c("b", "p"), "apa7starcolumn")
  )
  expect_error(
    add_star_column(d, "s"),
    "Can't select columns that don't exist."
  )
  expect_identical(ncol(add_star_column(d, b, p, merge = TRUE)), 2L)
})

# separate_star_column ----
test_that("add_star_column", {
  d <- tibble::tibble(x = c(".45", ".58*", ".68**"), y = c(1, 2, 3), z = 4:6)

  expect_identical(
    colnames(separate_star_column(d, x))[2],
    "xapa7starcolumn"
  )

  expect_identical(ncol(separate_star_column(d)), 6L)
})

# apa_flextable ----
test_that("apa_flextable", {
  skip_on_cran()
  d <- mtcars %>%
    dplyr::select(vs, am, gear, carb) |>
    tidyr::pivot_longer(-vs, names_to = "Variable") |>
    dplyr::summarise(
      Mean = round(mean(value), 2),
      SD = round(sd(value), 2),
      .by = c(Variable, vs)
    ) |>
    dplyr::mutate(
      vs = factor(vs, levels = 0:1, labels = c("Automatic", "Manual"))
    )
  expect_no_error(
    apa_flextable(d, row_title_column = "vs", row_title_align = "center")
  )
  expect_no_error(
    apa_flextable(d, row_title_column = "vs", row_title_align = "left")
  )

  expect_no_error(
    apa_flextable(d, row_title_column = "fred", row_title_align = "left")
  )
})

test_that("deprecation warning", {
  expect_no_error(
    data.frame(x_n = 3, x_mean = 4, y_n = 5, y_mean = 6, z_n = 4, z_mean = 4) |>
      apa_flextable()
  )
})

# pivot_wider_name_first  ----
test_that("pivot_wider_name_first ", {
  d <- data.frame(Model = paste0("Model", " ", 1:2), t = 1:2, p = c(.05, .45))

  expect_equal(
    colnames(
      pivot_wider_name_first(d, names_from = Model, values_from = c(t, p))
    ),
    c("Model 1_t", "Model 1_p", "Model 2_t", "Model 2_p")
  )

  expect_error(
    pivot_wider_name_first(d, names_from = c(Model, p), values_from = c(t, p)),
    "Only one names_from variable is allowed."
  )
})

test_that("apa_cor centers every summary column", {
  skip_on_cran()
  # Body cells of the summary columns are centered, with equal padding
  summary_alignment <- function(ft) {
    info <- flextable::information_data_paragraph(ft)
    keys <- setdiff(ft$col_keys, c("apa7listcolumn", "Variable"))
    keys <- keys[!grepl("apa7starcolumn$|^[0-9]+$", keys)]
    body <- info[info$.part == "body" & info$.col_id %in% keys, ]
    list(
      keys = keys,
      align = unique(body$text.align),
      balanced = all(body$padding.left == body$padding.right)
    )
  }
  a <- summary_alignment(apa_cor(trees))
  expect_equal(a$keys, c("*M*", "*SD*"))
  expect_equal(a$align, "center")
  expect_true(a$balanced)

  b <- summary_alignment(apa_cor(
    mtcars[, c("mpg", "hp", "wt")],
    summary_functions = c("n", "M", "SD", "Skewness", "Kurtosis", "Median", "IQR", "Var")
  ))
  expect_length(b$keys, 8)
  expect_equal(b$align, "center")
  expect_true(b$balanced)
})

test_that("random effects can be shown as variances and covariances", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("pbkrtest")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  result <- apa_parameters(fit, random_scale = "variance")
  expect_equal(result$Variable[4:5], c("Random effects (*Var*)", "\u2003Subject"))
  expect_equal(result$Variable[8], "\u2003\u2003*Cov* (Constant, Days)")
  vc <- as.data.frame(lme4::VarCorr(fit))
  values <- as.numeric(gsub("[^0-9.\u2212-]", "", result[["*B*"]][c(6:8, 9)]))
  expect_equal(values, round(vc$vcov, 2))
  note <- apa_note(result)
  expect_match(note, "*Var* = variance; *Cov* = covariance.", fixed = TRUE)
  expect_no_match(note, "standard deviation", fixed = TRUE)

  # The SD scale defines its symbols, too
  expect_match(
    apa_note(apa_parameters(fit)),
    "*SD* = standard deviation; *r* = correlation.",
    fixed = TRUE
  )

  # Without headings, the labels name the statistic
  plain <- apa_parameters(fit, random_scale = "variance", effect_headings = FALSE)
  expect_equal(
    plain$Variable[3:6],
    c(
      "Subject: *Var* (Constant)", "Subject: *Var* (Days)",
      "Subject: *Cov* (Constant, Days)", "Residual *Var*"
    )
  )
  expect_error(apa_parameters(fit, random_scale = "precision"), "should be one of")
})

test_that("variance-scale nlme residual structures are squared or doubled", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(
    distance ~ age + Sex,
    random = ~ 1 | Subject,
    weights = nlme::varIdent(form = ~ 1 | Sex),
    data = nlme::Orthodont
  )
  result <- apa_parameters(fit, random_scale = "variance")
  ratio <- stats::coef(fit$modelStruct$varStruct, unconstrained = FALSE)
  row <- result$Variable == "\u2003Residual variance ratio: Female"
  expect_equal(as_num(result[["*B*"]][row]), unname(round(ratio^2, 2)))
  expect_match(apa_note(result), "has its own residual variance", fixed = TRUE)

  fit_power <- nlme::lme(
    distance ~ age,
    random = ~ 1 | Subject,
    weights = nlme::varPower(form = ~ age),
    data = nlme::Orthodont
  )
  delta <- stats::coef(fit_power$modelStruct$varStruct, unconstrained = FALSE)
  power <- apa_parameters(fit_power, random_scale = "variance")
  row <- grepl("variance power", power$Variable, fixed = TRUE)
  expect_equal(as_num(power[["*B*"]][row]), unname(round(2 * delta, 2)))
})

test_that("each cluster gets a subheading; nested clusters are spelled out", {
  skip_if_not_installed("lme4")
  set.seed(1)
  d <- data.frame(
    school = rep(1:6, each = 20),
    classroom = rep(1:4, each = 5, times = 6),
    item = rep(1:5, times = 24)
  )
  d$y <- rnorm(120) + rep(rnorm(6), each = 20) + rep(rnorm(24), each = 5) + rnorm(5)[d$item]
  nested <- suppressMessages(lme4::lmer(y ~ 1 + (1 | school / classroom), data = d))
  result <- apa_parameters(nested, ci_method = "wald")
  expect_equal(
    result$Variable[3:8],
    c(
      "Random effects (*SD*)",
      "\u2003Classroom in School", "\u2003\u2003Constant",
      "\u2003School", "\u2003\u2003Constant",
      "\u2003Residual"
    )
  )
  crossed <- suppressMessages(lme4::lmer(y ~ 1 + (1 | school) + (1 | item), data = d))
  labels <- apa_parameters(crossed, ci_method = "wald")$Variable
  expect_true(all(c("\u2003School", "\u2003Item") %in% labels))
  expect_equal(sum(labels == "\u2003\u2003Constant"), 2)
})

test_that("apa_cor() table_width is a proportion, with inches still accepted", {
  skip_on_cran()
  d <- mtcars[, c("mpg", "wt", "hp")]
  total_width <- function(ft) sum(ft$body$colwidths)

  # Default: the full width of 6.5 inches, as before
  full <- apa_cor(d)
  expect_equal(total_width(full), 6.5, tolerance = 1e-6)
  expect_equal(full$properties$width, 1)

  half <- apa_cor(d, table_width = .5)
  expect_equal(total_width(half), 3.25, tolerance = 1e-6)
  expect_equal(half$properties$width, .5)

  # Values above 1 are read as inches, with a warning
  expect_warning(inches <- apa_cor(d, table_width = 5), "now a proportion")
  expect_equal(total_width(inches), 5, tolerance = 1e-6)
  expect_equal(inches$properties$width, 5 / 6.5)

  # 0 keeps the widths from pretty_widths()
  expect_no_error(apa_cor(d, table_width = 0))

  expect_error(apa_cor(d, table_width = -1), "proportion")
  expect_error(apa_cor(d, table_width = "wide"), "proportion")
})

test_that("table_width outside 0 to 1 gives a clear error in apa_flextable(), apa_style(), and apa_chisq()", {
  d <- head(mtcars[1:3])
  expect_error(apa_flextable(d, table_width = 4), "use `table_width = 0.62`")
  expect_error(apa_flextable(d, table_width = -1), "from 0 to 1")
  expect_error(apa_flextable(d, table_width = "wide"), "from 0 to 1")
  expect_error(apa_style(flextable::flextable(d), table_width = 8), "proportion of the page width")
  expect_error(apa_chisq(table(mtcars$am, mtcars$vs), table_width = 8), "proportion of the page width")
  expect_no_error(apa_flextable(d, table_width = .5))
  expect_no_error(apa_flextable(d, table_width = 0))
})
