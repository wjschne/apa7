library(apa7)
library(testthat)

# apa_chisq ----

test_that("apa_chisq returns an apaflextable for a valid 2-column data frame", {
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
  skip_if_not_installed("MASS")
  skip_if_not_installed("carData")
  m1 <- MASS::polr(poverty ~ gender, data = carData::WVS, Hess = TRUE)
  m2 <- update(m1, . ~ . + age)
  comp <- apa_performance_comparison(list(m1, m2))
  expect_true(all(c("*R*^2^~Nagelkerke~", "*&chi;*^2^") %in% colnames(comp)))
  expect_no_error(apa_flextable(apa_parameters(list(m1, m2))))
})

# align_padded_columns ----

test_that("apa_flextable left-aligns padded columns and centers the block", {
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

  # Unpadded columns stay centered
  d2 <- tibble::tibble(Variable = c("a", "b"), x = c("one", "two"))
  ft2 <- apa_flextable(d2)
  expect_true(all(ft2$body$styles$pars$text.align$data[, "x"] == "center"))
})

# apa7_tbl ----

test_that("apa tables have class apa7_tbl and remain tibbles", {
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

# apa_loadings ----
test_that("apa_loadings", {
  fit <- psych::fa(Harman74.cor$cov, 4, fm = "pa", rotate = "varimax")
  expect_equal(nrow(apa_loadings(fit)), 24L)
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
