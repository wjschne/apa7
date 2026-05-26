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
