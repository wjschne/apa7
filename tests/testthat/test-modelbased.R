library(apa7)
library(testthat)

skip_if_not_installed("modelbased")
skip_if_not_installed("marginaleffects")

# Padding spaces that align values
unpad <- function(x) gsub("[\u2002-\u200a ]", "", x)
minus <- "\u2212"
em <- "\u2003"

iris_fit <- function() lm(Sepal.Width ~ Species * Sepal.Length, data = iris)

cars_data <- function() {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  d$am <- factor(d$am, labels = c("Auto", "Manual"))
  d
}

test_that("apa_means() shows means, SE, and CI, and says what was held constant", {
  fit <- iris_fit()
  result <- apa_means(fit, by = "Species")
  expect_s3_class(result, "apa7_tbl")
  expect_equal(colnames(result), c("Species", "*M*", "*SE*", "95% CI"))
  expect_equal(result$Species, c("setosa", "versicolor", "virginica"))

  em_means <- modelbased::estimate_means(fit, by = "Species")
  expect_equal(unpad(result$`*M*`), sprintf("%.2f", em_means$Mean))
  expect_equal(
    unpad(result$`95% CI`[1]),
    sprintf("[%.2f,%.2f]", em_means$CI_low[1], em_means$CI_high[1])
  )
  # modelbased output gives the same table
  expect_equal(apa_means(em_means), result, ignore_attr = TRUE)

  note <- apa_note(result)
  expect_match(note, "Values are estimated marginal means of Sepal Width.", fixed = TRUE)
  expect_match(
    note,
    sprintf("Sepal Length is held at its mean (%.2f).", mean(iris$Sepal.Length)),
    fixed = TRUE
  )
  expect_match(note, "*M* = estimated marginal mean", fixed = TRUE)
  expect_equal(apa_flextable(result)$body$dataset[[1]], result$Species)
})

test_that("apa_contrasts() labels differences and names the p-value adjustment", {
  fit <- iris_fit()
  result <- apa_contrasts(fit, contrast = "Species", p_adjust = "holm")
  expect_equal(
    colnames(result),
    c("Species", "Difference", "*SE*", "95% CI", "*d*", "95% CI for *d*", "*t*(144)", "*p*")
  )
  expect_equal(result$Species[1], paste0("versicolor ", minus, " setosa"))

  ec <- modelbased::estimate_contrasts(fit, contrast = "Species", p_adjust = "holm")
  expect_equal(unpad(result$`*p*`), unpad(the$column_formats$p@formatter(ec$p)))

  note <- apa_note(result)
  expect_match(note, "(the first level minus the second)", fixed = TRUE)
  expect_match(note, "adjusted for multiple comparisons with Holm's method.", fixed = TRUE)

  unadjusted <- apa_contrasts(fit, contrast = "Species")
  expect_match(apa_note(unadjusted), "not adjusted for multiple comparisons", fixed = TRUE)
})

test_that("other variables in by become heading rows", {
  d <- cars_data()
  fit <- lm(mpg ~ cyl * am + wt, data = d)

  means <- apa_means(fit, by = c("am", "cyl"))
  expect_equal(
    means$Cyl,
    c("Am: Auto", paste0(em, c("4", "6", "8")), "Am: Manual", paste0(em, c("4", "6", "8")))
  )
  expect_equal(means$`*M*`[1], "")

  contrasts <- apa_contrasts(fit, contrast = "cyl", by = "am", p_adjust = "tukey")
  expect_equal(contrasts$Cyl[c(1, 5)], c("Am: Auto", "Am: Manual"))
  expect_equal(contrasts$Cyl[2], paste0(em, "6 ", minus, " 4"))
  expect_match(apa_note(contrasts), "Tukey's method", fixed = TRUE)
  expect_match(apa_note(contrasts), "Wt is held at its mean", fixed = TRUE)
})

test_that("apa_slopes() shows simple slopes at moderator values, or one average slope", {
  fit <- iris_fit()
  result <- apa_slopes(fit, trend = "Sepal.Length", by = "Species")
  expect_equal(colnames(result)[1:2], c("Species", "Slope"))
  es <- modelbased::estimate_slopes(fit, trend = "Sepal.Length", by = "Species")
  expect_equal(unpad(result$Slope), sprintf("%.2f", es$Slope))
  expect_match(
    apa_note(result),
    "Values are simple slopes of Sepal Length (the expected change in Sepal Width for a one-unit increase in Sepal Length) at each value of Species.",
    fixed = TRUE
  )

  # Numeric moderator values are row labels
  numeric_mod <- apa_slopes(lm(mpg ~ wt * hp, mtcars), trend = "wt", by = "hp=c(100,150,200)")
  expect_equal(numeric_mod$Hp, c("100", "150", "200"))

  average <- apa_slopes(lm(mpg ~ wt + hp, mtcars), trend = "wt")
  expect_equal(average$Predictor, "Wt")
  expect_match(apa_note(average), "The estimate is the average slope of Wt", fixed = TRUE)
})

test_that("logistic models give probabilities without leading zeros and z tests", {
  d <- cars_data()
  fit <- glm(am ~ cyl + wt, data = d, family = binomial)

  means <- apa_means(fit, by = "cyl")
  expect_equal(colnames(means)[2], "Probability")
  expect_false(any(grepl("^0\\.", unpad(means$Probability))))
  note <- apa_note(means)
  expect_match(note, "Values are estimated probabilities of Am.", fixed = TRUE)
  expect_match(note, "back-transformed from the logit scale", fixed = TRUE)

  contrasts <- apa_contrasts(fit, contrast = "cyl")
  expect_true("*z*" %in% colnames(contrasts))
  expect_match(apa_note(contrasts), "differences between estimated probabilities of Am", fixed = TRUE)
})

test_that("grouping factors of mixed models are not called predictors held constant", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  result <- apa_means(fit, by = "Days=c(0,5,9)")
  expect_equal(result$Days, c("0", "5", "9"))
  expect_no_match(apa_note(result), "Subject", fixed = TRUE)
})

test_that("inputs are checked", {
  fit <- iris_fit()
  ec <- modelbased::estimate_contrasts(fit, contrast = "Species")
  expect_error(apa_means(ec), "apa_contrasts")
  expect_error(apa_slopes(iris), "plain data frame")
  expect_warning(apa_contrasts(ec, p_adjust = "holm"), "ignored")
})

test_that("shortcut moderator values are named, and numeric by defaults to [sd]", {
  fit <- lm(mpg ~ wt * hp, data = mtcars)
  m <- mean(mtcars$hp)
  s <- stats::sd(mtcars$hp)
  expected <- c(
    sprintf("%s1*SD* (%.2f)", minus, m - s),
    sprintf("*M* (%.2f)", m),
    sprintf("+1*SD* (%.2f)", m + s)
  )
  expect_equal(apa_slopes(fit, trend = "wt", by = "hp=[sd]")$Hp, expected)
  # A numeric moderator without values is set to its mean and 1 SD around it
  expect_equal(apa_slopes(fit, trend = "wt", by = "hp")$Hp, expected)
  # Also from modelbased output
  es <- modelbased::estimate_slopes(fit, trend = "wt", by = "hp=[sd]")
  expect_equal(apa_slopes(es)$Hp, expected)

  five <- apa_slopes(fit, trend = "wt", by = "hp=[fivenum]")
  expect_equal(
    five$Hp,
    c("Min (52)", "*Q*~1~ (96)", "*Mdn* (123)", "*Q*~3~ (180)", "Max (335)")
  )
  expect_match(apa_note(five), "*Q*~1~ = first quartile (lower hinge)", fixed = TRUE)

  # Explicit values are left as they are
  expect_equal(apa_slopes(fit, trend = "wt", by = "hp=c(100, 150)")$Hp, c("100", "150"))

  # Shortcut values in heading rows
  d <- mtcars
  d$am <- factor(d$am, labels = c("Auto", "Manual"))
  fit2 <- lm(mpg ~ wt * hp * am, data = d)
  headed <- apa_slopes(fit2, trend = "wt", by = c("hp", "am"))
  expect_equal(headed$Am[1], paste0("Hp: ", expected[1]))
  expect_equal(headed$Am[2], paste0(em, "Auto"))
})

test_that("numeric by defaults to [sd] in means and contrasts; contrasted shortcut values are named", {
  d <- mtcars
  d$am <- factor(d$am, labels = c("Auto", "Manual"))
  fit <- lm(mpg ~ am * hp, data = d)
  m <- mean(d$hp)
  s <- stats::sd(d$hp)
  sd_labels <- c(
    sprintf("%s1*SD* (%.2f)", minus, m - s),
    sprintf("*M* (%.2f)", m),
    sprintf("+1*SD* (%.2f)", m + s)
  )
  expect_equal(apa_means(fit, by = "hp")$Hp, sd_labels)
  by_hp <- apa_contrasts(fit, contrast = "am", by = "hp")
  expect_equal(by_hp$Am[c(1, 3, 5)], paste0("Hp: ", sd_labels))

  # Contrasts between shortcut values
  sd_contrasts <- apa_contrasts(fit, contrast = "hp=[sd]")
  expect_equal(colnames(sd_contrasts)[1], "Hp")
  expect_equal(
    sd_contrasts$Hp,
    c(
      paste0("*M* ", minus, " ", minus, "1*SD*"),
      paste0("+1*SD* ", minus, " ", minus, "1*SD*"),
      paste0("+1*SD* ", minus, " *M*")
    )
  )
  expect_match(
    apa_note(sd_contrasts),
    sprintf("The values of Hp are %s1*SD* = %.2f, *M* = %.2f, and +1*SD* = %.2f.", minus, m - s, m, m + s),
    fixed = TRUE
  )
  five <- apa_contrasts(fit, contrast = "hp=[fivenum]")
  expect_equal(five$Hp[4], paste0("Max ", minus, " Min"))
  expect_match(apa_note(five), "lower hinge", fixed = TRUE)

  # With other variables in by
  grouped <- apa_contrasts(fit, contrast = "hp=[sd]", by = "am")
  expect_equal(grouped$Hp[1], "Am: Auto")
  expect_equal(grouped$Hp[2], paste0(em, "*M* ", minus, " ", minus, "1*SD*"))
})

test_that("by, contrast, and trend take unquoted variable names", {
  d <- cars_data()
  fit <- lm(mpg ~ cyl * am + hp * wt, data = d)
  expect_equal(apa_means(fit, by = cyl), apa_means(fit, by = "cyl"))
  expect_equal(apa_means(fit, by = c(am, cyl)), apa_means(fit, by = c("am", "cyl")))
  expect_equal(
    apa_contrasts(fit, contrast = cyl, by = am, p_adjust = "holm"),
    apa_contrasts(fit, contrast = "cyl", by = "am", p_adjust = "holm")
  )
  expect_equal(apa_slopes(fit, trend = wt, by = hp), apa_slopes(fit, trend = "wt", by = "hp"))
  # Unquoted names mix with quoted values
  expect_equal(
    apa_slopes(fit, trend = wt, by = c(am, "hp=[sd]")),
    apa_slopes(fit, trend = "wt", by = c("am", "hp=[sd]"))
  )
  # Objects holding names and other arguments are evaluated
  vars <- c("am", "cyl")
  adjust <- "holm"
  expect_equal(apa_means(fit, by = vars), apa_means(fit, by = c("am", "cyl")))
  expect_match(apa_note(apa_contrasts(fit, contrast = cyl, p_adjust = adjust)), "Holm", fixed = TRUE)

  expect_error(apa_means(fit, by = zzz), "not a variable in the model's data")
  em_cyl <- modelbased::estimate_means(fit, by = "cyl")
  expect_warning(apa_means(em_cyl, by = cyl), "ignored")
})

test_that("by, contrast, and trend are named arguments that can be given by position", {
  expect_equal(names(formals(apa_means))[1:3], c("x", "by", "..."))
  expect_equal(names(formals(apa_contrasts))[1:4], c("x", "contrast", "by", "..."))
  expect_equal(names(formals(apa_slopes))[1:4], c("x", "trend", "by", "..."))
  d <- cars_data()
  fit <- lm(mpg ~ cyl * am + wt, data = d)
  expect_equal(apa_means(fit, cyl), apa_means(fit, by = "cyl"))
  expect_equal(apa_contrasts(fit, cyl, am), apa_contrasts(fit, contrast = "cyl", by = "am"))
  expect_equal(apa_slopes(fit, wt, am), apa_slopes(fit, trend = "wt", by = "am"))
  # Named arguments are also ignored (with a warning) for modelbased output
  es <- modelbased::estimate_slopes(fit, trend = "wt", by = "am")
  expect_warning(apa_slopes(es, trend = wt), "ignored")
})

test_that("by given as a named list gets shortcut labels", {
  d <- cars_data()
  fit <- lm(mpg ~ wt * hp, data = d)
  five <- c("Min (52)", "*Q*~1~ (96)", "*Mdn* (123)", "*Q*~3~ (180)", "Max (335)")
  expect_equal(apa_slopes(fit, trend = wt, by = list(hp = "[fivenum]"))$Hp, five)
  expect_equal(apa_slopes(fit, trend = wt, by = list(hp = c(100, 150)))$Hp, c("100", "150"))
  # modelbased output made with a list keeps the labels
  es <- modelbased::estimate_slopes(fit, trend = "wt", by = list(hp = "[fivenum]"))
  expect_equal(apa_slopes(es)$Hp, five)

  # Contrasts with a list by (which modelbased cannot handle directly)
  fit2 <- lm(mpg ~ am * hp, data = d)
  m <- mean(d$hp)
  contrasts <- apa_contrasts(fit2, contrast = am, by = list(hp = "[sd]"))
  expect_equal(contrasts$Am[3], sprintf("Hp: *M* (%.2f)", m))

  # Unnamed elements are variable names
  fit3 <- lm(mpg ~ am * hp * wt, data = d)
  mixed <- apa_slopes(fit3, trend = wt, by = list("am", hp = "[sd]"))
  expect_equal(mixed, apa_slopes(fit3, trend = wt, by = c("am", "hp=[sd]")))
})

test_that("apa_contrasts() adds Cohen's d for models with a residual SD", {
  fit <- iris_fit()
  result <- apa_contrasts(fit, contrast = "Species")
  mb <- modelbased::estimate_contrasts(fit, contrast = "Species")
  sigma <- stats::sigma(fit)
  df <- stats::df.residual(fit)
  d <- mb$Difference / sigma
  se <- sqrt((mb$SE / sigma)^2 + d^2 / (2 * df))
  crit <- stats::qt(0.975, df)
  unpad <- function(x) gsub("[\u2002-\u200a ]", "", x)
  num <- function(v) gsub("-", "\u2212", sprintf("%.2f", v))
  expect_equal(unpad(result$`*d*`), num(d))
  expect_equal(
    unpad(result$`95% CI for *d*`[1]),
    paste0("[", num(d[1] - crit * se[1]), ",", num(d[1] + crit * se[1]), "]")
  )
  note <- apa_note(result)
  expect_match(note, "*d* = difference divided by the residual standard deviation of the model (", fixed = TRUE)
  expect_match(note, "accounts for the uncertainty in the residual standard deviation", fixed = TRUE)

  # Turned off, or with a given standardizer
  expect_false("*d*" %in% colnames(apa_contrasts(fit, contrast = "Species", effect_size = FALSE)))
  given <- apa_contrasts(fit, contrast = "Species", sd = 2)
  expect_equal(unpad(given$`*d*`[1]), num(mb$Difference[1] / 2))
  expect_match(apa_note(given), "*d* = difference divided by 2.", fixed = TRUE)

  # Also for modelbased output
  expect_equal(apa_contrasts(mb)$`*d*`, result$`*d*`)

  # Means and slopes have no d
  expect_false("*d*" %in% colnames(apa_means(fit, by = "Species")))
})

test_that("Cohen's d is skipped or refused for logistic and mixed models", {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  g <- glm(am ~ cyl, data = d, family = binomial)
  expect_false("*d*" %in% colnames(apa_contrasts(g, contrast = cyl)))
  expect_error(apa_contrasts(g, contrast = cyl, effect_size = TRUE), "identity link")

  skip_if_not_installed("lme4")
  s <- lme4::sleepstudy
  s$period <- factor(s$Days %/% 4)
  m <- lme4::lmer(Reaction ~ period + (1 | Subject), data = s)
  expect_false("*d*" %in% colnames(apa_contrasts(m, contrast = period)))
  expect_error(apa_contrasts(m, contrast = period, effect_size = TRUE), "mixed models")
  expect_true("*d*" %in% colnames(apa_contrasts(m, contrast = period, sd = 50)))

  expect_error(apa_contrasts(g, contrast = cyl, sd = -1), "positive number")
  expect_error(apa_contrasts(g, contrast = cyl, effect_size = "yes"), "effect_size")
})
