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
    c("Species", "Difference", "*SE*", "95% CI", "*t*(144)", "*p*")
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
