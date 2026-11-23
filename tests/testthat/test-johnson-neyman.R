library(apa7)
library(testthat)

skip_if_not_installed("modelbased")
skip_if_not_installed("marginaleffects")

test_that("Johnson-Neyman boundaries are where the slope's t equals its critical value", {
  fit <- lm(mpg ~ wt * hp, data = mtcars)
  jn <- apa7:::johnson_neyman(fit, "wt", "hp")
  expect_length(jn$boundaries, 1)
  b <- coef(fit)
  V <- vcov(fit)
  m <- jn$boundaries
  slope <- b[["wt"]] + b[["wt:hp"]] * m
  se <- sqrt(V["wt", "wt"] + 2 * m * V["wt", "wt:hp"] + m^2 * V["wt:hp", "wt:hp"])
  expect_equal(abs(slope / se), qt(0.975, df.residual(fit)))
  expect_equal(jn$segments$significant, c(TRUE, FALSE))
  expect_equal(jn$range, range(mtcars$hp))

  # The modelbased grid puts the boundary between two of its values
  grid <- as.data.frame(suppressMessages(summary(
    modelbased::estimate_slopes(fit, trend = "wt", by = "hp", length = 200)
  )))
  expect_gt(m, grid$End[1])
  expect_lt(m, grid$Start[2])
})

test_that("apa_slopes() notes give the Johnson-Neyman intervals", {
  fit <- lm(mpg ~ wt * hp, data = mtcars)
  note <- apa_note(apa_slopes(fit, trend = wt, by = hp))
  boundary <- sprintf("%.2f", apa7:::johnson_neyman(fit, "wt", "hp")$boundaries)
  expect_match(
    note,
    paste0(
      "Johnson\u2013Neyman intervals (Johnson & Neyman, 1936): the slope of Wt is significant (*p* < .05) when Hp is from 52 to ",
      boundary, " (observed range: 52 to 335)."
    ),
    fixed = TRUE
  )
  expect_match(
    apa_note(apa_slopes(fit, trend = wt, by = hp), citations = "keys"),
    "Johnson\u2013Neyman intervals [@johnsonTestsCertainLinear1936]",
    fixed = TRUE
  )
  expect_no_match(apa_note(apa_slopes(fit, trend = wt, by = hp, johnson_neyman = FALSE)), "Neyman", fixed = TRUE)

  # Significant everywhere, or nowhere, in the observed range
  everywhere <- apa7:::johnson_neyman_sentence(list(
    segments = tibble::tibble(from = 1, to = 5, significant = TRUE, direction = "negative"),
    range = c(1, 5), alpha = 0.05, trend = "X", moderator = "M"
  ))
  expect_match(everywhere, "is significant (*p* < .05) across the observed range of M (1 to 5)", fixed = TRUE)
  nowhere <- apa7:::johnson_neyman_sentence(list(
    segments = tibble::tibble(from = 1, to = 5, significant = FALSE, direction = "negative"),
    range = c(1, 5), alpha = 0.05, trend = "X", moderator = "M"
  ))
  expect_match(nowhere, "is not significant (*p* < .05) anywhere in the observed range of M", fixed = TRUE)
})

test_that("Johnson-Neyman intervals are skipped or refused when they do not apply", {
  binary <- lm(mpg ~ wt * am, data = mtcars)
  expect_no_match(apa_note(apa_slopes(binary, trend = wt, by = am)), "Neyman", fixed = TRUE)
  expect_error(apa_slopes(binary, trend = wt, by = am, johnson_neyman = TRUE), "continuous moderator")

  three_way <- lm(mpg ~ wt * hp * am, data = mtcars)
  expect_error(apa_slopes(three_way, trend = wt, by = hp, johnson_neyman = TRUE), "depend only on")

  logistic <- glm(am ~ wt * hp, data = mtcars, family = binomial)
  expect_no_match(apa_note(apa_slopes(logistic, trend = wt, by = hp)), "Neyman", fixed = TRUE)
  expect_error(apa_slopes(logistic, trend = wt, by = hp, johnson_neyman = TRUE), "identity link")

  fit <- lm(mpg ~ wt * hp, data = mtcars)
  expect_error(apa_slopes(fit, trend = wt, johnson_neyman = TRUE), "one numeric moderator")
  expect_error(apa_slopes(fit, trend = wt, by = hp, johnson_neyman = "yes"), "johnson_neyman")
})
