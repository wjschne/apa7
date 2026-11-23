library(apa7)
library(testthat)

skip_if_not_installed("mgcv")

unpad <- function(x) gsub("[\u2002-\u200a ]", "", x)
num <- function(v) gsub("-", "\u2212", sprintf("%.2f", v))

gam_data <- function() {
  set.seed(1)
  d <- mgcv::gamSim(1, n = 200, verbose = FALSE, scale = 2)
  d$g <- factor(sample(c("a", "b"), 200, TRUE))
  d
}

test_that("GAMs get parametric and smooth terms under headings", {
  d <- gam_data()
  fit <- mgcv::gam(y ~ g + s(x0) + te(x2, x3), data = d, method = "REML")
  s <- summary(fit)
  result <- apa_parameters(fit)
  expect_s3_class(result, "apa7_tbl")
  df_header <- paste0("*t*(", sprintf("%.2f", s$residual.df), ")")
  expect_equal(colnames(result), c("Variable", "*B*", "*SE*", df_header, "*edf*", "*F*", "*p*"))
  expect_equal(
    result$Variable,
    c("Parametric terms", "\u2003Constant", "\u2003G\u2013B", "Smooth terms", "\u2003s(X 0)", "\u2003te(X 2, X 3)")
  )
  expect_equal(unpad(result$`*B*`[3]), num(s$p.table["gb", "Estimate"]))
  expect_equal(unpad(result$`*edf*`[5]), num(s$s.table["s(x0)", "edf"]))
  expect_equal(unpad(result$`*F*`[6]), num(s$s.table["te(x2,x3)", "F"]))
  expect_equal(result$`*B*`[5], "")

  note <- apa_note(result)
  expect_match(note, "Smoothing parameters were estimated by restricted maximum likelihood (REML).", fixed = TRUE)
  expect_match(note, "Smooth terms are tested with approximate *F* tests.", fixed = TRUE)
  expect_match(note, "*edf* = effective degrees of freedom", fixed = TRUE)
  expect_equal(apa_flextable(result)$body$dataset[[1]], result$Variable)
})

test_that("terms, binomial models, and factor-by smooths", {
  d <- gam_data()
  fit <- mgcv::gam(y ~ g + s(x0), data = d, method = "REML")
  smooth <- apa_parameters(fit, terms = "smooth")
  expect_equal(colnames(smooth), c("Variable", "*edf*", "*F*", "*p*"))
  expect_equal(smooth$Variable, "s(X 0)")
  parametric <- apa_parameters(fit, terms = "parametric")
  expect_false("*edf*" %in% colnames(parametric))
  expect_no_match(apa_note(parametric), "Smooth", fixed = TRUE)

  fb <- mgcv::gam(I(y > 7) ~ g + s(x0), family = binomial, data = d, method = "REML")
  binomial <- apa_parameters(fb)
  expect_true(all(c("*OR*", "*z*", "*&chi;*^2^") %in% colnames(binomial)))
  expect_match(apa_note(binomial), "approximate *&chi;*^2^ tests", fixed = TRUE)

  by <- apa_parameters(mgcv::gam(y ~ g + s(x0, by = g), data = d, method = "REML"))
  expect_true(all(c("\u2003s(X 0): G = a", "\u2003s(X 0): G = b") %in% by$Variable))

  gcv <- apa_parameters(mgcv::gam(y ~ s(x0), data = d))
  expect_match(apa_note(gcv), "generalized cross-validation (GCV)", fixed = TRUE)

  expect_error(apa_parameters(mgcv::gam(y ~ x0, data = d), terms = "smooth"), "no smooth terms")
})

test_that("apa_performance gives the adjusted R2 and the deviance explained", {
  d <- gam_data()
  fit <- mgcv::gam(y ~ s(x0) + s(x1), data = d, method = "REML")
  s <- summary(fit)
  perf <- apa_performance(fit)
  expect_equal(colnames(perf), c("*R*^2^~adj~", "Deviance explained (%)", "*AIC*"))
  expect_equal(unpad(perf$`Deviance explained (%)`), sprintf("%.1f", 100 * s$dev.expl))
  expect_equal(unpad(perf$`*R*^2^~adj~`), sub("^0", "", sprintf("%.2f", s$r.sq)))
  all <- apa_performance(fit, metrics = "all")
  expect_true(all(c("*n*", "*BIC*") %in% colnames(all)))
  expect_error(apa_performance(fit, metrics = "Sigma"), "Unknown metric")
})

test_that("lists of GAMs are compared by R2 and AIC, without tests or stars", {
  d <- gam_data()
  g1 <- mgcv::gam(y ~ s(x0), data = d, method = "ML")
  g2 <- mgcv::gam(y ~ s(x0) + s(x1), data = d, method = "ML")
  comparison <- apa_performance_comparison(list(g1, g2))
  expect_equal(colnames(comparison), c("Model", "*R*^2^", "&Delta;*R*^2^", "*AIC*"))
  both <- apa_parameters(list(g1, g2))
  expect_true("*AIC*" %in% colnames(both))
  expect_false(any(grepl("*", both$`&Delta;*R*^2^`, fixed = TRUE)))
  expect_no_match(apa_note(both), "p* < .05", fixed = TRUE)
})
