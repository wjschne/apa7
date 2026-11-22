library(apa7)
library(testthat)

hs_model <- "visual =~ x1 + x2 + x3
             textual =~ x4 + x5 + x6
             speed =~ x7 + x8 + x9"

test_that("apa_performance.lavaan shows the standard fit indices", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::cfa(hs_model, data = lavaan::HolzingerSwineford1939)
  result <- apa_performance(fit)
  expect_equal(
    colnames(result),
    c("*&chi;*^2^", "*df*", "*p*", "CFI", "TLI", "RMSEA", "90% CI", "SRMR")
  )
  fm <- lavaan::fitMeasures(fit)
  expect_equal(as_num(result$`*&chi;*^2^`), round(unname(fm["chisq"]), 2))
  expect_equal(as_num(result$CFI), round(unname(fm["cfi"]), 2))
  expect_equal(
    unname(ci_bounds(result$`90% CI`)[1, ]),
    round(unname(fm[c("rmsea.ci.lower", "rmsea.ci.upper")]), 2)
  )
  note <- apa_note(result)
  expect_match(note, "*N* = 301. Estimation used maximum likelihood (ML).", fixed = TRUE)
  expect_match(note, "TLI = Tucker–Lewis index", fixed = TRUE)
  expect_match(note, "CI = confidence interval for RMSEA", fixed = TRUE)
  expect_no_match(note, "scaled", fixed = TRUE)
})

test_that("robust estimators use scaled tests and robust indices", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::cfa(
    hs_model,
    data = lavaan::HolzingerSwineford1939,
    estimator = "MLR",
    missing = "fiml"
  )
  result <- apa_performance(fit)
  fm <- lavaan::fitMeasures(fit)
  expect_equal(as_num(result$`*&chi;*^2^`), round(unname(fm["chisq.scaled"]), 2))
  expect_equal(as_num(result$CFI), round(unname(fm["cfi.robust"]), 2))
  note <- apa_note(result)
  expect_match(note, "robust maximum likelihood (MLR)", fixed = TRUE)
  expect_match(note, "CFI, TLI, and RMSEA are the robust versions", fixed = TRUE)
  expect_match(note, "full information maximum likelihood", fixed = TRUE)
})

test_that("WLSMV uses scaled indices when robust ones are unavailable", {
  skip_if_not_installed("lavaan")
  d <- lavaan::HolzingerSwineford1939
  for (v in c("x1", "x2", "x3")) d[[v]] <- cut(d[[v]], 3)
  fit <- lavaan::cfa(hs_model, data = d, ordered = c("x1", "x2", "x3"))
  result <- apa_performance(fit)
  fm <- lavaan::fitMeasures(fit)
  expect_equal(as_num(result$CFI), round(unname(fm["cfi.scaled"]), 2))
  note <- apa_note(result)
  expect_match(note, "(WLSMV)", fixed = TRUE)
  expect_match(note, "are the scaled versions", fixed = TRUE)
})

test_that("lists of lavaan models keep the note; metrics can be chosen", {
  skip_if_not_installed("lavaan")
  fit3 <- lavaan::cfa(hs_model, data = lavaan::HolzingerSwineford1939)
  fit1 <- lavaan::cfa(
    "g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9",
    data = lavaan::HolzingerSwineford1939
  )
  result <- apa_performance(list(`One factor` = fit1, `Three factors` = fit3))
  expect_equal(result$Model, c("One factor", "Three factors"))
  expect_true(is_ci_md(result$`90% CI`))
  expect_match(apa_note(result), "*N* = 301.", fixed = TRUE)

  expect_equal(
    colnames(apa_performance(fit3, metrics = c("CFI", "AIC"))),
    c("CFI", "*AIC*")
  )
  expect_equal(ncol(apa_performance(fit3, metrics = "all")), 10)
  expect_error(apa_performance(fit3, metrics = "GFI"), "Choose from")
})

sem_model <- paste(
  sep = "\n",
  hs_model,
  "textual ~ a * visual
   speed ~ b * textual + c * visual
   indirect := a * b"
)

test_that("apa_parameters.lavaan shows standardized estimates under headings", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::sem(sem_model, data = lavaan::HolzingerSwineford1939)
  result <- apa_parameters(fit)
  expect_equal(colnames(result), c("Parameter", "&beta;", "*SE*", "*p*"))
  expect_equal(
    result$Parameter[c(1, 11, 15)],
    c("Factor loadings", "Regressions", "Defined parameters")
  )
  expect_true(all(c(
    " Visual → X1", " Textual ← Visual", " Indirect"
  ) %in% result$Parameter))
  ss <- lavaan::standardizedSolution(fit)
  expect_equal(
    as_num(result$`&beta;`[2]),
    round(ss$est.std[ss$lhs == "visual" & ss$rhs == "x1"], 2)
  )
  note <- apa_note(result)
  expect_match(note, "Estimates are fully standardized", fixed = TRUE)
  expect_match(note, "*N* = 301.", fixed = TRUE)
  # Labels and headings survive flextables
  expect_equal(apa_flextable(result)$body$dataset[[1]], result$Parameter)
})

test_that("CFA correlations and unstandardized estimates with fixed markers", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::cfa(hs_model, data = lavaan::HolzingerSwineford1939)
  result <- apa_parameters(fit)
  expect_true("Correlations" %in% result$Parameter)
  expect_true(" Visual ↔ Textual" %in% result$Parameter)

  result_b <- apa_parameters(fit, standardize = FALSE)
  expect_equal(colnames(result_b), c("Parameter", "*B*", "*SE*", "*p*"))
  expect_true("Covariances" %in% result_b$Parameter)
  marker <- result_b$Parameter == " Visual → X1"
  expect_equal(trimws(result_b$`*SE*`[marker]), "")
  expect_match(
    apa_note(result_b),
    "the loadings Visual → X1, Textual → X4, and Speed → X7 were fixed at 1",
    fixed = TRUE
  )
})

test_that("variances and CIs can be added; fixed standardized values are not tested", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3", data = lavaan::HolzingerSwineford1939)
  result <- apa_parameters(
    fit,
    variances = TRUE,
    predictor_parameters = c("Parameter", "Std_Coefficient", "SE", "CI", "p")
  )
  expect_true(all(c("Variances", " X1 (residual)") %in% result$Parameter))
  expect_true(is_ci_md(result$`95% CI`))
  expect_false(any(grepl("0.", result$`95% CI`, fixed = TRUE)))
  latent <- result$Parameter == " Visual"
  expect_equal(trimws(result$`*SE*`[latent]), "")
})

test_that("robust and bootstrap standard errors are named; groups are not yet supported", {
  skip_if_not_installed("lavaan")
  fit_r <- lavaan::cfa(hs_model, data = lavaan::HolzingerSwineford1939, estimator = "MLR")
  expect_match(apa_note(apa_parameters(fit_r)), "Standard errors are robust.", fixed = TRUE)
  set.seed(1)
  fit_b <- lavaan::cfa(
    "visual =~ x1 + x2 + x3",
    data = lavaan::HolzingerSwineford1939,
    se = "bootstrap",
    bootstrap = 20
  )
  # Few bootstrap samples (for speed) give warnings about interval endpoints
  note_b <- suppressWarnings(apa_note(apa_parameters(fit_b)))
  expect_match(note_b, "20 bootstrap samples", fixed = TRUE)
  fit_g <- lavaan::cfa(
    "visual =~ x1 + x2 + x3",
    data = lavaan::HolzingerSwineford1939,
    group = "school"
  )
  expect_error(apa_parameters(fit_g), "multiple-group")
})
