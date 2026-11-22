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

test_that("robust and bootstrap standard errors are named", {
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
})

test_that("RMSEA intervals keep decimals at 0; bootstrap CIs are named percentile", {
  skip_if_not_installed("lavaan")
  pd_model <- "ind60 =~ x1 + x2 + x3
               dem60 =~ y1 + y2 + y3 + y4
               dem65 =~ y5 + y6 + y7 + y8
               dem60 ~ ind60
               dem65 ~ ind60 + dem60
               y1 ~~ y5
               y2 ~~ y4 + y6
               y3 ~~ y7
               y4 ~~ y8
               y6 ~~ y8"
  fit <- lavaan::sem(pd_model, data = lavaan::PoliticalDemocracy)
  expect_equal(apa_performance(fit)$`90% CI`, "[.00, .09]")

  set.seed(1)
  fit_b <- suppressWarnings(lavaan::cfa(
    "visual =~ x1 + x2 + x3",
    data = lavaan::HolzingerSwineford1939,
    se = "bootstrap",
    bootstrap = 20
  ))
  note <- suppressWarnings(apa_note(apa_parameters(
    fit_b,
    standardize = FALSE,
    predictor_parameters = c("Parameter", "Coefficient", "SE", "CI")
  )))
  expect_match(note, "confidence intervals are bootstrap percentile intervals", fixed = TRUE)
})

cross_model <- "visual =~ x1 + x2 + x3 + x9
                textual =~ x4 + x5 + x6
                speed =~ x7 + x8 + x9"

test_that("apa_loadings() shows standardized CFA loadings in model order", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::cfa(cross_model, data = lavaan::HolzingerSwineford1939)
  result <- apa_loadings(fit, communality = TRUE)
  expect_equal(colnames(result), c("Variable", "Visual", "Textual", "Speed", "*h*^2^"))
  expect_equal(result$Variable, paste0("X", c(1, 2, 3, 9, 4:8)))
  std <- lavaan::lavInspect(fit, "std")
  expect_equal(as_num(result$Visual[1]), round(std$lambda["x1", "visual"], 2))
  # The cross-loading is shown; loadings fixed at 0 are blank
  expect_equal(as_num(result$Speed[4]), round(std$lambda["x9", "speed"], 2))
  expect_equal(trimws(result$Textual[1]), "")
  expect_equal(
    as_num(result$`*h*^2^`[1]),
    round(1 - diag(std$theta)[["x1"]], 2)
  )
  note <- apa_note(result)
  expect_match(note, "*N* = 301. Estimation used maximum likelihood (ML).", fixed = TRUE)
  expect_match(note, "fully standardized estimates from a confirmatory factor analysis", fixed = TRUE)
  expect_match(note, "Blank cells are loadings fixed at 0.", fixed = TRUE)
  expect_no_match(note, "omitted", fixed = TRUE)
  expect_no_match(note, "pattern coefficients", fixed = TRUE)
  expect_equal(apa_flextable(result)$body$dataset[[1]], result$Variable)

  # Sorting, cutoffs, and names can still be chosen
  sorted <- apa_loadings(
    fit,
    sort_loading = TRUE,
    min_loading = .5,
    factor_names = c(speed = "Processing speed")
  )
  expect_equal(colnames(sorted)[4], "Processing speed")
  expect_equal(trimws(sorted$Visual[sorted$Variable == "X9"]), "")
  expect_match(apa_note(sorted), "smaller than .50", fixed = TRUE)
})

test_that("apa_factor_cor() shows latent correlations for lavaan models", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::cfa(hs_model, data = lavaan::HolzingerSwineford1939)
  result <- apa_factor_cor(fit)
  expect_equal(result$Factor, c("1. Visual", "2. Textual", "3. Speed"))
  cor_lv <- lavaan::lavInspect(fit, "cor.lv")
  expect_equal(as_num(result$`1`[2]), round(cor_lv["textual", "visual"], 2))
  expect_match(apa_note(result), "from the fully standardized solution.", fixed = TRUE)
  expect_no_match(apa_note(result), "implied", fixed = TRUE)

  fit_sem <- lavaan::sem(sem_model, data = lavaan::HolzingerSwineford1939)
  expect_match(apa_note(apa_factor_cor(fit_sem)), "implied by the model", fixed = TRUE)

  fit_1 <- lavaan::cfa("visual =~ x1 + x2 + x3", data = lavaan::HolzingerSwineford1939)
  expect_error(apa_factor_cor(fit_1), "one-factor")
  expect_no_match(apa_note(apa_loadings(fit_1)), "Blank cells", fixed = TRUE)
})

test_that("higher-order factors are left out; groups are not yet supported", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::cfa(
    paste(sep = "\n", hs_model, "g =~ visual + textual + speed"),
    data = lavaan::HolzingerSwineford1939
  )
  expect_equal(colnames(apa_loadings(fit))[-1], c("Visual", "Textual", "Speed"))
  fit_g <- lavaan::cfa(
    "visual =~ x1 + x2 + x3",
    data = lavaan::HolzingerSwineford1939,
    group = "school"
  )
  expect_error(apa_loadings(fit_g), "multiple-group")
})

invariance_fits <- function(...) {
  d <- lavaan::HolzingerSwineford1939
  list(
    Configural = lavaan::cfa(hs_model, d, group = "school", ...),
    Metric = lavaan::cfa(hs_model, d, group = "school", group.equal = "loadings", ...),
    Scalar = lavaan::cfa(
      hs_model, d,
      group = "school",
      group.equal = c("loadings", "intercepts"),
      ...
    )
  )
}

test_that("apa_performance_comparison() compares each lavaan model with the previous one", {
  skip_if_not_installed("lavaan")
  fits <- invariance_fits()
  result <- apa_performance_comparison(fits)
  spanner <- "Change from previous model_"
  expect_equal(
    colnames(result),
    c(
      "Model", "*&chi;*^2^", "*df*", "CFI", "RMSEA", "90% CI", "SRMR",
      paste0(spanner, c("&Delta;*&chi;*^2^", "&Delta;*df*", "*p*", "&Delta;CFI", "&Delta;RMSEA"))
    )
  )
  expect_equal(result$Model, c("Configural", "Metric", "Scalar"))
  # The first model has nothing to be compared with
  expect_true(all(trimws(unlist(result[1, -(1:7)])) == ""))

  lrt <- lavaan::lavTestLRT(fits$Metric, fits$Scalar)
  expect_equal(
    as_num(result[[paste0(spanner, "&Delta;*&chi;*^2^")]][3]),
    round(lrt$`Chisq diff`[2], 2)
  )
  expect_equal(trimws(result[[paste0(spanner, "&Delta;*df*")]][3]), "6")
  cfi <- purrr::map_dbl(fits, \(f) lavaan::fitMeasures(f, "cfi"))
  expect_equal(
    as_num(result[[paste0(spanner, "&Delta;CFI")]][3]),
    round(cfi[[3]] - cfi[[2]], 3)
  )
  note <- apa_note(result)
  expect_match(note, "&Delta; = change from the previous model.", fixed = TRUE)
  expect_match(note, "is a likelihood ratio (*&chi;*^2^ difference) test.", fixed = TRUE)
  expect_equal(apa_flextable(result)$body$dataset$Model, result$Model)
})

test_that("robust lavaan comparisons name the scaled difference test", {
  skip_if_not_installed("lavaan")
  fits <- invariance_fits(estimator = "MLR")
  result <- apa_performance_comparison(
    fits[1:2],
    metrics = c("CFI", "deltaChi2", "deltaChi2_df", "p_deltaChi2", "deltaSRMR")
  )
  expect_equal(ncol(result), 6)
  lrt <- lavaan::lavTestLRT(fits$Configural, fits$Metric)
  expect_equal(
    as_num(result$`Change from previous model_&Delta;*&chi;*^2^`[2]),
    round(lrt$`Chisq diff`[2], 2)
  )
  expect_match(
    apa_note(result),
    "Satorra–Bentler (2001) scaled *&chi;*^2^ difference test",
    fixed = TRUE
  )
  # Changes in fit indices alone have no difference test
  only_delta <- apa_performance_comparison(fits[1:2], metrics = "deltaCFI")
  expect_equal(colnames(only_delta), c("Model", "Change from previous model_&Delta;CFI"))
  expect_no_match(apa_note(only_delta), "difference test", fixed = TRUE)

  expect_error(apa_performance_comparison(fits[1:2], metrics = "GFI"), "Choose from")
  expect_error(apa_performance_comparison(fits[1]), "at least two")
  expect_equal(ncol(apa_performance_comparison(fits, metrics = "all")), 18)
})

mg_model <- paste(
  sep = "\n",
  hs_model,
  "speed ~ c(a1, a2) * visual
   ind1 := a1 * 2
   diff := a1 - a2"
)

test_that("multiple-group models show groups side by side by default", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::sem(
    mg_model,
    data = lavaan::HolzingerSwineford1939,
    group = "school",
    group.equal = "loadings",
    group.partial = "visual=~x3"
  )
  result <- apa_parameters(fit)
  expect_equal(
    colnames(result),
    c(
      "Parameter",
      paste0("Pasteur_", c("&beta;", "*SE*", "*p*")),
      paste0("Grant-White_", c("&beta;", "*SE*", "*p*"))
    )
  )
  ss <- lavaan::standardizedSolution(fit)
  x1 <- result$Parameter == " Visual → X1"
  expect_equal(
    as_num(result$`Grant-White_&beta;`[x1]),
    round(ss$est.std[ss$lhs == "visual" & ss$rhs == "x1" & ss$group == 2], 2)
  )
  # ind1 uses a group-1 label; diff combines groups, so both are in the
  # first group's columns
  defined <- result$Parameter %in% c(" Ind1", " Diff")
  expect_true(all(trimws(result$`Pasteur_&beta;`[defined]) != ""))
  expect_true(all(trimws(result$`Grant-White_&beta;`[defined]) == ""))

  note <- apa_note(result)
  expect_match(note, "*N* = 301 (Pasteur: *n* = 156; Grant-White: *n* = 145).", fixed = TRUE)
  expect_match(
    note,
    "Loadings were constrained to be equal across groups, except Visual → X3.",
    fixed = TRUE
  )
  expect_match(note, "Standardized estimates of equal parameters can differ", fixed = TRUE)
  expect_match(note, "shown in the Pasteur columns", fixed = TRUE)

  ft <- apa_flextable(result)
  expect_equal(ft$body$dataset[[1]], result$Parameter)
  expect_true(all(c("Pasteur", "Grant-White") %in% unlist(ft$header$dataset[1, ])))
})

test_that("multiple-group models can be stacked with group headings", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::sem(
    mg_model,
    data = lavaan::HolzingerSwineford1939,
    group = "school",
    group.equal = c("loadings", "intercepts")
  )
  result <- apa_parameters(fit, groups = "rows", standardize = FALSE)
  expect_equal(colnames(result), c("Parameter", "*B*", "*SE*", "*p*"))
  headings <- c("Pasteur", "Grant-White", "Across groups")
  expect_true(all(headings %in% result$Parameter))
  expect_true(all(diff(match(headings, result$Parameter)) > 0))
  expect_true("  Visual → X1" %in% result$Parameter)
  # diff combines groups, so it is under its own heading
  expect_gt(
    match("  Diff", result$Parameter),
    match("Across groups", result$Parameter)
  )
  note <- apa_note(result)
  expect_match(note, "Loadings and intercepts were constrained", fixed = TRUE)
  expect_no_match(note, "Standardized estimates of equal", fixed = TRUE)
  expect_no_match(note, "columns", fixed = TRUE)
  expect_match(note, "were fixed at 1", fixed = TRUE)
})

test_that("partial invariance labels name the freed parameters", {
  expect_equal(
    lavaan_partial_label(c("visual =~ x3", "x1 ~ 1", "speed ~ visual", "x1 ~~ x2", "u1|t1")),
    c(
      "Visual → X3",
      "the intercept of X1",
      "Speed ← Visual",
      "X1 ↔ X2",
      "U1 | t1"
    )
  )
})
