library(apa7)
library(testthat)

skip_if_not_installed("metafor")
skip_if_not_installed("metadat")

unpad <- function(x) gsub("[\u2002-\u200a ]", "", x)

bcg <- function() {
  metafor::escalc(
    measure = "RR",
    ai = tpos, bi = tneg, ci = cpos, di = cneg,
    data = metadat::dat.bcg,
    slab = paste(author, year)
  )
}

test_that("models without moderators get a row for each study and the pooled estimate", {
  d <- bcg()
  fit <- metafor::rma(yi, vi, data = d)
  result <- apa_parameters(fit)
  expect_s3_class(result, "apa7_tbl")
  expect_equal(colnames(result), c("Study", "*RR*", "95% CI", "Weight (%)"))
  expect_equal(nrow(result), 14)
  expect_equal(result$Study[1], "Aronson 1948")
  expect_equal(result$Study[14], "Pooled estimate (random effects)")
  expect_equal(attr(result, "rule_above"), "Pooled estimate (random effects)")

  # Exponentiated estimates and weights
  expect_equal(unpad(result$`*RR*`[1]), sprintf("%.2f", exp(d$yi[1])))
  expect_equal(unpad(result$`*RR*`[14]), sprintf("%.2f", exp(as.vector(fit$b))))
  expect_equal(unpad(result$`Weight (%)`[1]), sprintf("%.1f", weights(fit)[1]))
  expect_equal(result$`Weight (%)`[14], "")

  note <- apa_note(result)
  expect_match(note, "Effect sizes are risk ratios, analyzed on the log scale and exponentiated for the table.", fixed = TRUE)
  expect_match(note, "The 13 studies were pooled with a random-effects model, with *&tau;*^2^ estimated by restricted maximum likelihood.", fixed = TRUE)
  expect_match(note, "Test of the pooled estimate: *z* = \u22123.97, *p* < .001; 95% PI [", fixed = TRUE)
  expect_match(note, "Heterogeneity: *Q*(12) = 152.23, *p* < .001", fixed = TRUE)
  expect_match(note, "PI = prediction interval", fixed = TRUE)

  ft <- apa_flextable(result)
  expect_equal(ft$body$dataset[[1]], result$Study)
})

test_that("transform, weights, and studies options", {
  d <- bcg()
  fit <- metafor::rma(yi, vi, data = d)
  raw <- apa_parameters(fit, transform = FALSE, weights = FALSE)
  expect_equal(colnames(raw), c("Study", "ln(*RR*)", "95% CI"))
  expect_match(apa_note(raw), "Effect sizes are risk ratios on the log scale.", fixed = TRUE)
  expect_no_match(apa_note(raw), "Weights", fixed = TRUE)

  pooled <- apa_parameters(fit, studies = FALSE)
  expect_equal(colnames(pooled), c("Variable", "*RR*", "95% CI", "*z*", "*p*"))
  expect_equal(pooled$Variable, "Pooled estimate (random effects)")

  # Equal-effects models have no prediction interval or tau2
  fe <- apa_parameters(metafor::rma(yi, vi, data = d, method = "EE"))
  expect_equal(fe$Study[14], "Pooled estimate (equal effects)")
  expect_no_match(apa_note(fe), "PI", fixed = TRUE)
  expect_match(apa_note(fe), "pooled with an equal-effects model.", fixed = TRUE)

  # Knapp-Hartung adjustment gives t tests
  kh <- apa_parameters(metafor::rma(yi, vi, data = d, test = "knha", method = "DL"))
  expect_match(apa_note(kh), "Knapp\u2013Hartung adjustment", fixed = TRUE)
  expect_match(apa_note(kh), "*t*(12) = ", fixed = TRUE)
  expect_match(apa_note(kh), "DerSimonian\u2013Laird", fixed = TRUE)

  expect_error(
    apa_parameters(metafor::rma(yi, vi, mods = ~ablat, data = d), studies = TRUE),
    "without moderators"
  )
})

test_that("correlations are converted back from Fisher's z", {
  d <- metafor::escalc(
    measure = "ZCOR", ri = ri, ni = ni,
    data = metadat::dat.molloy2014,
    slab = paste(authors, year)
  )
  fit <- metafor::rma(yi, vi, data = d)
  result <- apa_parameters(fit)
  expect_equal(colnames(result)[2], "*r*")
  expect_equal(unpad(result$`*r*`[1]), sub("^0", "", sprintf("%.2f", tanh(d$yi[1]))))
  expect_match(apa_note(result), "analyzed on the Fisher's *z* scale and converted back", fixed = TRUE)
})

test_that("meta-regressions get a coefficient table", {
  d <- bcg()
  fit <- metafor::rma(yi, vi, mods = ~ ablat + year, data = d)
  result <- apa_parameters(fit)
  expect_equal(result$Variable, c("Constant", "Ablat", "Year"))
  expect_equal(colnames(result), c("Variable", "*B*", "*SE*", "95% CI", "*RR*", "*z*", "*p*"))
  expect_equal(unpad(result$`*RR*`[2]), sprintf("%.2f", exp(coef(fit)[["ablat"]])))
  note <- apa_note(result)
  expect_match(note, "The meta-regression of 13 studies used a mixed-effects model", fixed = TRUE)
  expect_match(note, "*RR* = risk ratio", fixed = TRUE)
})

test_that("apa_performance gives heterogeneity statistics", {
  d <- bcg()
  fit <- metafor::rma(yi, vi, data = d)
  perf <- apa_performance(fit)
  expect_equal(colnames(perf), c("*k*", "*Q*(12)", "*p*", "*&tau;*^2^", "*I*^2^ (%)"))
  expect_equal(unpad(perf$`*Q*(12)`), sprintf("%.2f", fit$QE))
  expect_equal(unpad(perf$`*I*^2^ (%)`), sprintf("%.1f", fit$I2))
  expect_match(apa_note(perf), "*I*^2^ = percentage of total variability due to heterogeneity", fixed = TRUE)

  all <- apa_performance(fit, metrics = "all")
  expect_true(all(c("95% CI for *&tau;*^2^", "95% CI for *I*^2^", "*H*^2^") %in% colnames(all)))

  fit2 <- metafor::rma(yi, vi, mods = ~ ablat + year, data = d)
  perf2 <- apa_performance(fit2)
  expect_equal(
    colnames(perf2),
    c("*k*", "*Q*~E~(10)", "*p*~E~", "*&tau;*^2^", "*I*^2^ (%)", "*Q*~M~(2)", "*p*~M~", "*R*^2^ (%)")
  )

  fe <- apa_performance(metafor::rma(yi, vi, data = d, method = "EE"))
  expect_equal(colnames(fe), c("*k*", "*Q*(12)", "*p*"))

  expect_error(apa_performance(fit, metrics = "zz"), "Unknown metric")
})

test_that("multilevel meta-analyses (rma.mv) get effect-size rows and variance components", {
  d <- metadat::dat.konstantopoulos2011
  fit <- metafor::rma.mv(
    yi, vi,
    random = ~ 1 | district / school,
    data = d,
    slab = paste0("District ", district, ", School ", school)
  )
  result <- apa_parameters(fit)
  expect_equal(nrow(result), 57)
  expect_equal(colnames(result), c("Study", "*g*", "95% CI", "Weight (%)"))
  expect_equal(result$Study[57], "Pooled estimate (random effects)")
  expect_equal(unpad(result$`*g*`[57]), sprintf("%.2f", as.vector(fit$b)))
  expect_equal(unpad(result$`Weight (%)`[5]), sprintf("%.1f", weights(fit)[5]))

  # Multilevel I2 (Cheung, 2014; formula from the metafor FAQ)
  W <- solve(fit$V)
  X <- model.matrix(fit)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  I2 <- 100 * sum(fit$sigma2) / (sum(fit$sigma2) + (fit$k - fit$p) / sum(diag(P)))

  note <- apa_note(result)
  expect_match(
    note,
    "The 56 effect sizes were pooled with a multilevel random-effects model with random intercepts for district (11 levels) and school in district (56 levels), with variance components estimated by restricted maximum likelihood.",
    fixed = TRUE
  )
  expect_match(note, "*&sigma;*^2^~District~ = 0.07, *&sigma;*^2^~School\u00a0in\u00a0District~ = 0.03", fixed = TRUE)
  expect_match(note, paste0("*I*^2^ = ", sprintf("%.1f", I2), "%"), fixed = TRUE)

  perf <- apa_performance(fit)
  expect_equal(
    colnames(perf),
    c("*k*", "*Q*(55)", "*p*", "*&sigma;*^2^~District~", "*&sigma;*^2^~School\u00a0in\u00a0District~", "*I*^2^ (%)")
  )
  expect_equal(unpad(perf$`*I*^2^ (%)`), sprintf("%.1f", I2))
  perf_note <- apa_note(perf)
  expect_match(perf_note, "*k* = number of effect sizes", fixed = TRUE)
  expect_match(perf_note, "multilevel *I*^2^ (Cheung, 2014)", fixed = TRUE)

  all <- apa_performance(fit, metrics = "all")
  expect_true(all(c(
    "95% CI for *&sigma;*^2^~District~",
    "*&sigma;*~School\u00a0in\u00a0District~",
    "*I*^2^~District~ (%)"
  ) %in% colnames(all)))
  ci <- confint(fit)
  expect_equal(
    unpad(all$`95% CI for *&sigma;*^2^~District~`),
    paste0("[", sprintf("%.2f", ci[[1]]$random[1, "ci.lb"]), ",", sprintf("%.2f", ci[[1]]$random[1, "ci.ub"]), "]")
  )

  # Meta-regression
  fit2 <- metafor::rma.mv(yi, vi, mods = ~ year, random = ~ 1 | district / school, data = d)
  reg <- apa_parameters(fit2)
  expect_equal(reg$Variable, c("Constant", "Year"))
  expect_match(apa_note(reg), "The meta-regression of 56 effect sizes used a multilevel mixed-effects model", fixed = TRUE)
  expect_equal(
    colnames(apa_performance(fit2)),
    c("*k*", "*Q*~E~(54)", "*p*~E~", "*&sigma;*^2^~District~", "*&sigma;*^2^~School\u00a0in\u00a0District~", "*I*^2^ (%)", "*Q*~M~(1)", "*p*~M~")
  )

  # Univariate metrics are not available for multilevel models
  expect_false("*&tau;*^2^" %in% colnames(apa_performance(fit, metrics = c("k", "tau2"))))
})

test_that("unsupported metafor models give a clear error", {
  d <- metadat::dat.konstantopoulos2011
  fit <- metafor::rma.mv(yi, vi, random = ~ factor(school) | district, data = d)
  expect_error(apa_parameters(fit), "not yet supported")
  expect_error(apa_performance(fit), "not yet supported")
})
