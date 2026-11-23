library(apa7)
library(testthat)

skip_if_not_installed("metafor")
skip_if_not_installed("metadat")

unpad <- function(x) gsub("[\u2002-\u200a ]", "", x)
num <- function(v) gsub("-", "\u2212", sprintf("%.2f", v))

bcg_fit <- function(...) {
  d <- metafor::escalc(
    measure = "RR",
    ai = tpos, bi = tneg, ci = cpos, di = cneg,
    data = metadat::dat.bcg
  )
  metafor::rma(yi, vi, data = d, ...)
}

test_that("apa_publication_bias shows Egger's test, the rank test, and trim and fill", {
  fit <- bcg_fit()
  result <- apa_publication_bias(fit)
  expect_s3_class(result, "apa7_tbl")
  expect_equal(colnames(result), c("Method", "Statistic", "Value", "*p*", "Adjusted *RR*", "95% CI"))
  expect_equal(result$Method, c("Egger's regression test", "Rank correlation test", "Trim and fill"))
  expect_equal(result$Statistic, c("*z*", "*&tau;*", "*k*~0~"))

  egger <- metafor::regtest(fit)
  expect_equal(unpad(result$Value[1]), num(egger$zval))
  expect_equal(unpad(result$`Adjusted *RR*`[1]), num(exp(egger$est)))
  rank <- metafor::ranktest(fit)
  expect_equal(unpad(result$Value[2]), sub("^0", "", sprintf("%.2f", rank$tau)))
  tf <- metafor::trimfill(fit)
  expect_equal(unpad(result$Value[3]), as.character(tf$k0))
  expect_equal(unpad(result$`Adjusted *RR*`[3]), num(exp(as.numeric(tf$beta))))
  expect_equal(result$`*p*`[3], "")
  expect_equal(result$`Adjusted *RR*`[2], "")

  note <- apa_note(result)
  expect_match(note, "The unadjusted pooled estimate of the 13 studies is *RR* = 0.49, 95% CI [0.34, 0.70].", fixed = TRUE)
  expect_match(note, "Egger's regression test is the test of the standard error", fixed = TRUE)
  expect_match(note, "Trim and fill (Duval & Tweedie, 2000; L0 estimator) imputed 1 study on the right side of the funnel plot", fixed = TRUE)
  expect_match(note, "*k*~0~ = number of imputed studies", fixed = TRUE)
  expect_equal(apa_flextable(result)$body$dataset[[1]], result$Method)
})

test_that("methods, transform, and Knapp-Hartung options", {
  fit <- bcg_fit()
  all <- apa_publication_bias(fit, methods = c("egger", "rank", "trimfill", "fsn"), transform = FALSE)
  expect_equal(nrow(all), 4)
  expect_equal(all$Statistic[4], "*N*~fs~")
  expect_equal(unpad(all$Value[4]), as.character(metafor::fsn(fit)$fsnum))
  expect_true("Adjusted ln(*RR*)" %in% colnames(all))
  expect_match(apa_note(all), "fail-safe *N* (general method)", fixed = TRUE)

  rank_only <- apa_publication_bias(fit, methods = "rank")
  expect_equal(colnames(rank_only), c("Method", "Statistic", "Value", "*p*"))
  expect_no_match(apa_note(rank_only), "CI =", fixed = TRUE)

  kh <- apa_publication_bias(bcg_fit(test = "knha"), methods = "egger")
  expect_equal(kh$Statistic, "*t*(11)")
})

test_that("apa_publication_bias gives clear errors", {
  fit <- bcg_fit()
  expect_error(apa_publication_bias(fit, methods = "funnel"), "methods")
  expect_error(apa_publication_bias(lm(mpg ~ wt, mtcars)), "metafor::rma")
  d <- metafor::escalc(
    measure = "RR",
    ai = tpos, bi = tneg, ci = cpos, di = cneg,
    data = metadat::dat.bcg
  )
  expect_error(apa_publication_bias(metafor::rma(yi, vi, mods = ~ablat, data = d)), "without moderators")
  mv <- metafor::rma.mv(yi, vi, random = ~ 1 | district / school, data = metadat::dat.konstantopoulos2011)
  expect_error(apa_publication_bias(mv), "multilevel")
})
