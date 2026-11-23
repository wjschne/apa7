library(apa7)
library(testthat)

skip_if_not_installed("metafor")
skip_if_not_installed("metadat")

unpad <- function(x) gsub("[\u2002-\u200a ]", "", x)

bcg <- function() {
  metafor::escalc(
    measure = "RR",
    ai = tpos, bi = tneg, ci = cpos, di = cneg,
    data = metadat::dat.bcg
  )
}

test_that("separate tau2: each subgroup's estimate, then the overall estimate, and Q between", {
  d <- bcg()
  fit <- metafor::rma(yi, vi, data = d)
  result <- apa_subgroups(fit, alloc)
  expect_s3_class(result, "apa7_tbl")
  expect_equal(colnames(result), c("Alloc", "*k*", "*RR*", "95% CI", "*p*", "*I*^2^ (%)"))
  expect_equal(result$Alloc, c("alternate", "random", "systematic", "Overall"))
  expect_equal(attr(result, "rule_above"), "Overall")

  random <- metafor::rma(yi, vi, data = d, subset = alloc == "random")
  expect_equal(result$`*k*`[2], "7")
  expect_equal(unpad(result$`*RR*`[2]), sprintf("%.2f", exp(as.vector(random$b))))
  expect_equal(unpad(result$`*RR*`[4]), sprintf("%.2f", exp(as.vector(fit$b))))

  fits <- lapply(split(seq_len(nrow(d)), d$alloc), \(i) metafor::rma(d$yi[i], d$vi[i]))
  between <- metafor::rma(sapply(fits, \(f) f$b[1]), sei = sapply(fits, \(f) f$se), method = "EE")
  note <- apa_note(result)
  expect_match(note, paste0("Test of subgroup differences: *Q*~between~(2) = ", sprintf("%.2f", between$QE)), fixed = TRUE)
  expect_match(note, "Each subgroup of Alloc was pooled with its own random-effects model", fixed = TRUE)
  expect_match(note, "Effect sizes are risk ratios, analyzed on the log scale and exponentiated", fixed = TRUE)

  # Quoted names work too
  expect_equal(apa_subgroups(fit, "alloc"), result)
})

test_that("common tau2 uses a meta-regression and its omnibus test", {
  d <- bcg()
  fit <- metafor::rma(yi, vi, data = d)
  result <- apa_subgroups(fit, alloc, tau2 = "common")
  expect_false("*I*^2^ (%)" %in% colnames(result))
  contrast <- metafor::rma(yi, vi, mods = ~ factor(alloc), data = d)
  note <- apa_note(result)
  expect_match(note, paste0("*Q*~M~(2) = ", sprintf("%.2f", contrast$QM)), fixed = TRUE)
  expect_match(note, "with a common *&tau;*^2^", fixed = TRUE)

  # Knapp-Hartung gives an F test
  kh <- apa_subgroups(metafor::rma(yi, vi, data = d, test = "knha"), alloc, tau2 = "common")
  expect_match(apa_note(kh), "*F*(2, 10) = ", fixed = TRUE)
})

test_that("subgroup vectors, missing subgroups, and errors", {
  d <- bcg()
  fit <- metafor::rma(yi, vi, data = d)
  by_latitude <- apa_subgroups(fit, d$ablat > 30)
  expect_equal(colnames(by_latitude)[1], "Subgroup")
  expect_match(apa_note(by_latitude), "Each subgroup was pooled", fixed = TRUE)

  d$alloc[1] <- NA
  missing <- apa_subgroups(metafor::rma(yi, vi, data = d), alloc)
  expect_equal(missing$`*k*`[2], "6")
  expect_match(apa_note(missing), "Effect sizes with a missing value of Alloc (*n* = 1) are not in any subgroup.", fixed = TRUE)

  expect_error(apa_subgroups(fit, nope), "must name a column")
  expect_error(apa_subgroups(fit, rep("a", 13)), "at least 2 subgroups")
  expect_error(apa_subgroups(fit, c("a", "b")), "one value for each")
  expect_error(apa_subgroups(metafor::rma(yi, vi, mods = ~ablat, data = d), alloc), "without moderators")
  expect_error(apa_subgroups(metafor::rma(yi, vi, data = d, method = "EE"), alloc), "random-effects")
})
