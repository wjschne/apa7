library(apa7)
library(testthat)

d_htest <- mtcars
d_htest$am <- factor(d_htest$am, labels = c("Automatic", "Manual"))
d_htest$cyl <- factor(d_htest$cyl)
d_htest$vs <- factor(d_htest$vs, labels = c("V-shaped", "Straight"))

# Formatted numbers back to numbers (minus signs, padding, leading zeros)
as_num <- function(x) {
  x <- gsub(intToUtf8(8722), "-", x, fixed = TRUE)
  x <- gsub("[^0-9.-]", "", x)
  as.numeric(x)
}

ci_bounds <- function(x) {
  x <- gsub(intToUtf8(8722), "-", x, fixed = TRUE)
  parts <- strsplit(gsub("[^0-9.,-]", "", x), ",", fixed = TRUE)
  t(vapply(parts, as.numeric, numeric(2)))
}

test_that("apa_htest makes Welch's t tests with exact Cohen's d", {
  result <- apa_htest(d_htest, mpg + hp ~ am)
  expect_s3_class(result, "apa7_tbl")
  expect_equal(
    colnames(result),
    c(
      "Variable", "Automatic_*M*", "Automatic_*SD*", "Manual_*M*",
      "Manual_*SD*", "*t*", "*df*", "*p*", "Cohen's *d*", "95% CI"
    )
  )
  expect_equal(result$Variable, c("Mpg", "Hp"))

  x1 <- d_htest$mpg[d_htest$am == "Automatic"]
  x2 <- d_htest$mpg[d_htest$am == "Manual"]
  tt <- t.test(x1, x2)
  es <- effectsize::cohens_d(x1, x2, pooled_sd = FALSE)
  expect_equal(as_num(result$`*t*`[1]), round(unname(tt$statistic), 2))
  expect_equal(as_num(result$`*df*`[1]), round(unname(tt$parameter), 2))
  expect_equal(as_num(result$`Cohen's *d*`[1]), round(es$Cohens_d, 2))
  expect_equal(
    unname(ci_bounds(result$`95% CI`[1])[1, ]),
    round(c(es$CI_low, es$CI_high), 2)
  )
  expect_equal(as_num(result$`Automatic_*M*`[1]), round(mean(x1), 2))
  expect_equal(as_num(result$`Manual_*SD*`[1]), round(sd(x2), 2))
})

test_that("apa_htest gives the same d wherever it is called", {
  inside <- function() {
    dd <- d_htest
    apa_htest(dd, mpg ~ am)
  }
  expect_equal(
    as.data.frame(inside()),
    as.data.frame(apa_htest(d_htest, mpg ~ am))
  )
})

test_that("Student's t tests put df in the header and pool the SD", {
  result <- apa_htest(d_htest, mpg ~ am, var_equal = TRUE, effect_size = "hedges_g")
  expect_true("*t*(30)" %in% colnames(result))
  expect_true("Hedges' *g*" %in% colnames(result))
  x1 <- d_htest$mpg[d_htest$am == "Automatic"]
  x2 <- d_htest$mpg[d_htest$am == "Manual"]
  es <- effectsize::hedges_g(x1, x2, pooled_sd = TRUE)
  expect_equal(as_num(result$`Hedges' *g*`), round(es$Hedges_g, 2))
  note <- apa_note(result)
  expect_match(note, "Student's *t* test compares the groups of Am", fixed = TRUE)
  expect_match(note, "Hedges' *g* uses the pooled standard deviation.", fixed = TRUE)
})

test_that("one-sample tests compare with mu", {
  result <- apa_htest(d_htest, mpg ~ 1, mu = 20)
  expect_equal(
    colnames(result),
    c("Variable", "*M*", "*SD*", "*t*(31)", "*p*", "Cohen's *d*", "95% CI")
  )
  tt <- t.test(d_htest$mpg, mu = 20)
  expect_equal(as_num(result$`*t*(31)`), round(unname(tt$statistic), 2))
  expect_match(apa_note(result), "compares the mean with 20.", fixed = TRUE)

  result_w <- apa_htest(d_htest, mpg ~ 1, test = "wilcoxon", mu = 20, exact = FALSE)
  expect_equal(colnames(result_w)[1:4], c("Variable", "*Mdn*", "*V*", "*p*"))
  expect_match(apa_note(result_w), "Wilcoxon signed-rank test", fixed = TRUE)
})

test_that("Wilcoxon rank-sum tests show medians and rank-biserial r", {
  result <- apa_htest(d_htest, mpg + hp ~ am, test = "wilcoxon", exact = FALSE)
  expect_equal(
    colnames(result),
    c(
      "Variable", "Automatic_*Mdn*", "Manual_*Mdn*", "*W*", "*p*",
      "*r*~rb~", "95% CI"
    )
  )
  x1 <- d_htest$mpg[d_htest$am == "Automatic"]
  x2 <- d_htest$mpg[d_htest$am == "Manual"]
  es <- effectsize::rank_biserial(x1, x2)
  expect_equal(as_num(result$`*r*~rb~`[1]), round(es$r_rank_biserial, 2))
  # Bounded effect sizes have no leading zero
  expect_false(grepl("0\\.", result$`*r*~rb~`[1]))
  expect_match(
    apa_note(result),
    "*r*~rb~ = rank-biserial correlation; CI = confidence interval for *r*~rb~.",
    fixed = TRUE
  )
})

test_that("correlation tests show r, its interval, and t", {
  result <- apa_htest(d_htest, mpg + hp ~ wt, test = "cor")
  expect_equal(
    colnames(result),
    c("Variable", "*n*", "*r*", "95% CI", "*t*(30)", "*p*")
  )
  ct <- cor.test(d_htest$mpg, d_htest$wt)
  expect_equal(as_num(result$`*r*`[1]), round(unname(ct$estimate), 2))
  expect_match(apa_note(result), "Pearson correlations with Wt.", fixed = TRUE)

  result_s <- apa_htest(d_htest, mpg ~ wt, test = "cor", method = "spearman", exact = FALSE)
  expect_equal(colnames(result_s), c("Variable", "*n*", "&rho;", "*p*"))
  expect_match(apa_note(result_s), "&rho; = Spearman's rank correlation.", fixed = TRUE)

  result_k <- apa_htest(d_htest, mpg ~ wt, test = "cor", method = "kendall", exact = FALSE)
  expect_true("&tau;" %in% colnames(result_k))
  expect_false(grepl("0\\.", result_k$`&tau;`))
})

test_that("chi-square tests show unadjusted Cramer's V with two-sided intervals", {
  result <- suppressWarnings(apa_htest(d_htest, cyl + vs ~ am, test = "chisq"))
  expect_equal(
    colnames(result),
    c("Variable", "*N*", "*&chi;*^2^", "*df*", "*p*", "Cramér's *V*", "95% CI")
  )
  tab <- table(d_htest$cyl, d_htest$am)
  es <- suppressWarnings(
    effectsize::cramers_v(tab, adjust = FALSE, alternative = "two.sided")
  )
  expect_equal(as_num(result$`Cramér's *V*`[1]), round(es$Cramers_v, 2))
  expect_true(ci_bounds(result$`95% CI`[1])[1, 2] < 1)
  note <- apa_note(result)
  expect_match(note, "Chi-square tests of independence with Am.", fixed = TRUE)
  expect_match(note, "Yates's continuity correction", fixed = TRUE)

  result_phi <- suppressWarnings(
    apa_htest(d_htest, vs ~ am, test = "chisq", effect_size = "phi", correct = FALSE)
  )
  expect_true("&phi;" %in% colnames(result_phi))
  expect_false(grepl("Yates", apa_note(result_phi)))
  expect_error(
    suppressWarnings(apa_htest(d_htest, cyl ~ am, test = "chisq", effect_size = "phi")),
    "2 . 2 tables"
  )
})

test_that("apa_htest notes say when tests are one-sided", {
  result <- apa_htest(d_htest, mpg ~ am, alternative = "less")
  expect_match(
    apa_note(result),
    "The *p*-value is one-sided (alternative: less).",
    fixed = TRUE
  )
})

test_that("apa_htest stars and bolds significant results", {
  result <- apa_htest(d_htest, mpg + qsec ~ am, starred = "t", bolded = "p")
  expect_match(result$`*t*`[1], "\\*")
  expect_false(grepl("\\*", result$`*t*`[2]))
  expect_match(result$`*p*`[1], "^\\*\\*")
  note <- apa_note(result)
  expect_match(note, apa_p_star_note(), fixed = TRUE)
  expect_match(note, "Bold values", fixed = TRUE)
})

test_that("apa_htest can leave out effect sizes and descriptives", {
  result <- apa_htest(d_htest, mpg ~ am, effect_size = "none", descriptives = FALSE)
  expect_equal(colnames(result), c("Variable", "*t*(18.33)", "*p*"))
  expect_false(grepl("Cohen", apa_note(result)))
})

test_that("apa_htest drops missing values for each outcome", {
  dd <- d_htest
  dd$mpg[1:3] <- NA
  result <- apa_htest(dd, mpg ~ wt, test = "cor")
  expect_equal(as_num(result$`*n*`), 29)
})

test_that("apa_htest gives clear errors", {
  expect_error(apa_htest(mtcars$mpg, mpg ~ am), "data frame")
  expect_error(apa_htest(d_htest, ~am), "two-sided")
  expect_error(apa_htest(d_htest, mpg ~ am + vs), "only one variable")
  expect_error(apa_htest(d_htest, mpg ~ cyl), "exactly 2 groups")
  expect_error(apa_htest(d_htest, mpg ~ 1, test = "cor"), "needs a variable")
  expect_error(apa_htest(d_htest, nope ~ am), "nope")
  expect_error(apa_htest(d_htest, cyl ~ am), "numeric outcome")
  expect_error(apa_htest(d_htest, mpg ~ am, effect_size = "cramers_v"), "cohens_d")
  expect_error(apa_htest(d_htest, mpg ~ wt, test = "cor", effect_size = "cohens_d"), "correlation")
  expect_error(apa_htest(d_htest, mpg ~ am, ci = 95), "between 0 and 1")
})

test_that("apa_htest tables render as flextables with group spanners", {
  ft <- apa_flextable(apa_htest(d_htest, mpg ~ am), note = TRUE)
  expect_s3_class(ft, "flextable")
  expect_equal(flextable::nrow_part(ft, "header"), 2)
})
