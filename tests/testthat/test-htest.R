library(apa7)
library(testthat)

d_htest <- mtcars
d_htest$am <- factor(d_htest$am, labels = c("Automatic", "Manual"))
d_htest$cyl <- factor(d_htest$cyl)
d_htest$vs <- factor(d_htest$vs, labels = c("V-shaped", "Straight"))

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

d_paired <- data.frame(
  drug1 = sleep$extra[1:10],
  drug2 = sleep$extra[11:20]
)

test_that("paired t tests use Pair() and d_av by default", {
  result <- apa_htest(d_paired, Pair(drug1, drug2) ~ 1)
  expect_equal(
    colnames(result),
    c(
      "Variable", "Drug 1_*M*", "Drug 1_*SD*", "Drug 2_*M*", "Drug 2_*SD*",
      "*t*(9)", "*p*", "*d*~av~", "95% CI"
    )
  )
  expect_equal(result$Variable, "Drug 1 vs. Drug 2")
  tt <- t.test(d_paired$drug1, d_paired$drug2, paired = TRUE)
  expect_equal(as_num(result$`*t*(9)`), round(unname(tt$statistic), 2))
  es <- effectsize::repeated_measures_d(
    d_paired$drug1, d_paired$drug2, method = "av", adjust = FALSE
  )
  expect_equal(as_num(result$`*d*~av~`), round(es$d_av, 2))
  expect_equal(
    unname(ci_bounds(result$`95% CI`)[1, ]),
    round(c(es$CI_low, es$CI_high), 2)
  )
  note <- apa_note(result)
  expect_match(note, "Paired *t* test compares Drug 1 with Drug 2.", fixed = TRUE)
  expect_match(note, "*d*~av~ divides the mean difference by the average", fixed = TRUE)
})

test_that("paired t tests offer d_z, d_rm, and Hedges' g versions", {
  result_z <- apa_htest(d_paired, Pair(drug1, drug2) ~ 1, effect_size = "d_z")
  es_z <- effectsize::repeated_measures_d(
    d_paired$drug1, d_paired$drug2, method = "z", adjust = FALSE
  )
  expect_equal(as_num(result_z$`*d*~z~`), round(es_z$d_z, 2))
  expect_match(apa_note(result_z), "standard deviation of the differences (Lakens, 2013).", fixed = TRUE)

  result_g <- apa_htest(d_paired, Pair(drug1, drug2) ~ 1, effect_size = "hedges_g")
  es_g <- effectsize::repeated_measures_d(
    d_paired$drug1, d_paired$drug2, method = "av", adjust = TRUE
  )
  expect_true("*g*~av~" %in% colnames(result_g))
  expect_equal(as_num(result_g$`*g*~av~`), round(es_g$d_av, 2))
  expect_match(apa_note(result_g), "with a small-sample correction", fixed = TRUE)

  result_rm <- apa_htest(d_paired, Pair(drug1, drug2) ~ 1, effect_size = "d_rm")
  expect_true("*d*~rm~" %in% colnames(result_rm))

  expect_error(
    apa_htest(d_paired, Pair(drug1, drug2) ~ 1, effect_size = "cramers_v"),
    "d_av"
  )
})

test_that("several pairs share pair_labels headings", {
  set.seed(1)
  dw <- data.frame(
    anx_pre = rnorm(30, 10), anx_post = rnorm(30, 9),
    dep_pre = rnorm(30, 5), dep_post = rnorm(30, 5)
  )
  result <- apa_htest(
    dw,
    Pair(anx_pre, anx_post) + Pair(dep_pre, dep_post) ~ 1,
    pair_labels = c("Pretest", "Posttest")
  )
  expect_equal(result$Variable, c("Anx Pre vs. Anx Post", "Dep Pre vs. Dep Post"))
  expect_true(all(c("Pretest_*M*", "Posttest_*SD*") %in% colnames(result)))
  expect_match(
    apa_note(result),
    "Paired *t* tests compare the two variables in each row.",
    fixed = TRUE
  )

  result_default <- apa_htest(dw, Pair(anx_pre, anx_post) + Pair(dep_pre, dep_post) ~ 1)
  expect_true("First_*M*" %in% colnames(result_default))
  expect_error(
    apa_htest(dw, Pair(anx_pre, anx_post) ~ 1, pair_labels = "Pre"),
    "length 2"
  )
})

test_that("paired Wilcoxon signed-rank tests show medians and rank-biserial r", {
  result <- apa_htest(d_paired, Pair(drug1, drug2) ~ 1, test = "wilcoxon", exact = FALSE)
  expect_equal(
    colnames(result),
    c("Variable", "Drug 1_*Mdn*", "Drug 2_*Mdn*", "*V*", "*p*", "*r*~rb~", "95% CI")
  )
  # Exact values keep their decimals
  expect_equal(trimws(result$`*r*~rb~`), paste0(intToUtf8(8722), "1.00"))
  expect_match(
    apa_note(result),
    "Wilcoxon signed-rank test compares Drug 1 with Drug 2.",
    fixed = TRUE
  )
})

test_that("paired tests use complete pairs and say what mu is", {
  dd <- d_paired
  dd$drug2[1] <- NA
  result <- apa_htest(dd, Pair(drug1, drug2) ~ 1, mu = -1)
  expect_true("*t*(8)" %in% colnames(result))
  expect_match(
    apa_note(result),
    paste0("hypothesized difference between variables is ", intToUtf8(8722), "1."),
    fixed = TRUE
  )
})

test_that("Pair() gives clear errors", {
  expect_error(apa_htest(d_paired, Pair(drug1, drug2) + drug1 ~ 1), "mix")
  expect_error(apa_htest(d_paired, Pair(drug1) ~ 1), "two variable names")
  expect_error(apa_htest(d_paired, Pair(drug1, drug2 * 2) ~ 1), "two variable names")
  expect_error(apa_htest(d_htest, Pair(mpg, hp) ~ am), "1")
  expect_error(apa_htest(d_paired, Pair(drug1, drug2) ~ 1, test = "cor"), "Pair")
  expect_error(apa_htest(d_paired, Pair(drug1, nope) ~ 1), "nope")
})

test_that("paired row labels keep 'vs.' in flextables", {
  result <- apa_htest(d_paired, Pair(drug1, drug2) ~ 1)
  ft <- apa_flextable(result)
  expect_equal(ft$body$dataset[[1]], "Drug 1 vs. Drug 2")
})
