library(apa7)
library(testthat)

d_mt <- mtcars
d_mt$am <- factor(d_mt$am, labels = c("Automatic", "Manual"))

test_that("two-sample t test objects approximate d from t", {
  skip_if_not_installed("effectsize")
  tt <- t.test(mpg ~ am, data = d_mt)
  result <- apa_htest(tt)
  expect_equal(colnames(result), c("Variable", "*t*(18.33)", "*p*", "Cohen's *d*", "95% CI"))
  expect_equal(result$Variable, "mpg by am")
  expect_equal(as_num(result$`*t*(18.33)`), round(unname(tt$statistic), 2))
  d_approx <- 2 * unname(tt$statistic) / sqrt(unname(tt$parameter))
  expect_equal(as_num(result$`Cohen's *d*`), round(d_approx, 2))
  note <- apa_note(result)
  expect_match(note, "The result is from Welch's *t* test.", fixed = TRUE)
  expect_match(note, "which assumes groups of equal size", fixed = TRUE)

  student <- apa_htest(t.test(mpg ~ am, data = d_mt, var.equal = TRUE))
  expect_match(apa_note(student), "Student's *t* test", fixed = TRUE)
  expect_equal(colnames(student)[2], "*t*(30)")
})

test_that("one-sample and paired t test objects give exact d", {
  skip_if_not_installed("effectsize")
  one <- t.test(d_mt$mpg, mu = 18)
  result <- apa_htest(list(MPG = one))
  expect_equal(result$Variable, "MPG")
  n <- sum(!is.na(d_mt$mpg))
  expect_equal(
    as_num(result$`Cohen's *d*`),
    round((mean(d_mt$mpg) - 18) / sd(d_mt$mpg), 2)
  )
  note <- apa_note(result)
  expect_match(note, "one-sample *t* test", fixed = TRUE)
  expect_match(note, "The hypothesized mean is 18.", fixed = TRUE)
  expect_match(note, "computed from *t* as *t*/&radic;*n*.", fixed = TRUE)

  pre <- sleep$extra[1:10]
  post <- sleep$extra[11:20]
  paired <- apa_htest(t.test(pre, post, paired = TRUE))
  expect_true("*d*~z~" %in% colnames(paired))
  expect_equal(as_num(paired$`*d*~z~`), round(mean(pre - post) / sd(pre - post), 2))
  # The exact interval matches effectsize's from the differences
  es <- effectsize::cohens_d(pre - post)
  expect_equal(ci_bounds(paired$`95% CI`)[1, ], round(c(es$CI_low, es$CI_high), 2), ignore_attr = TRUE)
  expect_match(apa_note(paired), "standard deviation of the differences", fixed = TRUE)
})

test_that("lists of correlation and chi-square test objects get one row each", {
  skip_if_not_installed("effectsize")
  cors <- apa_htest(list(
    Weight = cor.test(d_mt$mpg, d_mt$wt),
    Displacement = cor.test(d_mt$mpg, d_mt$disp, alternative = "less")
  ))
  expect_equal(cors$Variable, c("Weight", "Displacement"))
  expect_equal(colnames(cors), c("Variable", "*n*", "*r*", "95% CI", "*t*(30)", "*p*"))
  expect_equal(as_num(cors$`*r*`[1]), round(cor(d_mt$mpg, d_mt$wt), 2))
  note <- apa_note(cors)
  expect_match(note, "Results are from Pearson correlation tests.", fixed = TRUE)
  expect_match(note, "one-sided (alternative: less)", fixed = TRUE)

  tab <- table(factor(d_mt$cyl), d_mt$am)
  ch <- suppressWarnings(chisq.test(tab))
  chi <- apa_htest(list(Cylinders = ch))
  v <- effectsize::cramers_v(tab, adjust = FALSE)
  expect_equal(as_num(chi$`Cramér's *V*`), round(v$Cramers_v, 2))
  expect_equal(as_num(chi$`*N*`), 32)
  expect_match(apa_note(chi), "chi-square test of independence", fixed = TRUE)
  expect_true("*w*" %in% colnames(apa_htest(ch, effect_size = "cohens_w")) ||
    any(grepl("w", colnames(apa_htest(ch, effect_size = "cohens_w")))))
  expect_error(apa_htest(ch, effect_size = "phi"), "2 × 2|2 x 2|needs 2")
})

test_that("Wilcoxon test objects have no effect size; unsupported tests give errors", {
  wt <- suppressWarnings(wilcox.test(mpg ~ am, data = d_mt))
  result <- apa_htest(wt)
  expect_equal(colnames(result), c("Variable", "*W*", "*p*"))
  expect_match(apa_note(result), "do not include the sample sizes", fixed = TRUE)
  expect_error(apa_htest(wt, effect_size = "rank_biserial"), "No effect size")
  expect_error(
    apa_htest(list(t.test(d_mt$mpg), cor.test(d_mt$mpg, d_mt$wt))),
    "same kind"
  )
  expect_error(apa_htest(fisher.test(table(d_mt$vs, d_mt$am))), "does not support")
  expect_error(apa_htest(t.test(d_mt$mpg), effect_size = "hedges_g"), "must be")
  # The data interface still works
  expect_equal(nrow(apa_htest(d_mt, mpg ~ am)), 1)
})
