library(apa7)
library(testthat)

d_anova <- mtcars
d_anova$cyl <- factor(d_anova$cyl)
d_anova$am <- factor(d_anova$am)

anova_rm_data <- function() {
  set.seed(1)
  d <- expand.grid(id = factor(1:12), phase = factor(c("pre", "post", "fu")))
  d$group <- factor(ifelse(as.integer(d$id) <= 6, "A", "B"))
  d$y <- stats::rnorm(nrow(d)) + as.integer(d$phase)
  d
}

test_that("apa_anova makes a source table for aov models", {
  fit <- aov(mpg ~ cyl * am, data = d_anova)
  result <- apa_anova(fit)
  expect_s3_class(result, "apa7_tbl")
  expect_equal(
    colnames(result),
    c("Source", "*SS*", "*df*", "*MS*", "*F*", "*p*", "*&eta;*^2^~p~")
  )
  expect_equal(result$Source, c("Cyl", "Am", "Cyl × Am", "Error"))
  # Error row has no test or effect size
  expect_equal(result$`*F*`[4], "")
  expect_equal(result$`*&eta;*^2^~p~`[4], "")

  es <- effectsize::eta_squared(fit, partial = TRUE)
  expect_equal(
    trimws(result$`*&eta;*^2^~p~`[1:3]),
    sub("^0", "", sprintf("%.2f", es$Eta2_partial))
  )
})

test_that("apa_anova gives the same table for lm and aov models", {
  fit_aov <- aov(mpg ~ cyl * am, data = d_anova)
  fit_lm <- lm(mpg ~ cyl * am, data = d_anova)
  expect_equal(
    as.data.frame(apa_anova(fit_aov)),
    as.data.frame(apa_anova(fit_lm))
  )
})

test_that("apa_anova selects effect sizes", {
  fit <- aov(mpg ~ cyl + am, data = d_anova)
  expect_true("*&omega;*^2^" %in% colnames(apa_anova(fit, effect_size = "omega", partial = FALSE)))
  expect_true("*&omega;*^2^~p~" %in% colnames(apa_anova(fit, effect_size = "omega")))
  expect_true("*&epsilon;*^2^~p~" %in% colnames(apa_anova(fit, effect_size = "epsilon")))
  expect_true("*&eta;*^2^" %in% colnames(apa_anova(fit, partial = FALSE)))
  expect_true("*&eta;*^2^~G~" %in% colnames(apa_anova(fit, generalized = TRUE)))
  expect_equal(ncol(apa_anova(fit, effect_size = NULL)), 6)
  expect_error(apa_anova(fit, effect_size = "omega", generalized = TRUE), "Generalized")
})

test_that("apa_anova shows two-sided effect size intervals", {
  fit <- aov(mpg ~ cyl * am, data = d_anova)
  result <- apa_anova(
    fit,
    anova_parameters = c("Parameter", "F", "p", "Effect_Size", "CI"),
    ci = 0.90
  )
  expect_true("90% CI" %in% colnames(result))
  es <- effectsize::eta_squared(fit, partial = TRUE, ci = 0.90, alternative = "two.sided")
  expect_match(result$`90% CI`[1], sub("^0", "", sprintf("%.2f", es$CI_high[1])), fixed = TRUE)
  # Upper bounds are not fixed at 1, and zero bounds keep their decimals
  expect_false(any(grepl("1.00]", result$`90% CI`, fixed = TRUE)))
  expect_match(result$`90% CI`[3], "[.00,", fixed = TRUE)
  expect_equal(result$`90% CI`[4], "")
})

test_that("apa_anova stars and bolds significant effects", {
  fit <- aov(mpg ~ cyl * am, data = d_anova)
  result <- apa_anova(fit, starred = "F", bolded = "Parameter")
  expect_match(result$`*F*`[1], "^\\*\\*\\*^", fixed = TRUE)
  expect_false(grepl("*", result$`*F*`[3], fixed = TRUE))
  expect_match(result$Source[1], "^\\*\\*")
})

test_that("apa_anova computes Type 2 and 3 sums of squares with car", {
  skip_if_not_installed("car")
  fit <- lm(mpg ~ cyl * am, data = d_anova)
  t2 <- apa_anova(fit, type = 2)
  ca <- car::Anova(fit, type = 2)
  expect_equal(trimws(t2$`*SS*`[1]), sprintf("%.2f", ca$`Sum Sq`[1]))
  expect_equal(apa_anova(fit, type = "II"), t2)
  # effectsize reminds users about contrasts for Type 3 tests
  t3 <- suppressMessages(apa_anova(fit, type = 3))
  expect_false(identical(t2$`*SS*`, t3$`*SS*`))
  # car::Anova tables can be passed directly, and type is then ignored
  expect_equal(as.data.frame(apa_anova(ca)), as.data.frame(t2))
  expect_warning(apa_anova(ca, type = 3), "ignored")
  expect_error(apa_anova(fit, type = 4), "1, 2, or 3")
})

test_that("apa_anova accepts stats::anova tables", {
  fit <- lm(mpg ~ cyl * am, data = d_anova)
  expect_equal(
    as.data.frame(apa_anova(stats::anova(fit))),
    as.data.frame(apa_anova(fit))
  )
})

test_that("apa_anova shows error strata of repeated-measures models", {
  fit <- aov(y ~ group * phase + Error(id / phase), data = anova_rm_data())
  result <- apa_anova(fit)
  expect_s3_class(result, "apa7_tbl")
  expect_equal(
    sub("^ ", "", result$Source),
    c(
      "Between subjects", "Group", "Error",
      "Within subjects", "Phase", "Group × Phase", "Error"
    )
  )
  # Heading rows are blank
  expect_true(all(unlist(result[1, -1]) == ""))
  expect_s3_class(apa_flextable(result), "flextable")
  expect_error(apa_anova(fit, type = 3), "Type 1")
})

test_that("apa_anova names effects of multiple within-subjects strata", {
  d <- anova_rm_data()
  d <- merge(d, data.frame(time = factor(c("am", "pm"))))
  d$y <- d$y + stats::rnorm(nrow(d))
  fit <- aov(y ~ phase * time + Error(id / (phase * time)), data = d)
  result <- apa_anova(fit)
  headings <- result$Source[!startsWith(result$Source, " ")]
  expect_equal(
    headings,
    c(
      "Between subjects",
      "Within subjects: Phase",
      "Within subjects: Time",
      "Within subjects: Phase × Time"
    )
  )
})

test_that("apa_anova reports afex models with per-effect error terms", {
  skip_if_not_installed("afex")
  data(obk.long, package = "afex")
  fit <- suppressMessages(
    afex::aov_ez(
      "id", "value", obk.long,
      between = "treatment", within = "phase", fun_aggregate = mean
    )
  )
  result <- apa_anova(fit)
  expect_s3_class(result, "apa7_tbl")
  expect_equal(
    colnames(result),
    c("Effect", "*df*~effect~", "*df*~error~", "*MSE*", "*F*", "*p*", "*&eta;*^2^~G~")
  )
  expect_equal(result$Effect, c("Treatment", "Phase", "Treatment × Phase"))
  ges <- fit$anova_table$ges
  expect_equal(trimws(result$`*&eta;*^2^~G~`), sub("^0", "", sprintf("%.2f", ges)))

  # Partial eta squared, with two-sided intervals
  result_p <- apa_anova(
    fit,
    generalized = FALSE,
    anova_parameters = c("Parameter", "F", "p", "Effect_Size", "CI")
  )
  expect_true(all(c("*&eta;*^2^~p~", "95% CI") %in% colnames(result_p)))
  expect_false(any(grepl("1.00]", result_p$`95% CI`, fixed = TRUE)))

  # No intervals for generalized eta squared
  expect_warning(
    result_g <- apa_anova(fit, anova_parameters = c("Parameter", "F", "Effect_Size", "CI")),
    "generalized"
  )
  expect_false("95% CI" %in% colnames(result_g))
  expect_warning(apa_anova(fit, type = 2), "ignored")
})

test_that("apa_parameters returns ANOVA tables for aov models", {
  fit <- aov(mpg ~ cyl * am, data = d_anova)
  expect_equal(apa_parameters(fit), apa_anova(fit))
  expect_equal(
    apa_parameters(fit, effect_size = "omega"),
    apa_anova(fit, effect_size = "omega")
  )
  fit_rm <- aov(y ~ group * phase + Error(id / phase), data = anova_rm_data())
  expect_equal(apa_parameters(fit_rm), apa_anova(fit_rm))
  # lm models still give coefficients
  expect_true("Variable" %in% colnames(apa_parameters(lm(mpg ~ wt, data = d_anova))))
})

test_that("apa_anova rejects unsupported objects", {
  m1 <- lm(mpg ~ wt, data = d_anova)
  m2 <- lm(mpg ~ wt + hp, data = d_anova)
  expect_error(apa_anova(stats::anova(m1, m2)), "apa_performance_comparison")
  g <- glm(am ~ wt, data = d_anova, family = binomial)
  expect_error(apa_anova(g), "generalized linear models")
  expect_error(apa_anova(stats::anova(g, test = "Chisq")), "no F|has none")
  expect_error(apa_anova(1:3), "does not support")
})

# apa_note ----
test_that("apa_note describes aov tables", {
  fit <- aov(mpg ~ cyl * am, data = d_anova)
  notes <- apa_note(apa_anova(fit), collapse = NULL)
  expect_equal(
    notes,
    c(
      "Sums of squares are sequential (Type I).",
      "*&eta;*^2^~p~ = partial eta squared."
    )
  )
  expect_equal(apa_note(apa_anova(fit)), paste(notes, collapse = " "))

  # Effect size choices
  expect_match(
    apa_note(apa_anova(fit, effect_size = "omega", partial = FALSE)),
    "*&omega;*^2^ = omega squared.",
    fixed = TRUE
  )
  expect_match(
    apa_note(apa_anova(fit, effect_size = "epsilon")),
    "partial epsilon squared",
    fixed = TRUE
  )
  expect_equal(
    apa_note(apa_anova(fit, effect_size = NULL)),
    "Sums of squares are sequential (Type I)."
  )

  # Stars and bold
  starred <- apa_note(apa_anova(fit, starred = "F"), collapse = NULL)
  expect_equal(utils::tail(starred, 1), paste0(apa_p_star_note(), "."))
  bolded <- apa_note(apa_anova(fit, bolded = "Parameter"), collapse = NULL)
  expect_match(utils::tail(bolded, 1), "^Bold values")
  expect_false(any(grepl("Bold", apa_note(apa_anova(fit, starred = "F")))))
})

test_that("apa_note gives the type of sums of squares", {
  skip_if_not_installed("car")
  fit <- aov(mpg ~ cyl * am, data = d_anova)
  expect_match(
    apa_note(suppressMessages(apa_anova(fit, type = 2))),
    "^Type II sums of squares[.]"
  )
  expect_match(
    apa_note(suppressMessages(apa_anova(fit, type = 3))),
    "^Type III sums of squares[.]"
  )
  fit_lm <- lm(mpg ~ cyl * am, data = d_anova)
  expect_match(
    apa_note(suppressMessages(apa_anova(car::Anova(fit_lm, type = 3)))),
    "^Type III"
  )
  expect_match(
    apa_note(apa_anova(car::Anova(fit_lm, type = 2))),
    "^Type II sums"
  )
  expect_match(apa_note(apa_anova(stats::anova(fit_lm))), "^Sums of squares are sequential")

  # Unknown heading: no sentence about sums of squares
  tab <- stats::anova(fit_lm)
  attr(tab, "heading") <- NULL
  expect_false(grepl("sums of squares", apa_note(apa_anova(tab)), ignore.case = TRUE))
})

test_that("apa_note describes repeated-measures aov tables", {
  d <- anova_rm_data()
  fit <- aov(y ~ group * phase + Error(id / phase), data = d)
  expect_match(apa_note(apa_anova(fit)), "^Sums of squares are sequential")
})

test_that("apa_note describes afex tables and sphericity corrections", {
  skip_if_not_installed("afex")
  data(obk.long, package = "afex")
  fit_gg <- suppressMessages(afex::aov_ez(
    "id", "value", obk.long,
    between = "treatment", within = c("phase", "hour"),
    fun_aggregate = mean
  ))
  notes <- apa_note(apa_anova(fit_gg), collapse = NULL)
  expect_equal(notes[1], "Type III sums of squares.")
  expect_equal(
    notes[2],
    paste0(
      "Degrees of freedom and *p*-values for effects involving Phase or Hour are Greenhouse",
      intToUtf8(8211),
      "Geisser corrected (Greenhouse & Geisser, 1959)."
    )
  )
  expect_equal(
    notes[3],
    "*MSE* = mean square error; *&eta;*^2^~G~ = generalized eta squared (Olejnik & Algina, 2003)."
  )

  # Huynh-Feldt and no correction
  fit_hf <- suppressMessages(afex::aov_ez(
    "id", "value", obk.long,
    between = "treatment", within = "phase",
    fun_aggregate = mean, anova_table = list(correction = "HF")
  ))
  expect_match(apa_note(apa_anova(fit_hf)), "Huynh", fixed = TRUE)
  expect_match(apa_note(apa_anova(fit_hf)), "involving Phase are", fixed = TRUE)

  fit_none <- suppressMessages(afex::aov_ez(
    "id", "value", obk.long,
    between = "treatment", within = "phase",
    fun_aggregate = mean, anova_table = list(correction = "none")
  ))
  expect_match(
    apa_note(apa_anova(fit_none)),
    "Degrees of freedom are not corrected for violations of sphericity.",
    fixed = TRUE
  )

  # Between-subjects only, or two-level within factors: nothing to correct
  fit_between <- suppressMessages(afex::aov_ez(
    "id", "value", obk.long,
    between = c("treatment", "gender"),
    fun_aggregate = mean
  ))
  expect_false(grepl("sphericity|corrected", apa_note(apa_anova(fit_between))))

  d2 <- obk.long[obk.long$phase != "fup", ]
  d2$phase <- droplevels(d2$phase)
  fit_two <- suppressMessages(afex::aov_ez(
    "id", "value", d2,
    within = "phase", fun_aggregate = mean
  ))
  expect_false(grepl("corrected", apa_note(apa_anova(fit_two))))

  # No MSE column, no MSE definition
  no_mse <- apa_anova(
    fit_gg,
    anova_parameters = c("Parameter", "df", "df_error", "F", "p", "Effect_Size")
  )
  expect_false(grepl("MSE", apa_note(no_mse), fixed = TRUE))
})

test_that("apa_note survives dplyr verbs and errors without a record", {
  fit <- aov(mpg ~ cyl * am, data = d_anova)
  tbl <- apa_anova(fit)
  expect_equal(apa_note(dplyr::filter(tbl, Source != "Error")), apa_note(tbl))
  expect_error(apa_note(mtcars), "no table note information")
  expect_equal(apa_note(apa_parameters(fit)), apa_note(tbl))
})

# Linear mixed models (lmerTest) ----
lmer_data <- function() {
  d <- lme4::sleepstudy
  d$Group <- factor(rep(c("A", "B"), length.out = 18))[as.integer(d$Subject)]
  d
}

test_that("apa_anova() gives F tests of lmer fixed effects with Kenward-Roger df", {
  skip_if_not_installed("lmerTest")
  skip_if_not_installed("pbkrtest")
  fit <- lme4::lmer(Reaction ~ Days * Group + (Days | Subject), data = lmer_data())
  result <- apa_anova(fit)
  expect_equal(
    colnames(result),
    c("Effect", "*df*~effect~", "*df*~error~", "*F*", "*p*")
  )
  expect_equal(result$Effect, c("Days", "Group", "Days × Group"))
  tab <- anova(lmerTest::as_lmerModLmerTest(fit), ddf = "Kenward-Roger")
  expect_equal(as_num(result$`*F*`), round(tab$`F value`, 2))
  expect_equal(as_num(result$`*df*~error~`), round(tab$DenDF, 2))
  note <- apa_note(result)
  expect_match(note, "*F* tests of fixed effects are Type III.", fixed = TRUE)
  expect_match(note, "Kenward–Roger approximations", fixed = TRUE)
  # No effect size unless asked for
  expect_no_match(note, "eta", fixed = TRUE)
})

test_that("apa_anova() offers Satterthwaite df, other types, and effect sizes from F", {
  skip_if_not_installed("lmerTest")
  fit <- lmerTest::lmer(Reaction ~ Days * Group + (Days | Subject), data = lmer_data())
  result <- apa_anova(
    fit,
    ddf = "Satterthwaite",
    type = 2,
    effect_size = "eta",
    anova_parameters = c("Parameter", "df", "df_error", "F", "p", "Effect_Size", "CI")
  )
  tab <- anova(fit, ddf = "Satterthwaite", type = 2)
  es <- effectsize::F_to_eta2(
    tab$`F value`, tab$NumDF, tab$DenDF, alternative = "two.sided"
  )
  expect_true("*&eta;*^2^~p~" %in% colnames(result))
  expect_equal(
    as_num(result$`*&eta;*^2^~p~`),
    round(es$Eta2_partial, 2)
  )
  expect_true("95% CI" %in% colnames(result))
  note <- apa_note(result)
  expect_match(note, "Type II.", fixed = TRUE)
  expect_match(note, "Satterthwaite approximations", fixed = TRUE)
  expect_match(note, "computed from *F* and its degrees of freedom", fixed = TRUE)

  expect_error(apa_anova(fit, effect_size = "eta", generalized = TRUE), "Generalized")
  expect_error(apa_anova(fit, effect_size = "eta", partial = FALSE), "partial")
})

test_that("apa_anova() formats lmerTest ANOVA tables", {
  skip_if_not_installed("lmerTest")
  fit <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  result <- apa_anova(anova(fit))
  expect_equal(
    colnames(result),
    c("Effect", "*df*~effect~", "*df*~error~", "*F*", "*p*")
  )
  expect_match(apa_note(result), "Satterthwaite approximations", fixed = TRUE)
})

test_that("apa_anova() rejects generalized linear mixed models", {
  skip_if_not_installed("lmerTest")
  fit <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = lme4::cbpp,
    family = binomial
  )
  expect_error(apa_anova(fit), "apa_parameters")
})
