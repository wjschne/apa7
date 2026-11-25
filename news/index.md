# Changelog

## apa7 0.3.0

### Changes that can affect existing tables

These change the output of code that worked with apa7 0.2.0.

- `table_width` works the same way in every function that takes it
  ([`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md),
  [`apa_style()`](https://wjschne.github.io/apa7/reference/apa_style.md),
  [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md),
  [`apa_chisq()`](https://wjschne.github.io/apa7/reference/apa_chisq.md),
  and
  [`pretty_widths()`](https://wjschne.github.io/apa7/reference/pretty_widths.md)):
  a proportion of the page width from 0 to 1, the name of a width
  (`"page"`, `"landscape"`, or `"column"`), or a named number (e.g.,
  `c(landscape = .8)` or `c(cm = 10)`). Its default, `NULL`, is the new
  `table_width` setting of
  [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md)
  (1, the full width of the page, unless changed). This changes three
  functions:
  - [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md):
    `table_width` was a width in inches.
  - [`pretty_widths()`](https://wjschne.github.io/apa7/reference/pretty_widths.md):
    `table_width` was a width in `unit`, with `NULL` for the page width.
  - [`apa_style()`](https://wjschne.github.io/apa7/reference/apa_style.md):
    `table_width` defaulted to 0 (only as wide as the contents); use
    `table_width = 0` for that.

  A number greater than 1 is still read as inches (or in `unit` in
  [`pretty_widths()`](https://wjschne.github.io/apa7/reference/pretty_widths.md)),
  with a warning to write it with its unit (e.g., `c(inches = 5)`), and
  a table wider than its page fills the page. In
  [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md),
  the table’s width now matches `table_width`: before, the number
  column’s adjustment made the table wider than requested (e.g., 6.71
  rather than 6.5 inches), and very narrow tables could get a Variable
  column of negative width.
- The `accuracy` setting now changes the number of decimals in tables.
  Before, the default column formats kept the accuracy they had when
  apa7 was loaded, so `apa7_defaults(accuracy = .001)` did not change
  the decimals of most tables. Setting `column_formats` without
  `accuracy` in
  [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md)
  makes the formats’ own accuracy the `accuracy` setting.
- [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  introduces the fixed and random effects of mixed models (`lme4`,
  `glmmTMB`, and `nlme`) with “Fixed effects” and “Random effects”
  heading rows, as
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)
  does. Use `effect_headings = FALSE` to leave them out. The
  random-effects heading names the statistic of the *SD* rows (e.g.,
  “Random effects (*SD*)”), and each cluster (grouping factor) gets a
  subheading with its terms indented below it, so cluster names are not
  repeated on every row. Correlations name their own statistic (e.g.,
  “*r* (Constant, Days)”), and nested clusters are spelled out (e.g.,
  “Classroom in School”).
- [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md)
  passes its other arguments to
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
  rather than
  [`psych::corTest()`](https://rdrr.io/pkg/psych/man/corr.test.html).
  `adjust` is still accepted as `p_adjust`, and `use = "complete"`
  removes incomplete rows, each with a warning.
- [`apa_chisq()`](https://wjschne.github.io/apa7/reference/apa_chisq.md)
  has been rewritten to work like the other table functions. It returns
  a tibble (an `apa7_tbl`) that is displayed as an APA-style table in
  documents, and
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  writes its note instead of the footer it added itself.
  - The variables are chosen with `variable` (the rows) and `by` (the
    columns), unquoted or quoted. A data frame with exactly two columns
    still works without them: the first column gives the columns of the
    table, and the second gives its rows, as before.
  - `percent` shows percentages of each column (`"column"`, the default,
    as before), of each row (`"row"`), or of the whole sample
    (`"total"`), or counts only (`"none"`), with `percent_digits`
    decimals. `total = TRUE` (default) adds a “Total” row and column.
  - `test` chooses the chi-square test of independence (`"chisq"`,
    default), Fisher’s exact test (`"fisher"`), or no test (`"none"`). 2
    × 2 tables use Yates’s continuity correction unless
    `correct = FALSE`, and the note says so. When an expected count is
    below 5, the note says that the chi-square approximation may be
    inaccurate.
  - The effect size is *φ* for 2 × 2 tables and Cramér’s *V* otherwise
    (`effect_size`). Before, it was the adjusted Cramér’s *V* (labeled
    “Adj. Cramer’s *V*”) for every table.
  - The note gives the number of cases left out because of missing
    values, and `labels` names the variables.
  - The arguments that styled the flextable (e.g., `note`, `font_size`,
    and `table_width`) and `suppress_warnings` are deprecated. If any is
    given, a flextable is returned as before, with a warning. Pass the
    table to
    [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
    instead, with `note = TRUE` for the note.
- Table notes are now single-spaced, even when the table is
  double-spaced. This applies to `apa_flextable(note = TRUE)`,
  [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md),
  [`apa_chisq()`](https://wjschne.github.io/apa7/reference/apa_chisq.md),
  and footers styled by
  [`apa_style()`](https://wjschne.github.io/apa7/reference/apa_style.md).
  `line_spacing` now sets only the spacing of the header and body.

### New table functions

- New
  [`apa_descriptives()`](https://wjschne.github.io/apa7/reference/apa_descriptives.md)
  makes descriptive statistics tables (“Table 1”): *M* (*SD*) or *Mdn*
  (*IQR*) for continuous variables and *n* (%) for categorical
  variables, with a column for each group of `by` and for the whole
  sample, and optional tests of group differences.
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  says which statistics are shown and lists missing values.
- [`apa_descriptives()`](https://wjschne.github.io/apa7/reference/apa_descriptives.md)
  uses Fisher’s exact test instead of the chi-square test for a
  categorical variable whose table has an expected count below 5, and
  the note names those variables (`categorical_test = "chisq"` or
  `"fisher"` chooses one test for all). New `missing = "row"` shows the
  missing values of each variable in a “Missing” row instead of the
  note, and `range = TRUE` adds a “Range” row (minimum and maximum)
  below each continuous variable.
- New
  [`apa_reliability()`](https://wjschne.github.io/apa7/reference/apa_reliability.md)
  makes reliability and item analysis tables: for each item, *M*, *SD*,
  the corrected item–total correlation, and Cronbach’s α if deleted,
  followed for each scale by the scale mean and *SD*, α with a
  confidence interval, McDonald’s *ω*_(total), and the mean inter-item
  correlation. It handles several scales, reverse-keyed items, and items
  scored 0 or 1 (difficulty and KR-20), and `level = "scales"` gives one
  row per scale.
- [`apa_descriptives()`](https://wjschne.github.io/apa7/reference/apa_descriptives.md)
  selects `variables`, `by`, and `categorical`, and
  [`apa_reliability()`](https://wjschne.github.io/apa7/reference/apa_reliability.md)
  selects the items in `scales` and `reverse`, with unquoted or quoted
  names or tidyselect helpers (e.g.,
  `variables = c(age, starts_with("item")), by = group` or
  `scales = list(Anxiety = anx1:anx5), reverse = c(anx2, anx4)`).
- New
  [`apa_means()`](https://wjschne.github.io/apa7/reference/apa_means.md),
  [`apa_contrasts()`](https://wjschne.github.io/apa7/reference/apa_means.md),
  and
  [`apa_slopes()`](https://wjschne.github.io/apa7/reference/apa_means.md)
  make tables of estimated marginal means, contrasts (e.g., pairwise
  comparisons after an ANOVA), and simple slopes (e.g., to probe a
  moderation effect) from
  [`modelbased::estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.html),
  [`estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.html),
  and
  [`estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.html).
  They take a model (with the modelbased arguments, such as `by`,
  `contrast`, `trend`, and `p_adjust`) or the modelbased output. Other
  variables in `by` get heading rows, probabilities from logistic models
  have no leading zero, and
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  says what the values are, how the other predictors were held constant,
  on which scale the estimates are, which level was subtracted from
  which, and how *p*-values were adjusted for multiple comparisons.
  Moderator values set with a modelbased shortcut are named (e.g.,
  `by = "x=[sd]"` gives “−1*SD* (12.23)”, “*M* (14.23)”, and “+1*SD*
  (16.23)”, and `"x=[fivenum]"` gives the minimum, quartiles, median,
  and maximum), contrasts between such values (`contrast = "x=[sd]"`)
  are labeled by name (e.g., “+1*SD* − *M*”), and a numeric variable
  named in `by` without values (`by = "x"`) is set to its mean and 1
  *SD* above and below it rather than to the 10 values that modelbased
  uses by default. `by`, `contrast` (in
  [`apa_contrasts()`](https://wjschne.github.io/apa7/reference/apa_means.md)),
  and `trend` (in
  [`apa_slopes()`](https://wjschne.github.io/apa7/reference/apa_means.md))
  are named arguments, and their variable names can be quoted or
  unquoted (e.g., `apa_contrasts(fit, contrast = cyl, by = am)`). Models
  with variable names that are not syntactic (e.g.,
  `` `Partner Status` ``), for which marginaleffects gives standard
  errors of 0 or `NA`, are refit with syntactic names, and the tables
  keep the original names.
- [`apa_contrasts()`](https://wjschne.github.io/apa7/reference/apa_means.md)
  adds Cohen’s *d* with its confidence interval for models of continuous
  outcomes with an identity link (e.g.,
  [`lm()`](https://rdrr.io/r/stats/lm.html) and
  [`aov()`](https://rdrr.io/r/stats/aov.html)): each difference divided
  by the model’s residual standard deviation, with an interval that
  accounts for the uncertainty in that standard deviation (as in
  `emmeans::eff_size()`). For mixed models, give the standard deviation
  with `sd`. `effect_size = FALSE` leaves *d* out, and
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  says how it was computed.
- The note of
  [`apa_slopes()`](https://wjschne.github.io/apa7/reference/apa_means.md)
  gives the Johnson–Neyman intervals (Johnson & Neyman, 1936) when the
  slope depends on a single continuous moderator through a two-way
  interaction: the values of the moderator, within its observed range,
  at which the slope is significant. The boundaries are computed exactly
  from the coefficients and their covariance matrix, rather than from a
  grid of moderator values. `johnson_neyman = FALSE` leaves them out.
- New
  [`apa_collinearity()`](https://wjschne.github.io/apa7/reference/apa_collinearity.md)
  makes tables of collinearity diagnostics from
  [`performance::check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.html):
  the variance inflation factor (*VIF*) of each predictor with its
  confidence interval, and tolerance (or the standard-error inflation
  factor and the tolerance interval). Terms with more than 1 *df* get
  generalized *VIF*s, models with several parts (e.g., zero-inflated
  models) get a heading for each part, and `flag = 10` (or 5) bolds
  large *VIF*s.
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  names the terms with generalized *VIF*s, explains the *VIF*s of
  interaction terms, and defines tolerance.
- New
  [`apa_htest()`](https://wjschne.github.io/apa7/reference/apa_htest.md)
  runs *t* tests (Welch’s or Student’s, two-group or one-sample),
  Wilcoxon rank-sum and signed-rank tests, correlation tests (Pearson,
  Spearman, or Kendall), and chi-square tests of independence on the
  variables in a data frame, and makes a table with one row per outcome
  (e.g., `apa_htest(d, mpg + hp ~ am)`). It shows group means and
  standard deviations (or medians) under group spanners, the test
  statistic, *df*, *p*, and an effect size with a two-sided confidence
  interval: Cohen’s *d* or Hedges’ *g*, the rank-biserial correlation,
  or Cramér’s *V*, *φ*, or Cohen’s *w*. Effect sizes are computed from
  the data, so they do not depend on where the function is called, and
  Cohen’s *d* for Welch’s test is exact rather than approximated from
  *t*.
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  says which tests were done, how *d* was standardized, and what the
  abbreviations stand for.
- [`apa_htest()`](https://wjschne.github.io/apa7/reference/apa_htest.md)
  runs paired *t* tests and Wilcoxon signed-rank tests of paired
  variables with [`Pair()`](https://rdrr.io/r/stats/Pair.html) in the
  formula (e.g., `apa_htest(d, Pair(pre, post) ~ 1)`, or several pairs
  joined with `+`). Paired *t* tests show *d*_(av) by default, which is
  comparable to *d* for independent groups; *d*_(z), *d*_(rm), and
  Hedges’ *g* versions come from
  [`effectsize::repeated_measures_d()`](https://easystats.github.io/effectsize/reference/repeated_measures_d.html).
  New `pair_labels` argument sets the headings above each pair’s
  descriptive statistics.
- [`apa_htest()`](https://wjschne.github.io/apa7/reference/apa_htest.md)
  accepts test results already computed: an `htest` object from
  [`t.test()`](https://rdrr.io/r/stats/t.test.html),
  [`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html),
  [`cor.test()`](https://rdrr.io/r/stats/cor.test.html), or
  [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html), or a list
  of them. Effect sizes are computed from what the objects keep (exactly
  for one-sample and paired *t* tests and for chi-square tests of
  independence, and approximately for two-sample *t* tests), and
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  says how.
- New
  [`apa_subgroups()`](https://wjschne.github.io/apa7/reference/apa_subgroups.md)
  makes a table of a subgroup analysis of a meta-analysis fit with
  [`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html):
  each subgroup’s *k*, pooled estimate, confidence interval, *p*, and
  *I*², followed by the overall estimate. By default, each subgroup has
  its own *τ*² and the subgroups are compared with *Q*_(between); with
  `tau2 = "common"`, the estimates and the test (*Q*_(M), or *F* with
  the Knapp–Hartung adjustment) come from a meta-regression with the
  subgroup as a moderator.
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  describes the model and gives the test of subgroup differences.
- New
  [`apa_publication_bias()`](https://wjschne.github.io/apa7/reference/apa_publication_bias.md)
  makes a table of tests and corrections for publication bias in
  meta-analyses fit with
  [`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html):
  Egger’s regression test with its limit estimate, Begg and Mazumdar’s
  rank correlation test, trim and fill with the number of imputed
  studies and the adjusted estimate, and (optionally) the fail-safe *N*.
  Adjusted estimates are on the same scale as in
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md),
  and
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  gives the unadjusted pooled estimate and describes each method.

### New models

- [`apa_anova()`](https://wjschne.github.io/apa7/reference/apa_anova.md)
  makes *F* tables of the fixed effects of linear mixed models from
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) or
  [`lmerTest::lmer()`](https://rdrr.io/pkg/lmerTest/man/lmer.html), and
  formats ANOVA tables from lmerTest’s
  [`anova()`](https://rdrr.io/r/stats/anova.html). Tests come from
  lmerTest, with Kenward-Roger (default, as in
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md))
  or Satterthwaite (`ddf = "Satterthwaite"`) denominator degrees of
  freedom, and are Type III by default. The table shows *df*_(effect),
  *df*_(error), *F*, and *p*. Partial effect sizes, computed from *F*
  and its degrees of freedom, are shown only when requested (e.g.,
  `effect_size = "eta"`).
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  gives the type of tests, the degrees-of-freedom method, and how the
  effect size was computed.
- [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  and
  [`apa_performance()`](https://wjschne.github.io/apa7/reference/apa_performance.md)
  support mixed models fit with
  [`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html).
  Fixed effects are tested with *z* tests, coefficients of log and logit
  links are exponentiated by default (e.g., *IRR* for negative binomial
  models), and random effects are shown as for `lme4` models.
  Zero-inflation and dispersion parts appear under their own headings
  (“Zero-inflation model”, “Dispersion model”), and
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  gives their scale. glmmTMB models also work in lists of models and in
  [`apa_performance_comparison()`](https://wjschne.github.io/apa7/reference/apa_performance_comparison.md).
- [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  and
  [`apa_performance()`](https://wjschne.github.io/apa7/reference/apa_performance.md)
  support linear mixed models fit with
  [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html). Fixed effects
  are tested with *t* tests using nlme’s degrees of freedom, random
  effects are shown as for `lme4` models, and parameters of residual
  correlation structures (e.g., `corAR1()`) and variance functions
  (e.g., `varIdent()`) are shown below the residual *SD*.
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  names the structures and says how the degrees of freedom were
  computed. `lme` models also work in lists of models and in
  [`apa_performance_comparison()`](https://wjschne.github.io/apa7/reference/apa_performance_comparison.md).
- [`apa_performance()`](https://wjschne.github.io/apa7/reference/apa_performance.md)
  makes fit-index tables for `lavaan` models (e.g., from
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) and
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html)): the model
  *χ*² test, CFI, TLI, RMSEA with its 90% confidence interval, and SRMR
  (or any of these plus AIC and BIC). Models fit with robust estimators
  (e.g., MLR or WLSMV) show the scaled *χ*² test and robust (or scaled)
  indices.
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  gives the sample size, the estimator, and how missing data were
  handled, and defines the abbreviations. Lists of lavaan models give
  one row per model.
- [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  makes parameter tables for `lavaan` models. Fully standardized
  estimates (*β*) with their standard errors and *p*-values are shown by
  default (`standardize = FALSE` for unstandardized estimates), under
  headings for factor loadings (e.g., Visual → x1), regressions (e.g.,
  Speed ← Visual), correlations (e.g., Visual ↔︎ Textual), and defined
  parameters such as indirect effects. Variances, intercepts, and
  thresholds can be added. Parameters fixed to set the scale are not
  tested, and
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  names them, along with the estimator, the standardization, and robust
  or bootstrap standard errors.
- [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  supports multiple-group `lavaan` models. The groups are shown side by
  side under column spanners (or stacked under group headings with
  `groups = "rows"`), and
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  gives each group’s size and names the parameters constrained to be
  equal across groups.
- [`apa_loadings()`](https://wjschne.github.io/apa7/reference/apa_loadings.md)
  and
  [`apa_factor_cor()`](https://wjschne.github.io/apa7/reference/apa_factor_cor.md)
  accept `lavaan` models. The loading matrix shows the fully
  standardized loadings, sorted as for exploratory analyses, with
  loadings fixed at 0 left blank (cross-loadings are shown), and the
  factor correlation table shows the standardized latent correlations.
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  gives the sample size and the estimator.
- [`apa_performance_comparison()`](https://wjschne.github.io/apa7/reference/apa_performance_comparison.md)
  compares nested `lavaan` models (e.g., the configural, metric, and
  scalar models of a measurement invariance analysis), each with the one
  before it. Each row shows the model’s fit and, under a “Change from
  previous model” spanner, the *χ*² difference test from
  [`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html)
  (scaled for robust estimators) and the changes in CFI and RMSEA (or
  TLI and SRMR).
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  names the difference test.
- [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md)
  computes its correlations with
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
  (now imported) rather than
  [`psych::corTest()`](https://rdrr.io/pkg/psych/man/corr.test.html), so
  the table can show Spearman, Kendall, robust, partial, or multilevel
  correlations with the new `method`, `p_adjust`, and `partial`
  arguments, and other arguments are passed to
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html).
  The defaults (Pearson correlations, pairwise deletion, and unadjusted
  *p*-values) give the same tables as before. Variable names with spaces
  work for every kind of correlation, because
  [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md)
  uses internal names while computing them.
  [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md)
  also accepts the output of
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html).
  The note says what kind of correlations are shown and how the
  *p*-values were adjusted for multiple comparisons.
- [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  and
  [`apa_performance()`](https://wjschne.github.io/apa7/reference/apa_performance.md)
  support multinomial logistic regression models fit with
  [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html). Each
  outcome category gets a heading row naming its comparison with the
  reference category (e.g., “Part time vs. Not working”), with its
  coefficients indented below it (or `category_headings = FALSE` for
  labels such as “Part time: Constant”). Exponentiated coefficients are
  odds ratios relative to the reference category, labeled *OR*, and
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  names the reference category.
  [`apa_performance()`](https://wjschne.github.io/apa7/reference/apa_performance.md)
  reports McFadden’s pseudo-*R*², and lists of multinomial models are
  compared with likelihood ratio tests.
- [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  and
  [`apa_performance()`](https://wjschne.github.io/apa7/reference/apa_performance.md)
  support zero-inflated and hurdle count models fit with
  [`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) and
  [`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html). The
  count model and the zero model appear under their own headings (“Count
  model”, and “Zero-inflation model” or “Zero hurdle model”), and only
  the count model’s coefficients are exponentiated (*IRR*).
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  names the count distribution, gives the negative binomial dispersion
  parameter, and says what the zero model’s coefficients mean. Lists of
  these models are compared with likelihood ratio tests.
- [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  and
  [`apa_performance()`](https://wjschne.github.io/apa7/reference/apa_performance.md)
  support meta-analyses fit with
  [`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html).
  For models without moderators,
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  shows each study’s effect size, confidence interval, and percentage of
  the total weight, followed by the pooled estimate under a rule. The
  effect-size column is named by the measure (e.g., *g*, *r*, or *RR*);
  ratios are exponentiated, and Fisher’s *z* values are converted back
  to correlations (`transform = FALSE` keeps the analysis scale).
  Meta-regressions get a coefficient table, with exponentiated
  coefficients for ratio measures.
  [`apa_performance()`](https://wjschne.github.io/apa7/reference/apa_performance.md)
  gives *k*, *Q*, *τ*², *I*² (and *τ*, *H*², and confidence intervals),
  and, for meta-regressions, the test of the moderators and *R*².
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  names the measure and the model (including the estimator of *τ*² and
  the Knapp–Hartung adjustment) and gives the test of the pooled
  estimate, its prediction interval, and the test of heterogeneity.
  Multilevel meta-analyses fit with
  [`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html)
  (random intercepts at one or more nested levels, e.g., effect sizes in
  studies) get the same tables, with each level’s variance component
  (*σ*²) and the multilevel *I*² (Cheung, 2014). A new article describes
  the tables.
- [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  and
  [`apa_performance()`](https://wjschne.github.io/apa7/reference/apa_performance.md)
  support generalized additive models fit with
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) and
  [`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html). The table has
  the parametric terms (coefficients, standard errors, and *t* or *z*
  tests) and the smooth terms (effective degrees of freedom and
  approximate *F* or *χ*² tests) under their own headings
  (`terms = "parametric"` or `"smooth"` shows one kind). Smooth terms
  are labeled by type and variables (e.g., “s(Age)”, “te(Longitude,
  Latitude)”), with a row for each level of a factor-`by` variable, and
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  names the method that estimated the smoothing parameters.
  [`apa_performance()`](https://wjschne.github.io/apa7/reference/apa_performance.md)
  gives the adjusted *R*² and the deviance explained. Lists of GAMs are
  compared by *R*² and *AIC*, without nested tests, which are not well
  defined for penalized smooths.

### Table notes and citations

- Articles cited in table notes (e.g., for trim and fill or the
  multilevel *I*²) can be resolved by citeproc and added to the
  reference list. apa7 comes with a BibTeX file of these references, and
  [`apa7_write_bib()`](https://wjschne.github.io/apa7/reference/apa7_write_bib.md)
  copies it to a document’s folder. With the new `citations` argument of
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  (or `options(apa7.citations = )`), citations are written as Pandoc
  citation keys (`"keys"`, e.g., `[@duvalTrimFillSimple2000]`) or as
  text (`"text"`, e.g., “(Duval & Tweedie, 2000)”). The default,
  `"auto"`, writes a key when a Quarto or R Markdown document is being
  rendered and its bibliography has that entry, and text otherwise, so
  listing the file in the document’s `bibliography` is enough. A
  bibliography set for a whole Quarto project or folder (in
  `_quarto.yml` or `_metadata.yml`) is found, too. References cited in
  notes inside flextables (from `apa_flextable(note = TRUE)`,
  [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md),
  and
  [`apa_chisq()`](https://wjschne.github.io/apa7/reference/apa_chisq.md))
  are added to the document’s reference list: ftExtra formats such a
  note on its own, so apa7 adds the keys to the document’s `nocite`
  field when the table is printed. The keys are those of Zotero’s Better
  BibTeX extension. Notes cite the methods they name: Kenward–Roger and
  Satterthwaite degrees of freedom, Greenhouse–Geisser and Huynh–Feldt
  corrections, generalized *η*², Feldt’s confidence interval for α and
  McDonald’s *ω*_(total), the generalized *VIF*, paired-samples *d*,
  Holm and Benjamini–Hochberg adjustments, the DerSimonian–Laird
  estimator and Knapp–Hartung adjustment, the multilevel *I*²,
  publication-bias methods, the MLM and MLR estimators and scaled *χ*²
  difference tests of `lavaan`, and pseudo-*R*² (Tjur, Nagelkerke,
  McFadden, and the marginal *R*² of mixed models), which notes of lists
  of models now define.

### Settings

- [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md)
  sets all of apa7’s settings in one place: new `print` and `citations`
  arguments set the `apa7.print` and `apa7.citations` options. Its other
  simple settings (`accuracy`, `font_family`, `intercept_text`,
  `width_aware_padding`, and `page_width`) are now R options, too
  (`apa7.accuracy`, `apa7.font_family`, `apa7.intercept_text`,
  `apa7.width_aware_padding`, and `apa7.page_width`), so they can be set
  with [`options()`](https://rdrr.io/r/base/options.html), including in
  an `.Rprofile` file, before apa7 is loaded.
  [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md)
  checks its values before changing anything, and it returns the
  previous values of the settings it changed, which
  `do.call(apa7_defaults, old)` restores (before, it returned the
  settings’ environment itself, which changed along with the settings).
  `apa7_defaults(reset = TRUE)` now restores the same *p*-value
  formatter that is used when apa7 is loaded.
- New `page_width` setting in
  [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md)
  sets the width of the text area that tables fill: a number in inches,
  or the name of a paper size, whose width less APA’s 1-inch margins is
  used (e.g., `apa7_defaults(page_width = "a4")` for 6.27 inches). The
  paper sizes are those of Typst’s `page` function (e.g., `"a4"`,
  `"a5"`, `"us-letter"`, `"us-legal"`).
  [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md),
  [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md),
  and
  [`pretty_widths()`](https://wjschne.github.io/apa7/reference/pretty_widths.md)
  use it in place of the fixed 6.5 inches (the width of letter-size
  paper with 1-inch margins, which is still the default).
- Tables can be made for landscape pages and for two-column documents
  without changing the page width. `table_width = "landscape"` fills the
  text area of a landscape page, `table_width = "column"` fills one
  column of a two-column page, and `table_width = "page"` (or
  `"portrait"`) fills a portrait page, in every table function. A named
  number gives a proportion of one of these widths (e.g.,
  `c(landscape = .8)`) or a width in `inches`, `cm`, or `mm` (e.g.,
  `c(cm = 10)`).
  - New `landscape_width` and `column_width` settings in
    [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md)
    give these widths in inches. By default, they follow `page_width`:
    the paper’s height less 1-inch margins (9 inches for letter-size
    paper), and the width of a column in apaquarto’s two-column journal
    mode (`documentmode: jou`), which has 0.75-inch margins and a gutter
    of 4% of the text width (3.36 inches for letter-size paper).
  - New `table_width` setting in
    [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md)
    sets the default width of tables (e.g.,
    `apa7_defaults(table_width = "column")` in a two-column document).
    Proportions given to a table function are then proportions of that
    width.
  - New `apa7-width` chunk option (`apa7.width` in R Markdown) sets the
    width of the tables made in one code chunk, including tables printed
    automatically, without changing any other table (e.g.,
    `#| apa7-width: landscape`, or `#| apa7-width: {landscape: .8}`). A
    `table_width` given to a function overrides it. A chunk with
    apaquarto’s `apa-twocolumn: true`, which spans a table across both
    columns of a two-column page, makes the table as wide as the page.
  - When `page_width` and `table_width` are not set with
    [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md),
    the document being rendered supplies them: its `papersize` (e.g.,
    `papersize: a4`) sets the paper, and apaquarto’s `documentmode` sets
    the margins (0.75 inches on each side in journal mode and 1.5 inches
    on the left in thesis mode). In journal mode (`documentmode: jou`),
    which sets the text in two columns, tables are as wide as a column
    unless a chunk or function asks for another width, and a table with
    `apa-twocolumn: true` spans both columns.
  - The Getting Started article has a new section on table widths.
- New `font_size` setting in
  [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md)
  (the `apa7.font_size` option) sets the font size of all tables and
  their notes (12 points by default).
  [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md),
  [`apa_style()`](https://wjschne.github.io/apa7/reference/apa_style.md),
  [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md),
  and
  [`apa_chisq()`](https://wjschne.github.io/apa7/reference/apa_chisq.md)
  use it unless `font_size` is given.
- New `line_spacing` setting in
  [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md)
  (the `apa7.line_spacing` option) sets the spacing of the headers and
  bodies of all tables (2, double spacing, by default; APA style also
  allows 1.5 or single spacing in tables).
  [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md),
  [`apa_style()`](https://wjschne.github.io/apa7/reference/apa_style.md),
  [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md),
  and
  [`apa_chisq()`](https://wjschne.github.io/apa7/reference/apa_chisq.md)
  use it unless `line_spacing` is given. Notes stay single-spaced.

### Other improvements and bug fixes

- New `performance_layout` argument for lists of models in
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md):
  `"rows"` puts the model comparison metrics (e.g., *R*² and Δ*R*²) in
  rows below each model’s parameters (and random effects), with their
  values in the first value column, as in many hierarchical regression
  tables. The default, `"columns"`, keeps them in columns as before.
- New `random_scale` argument for mixed models in
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md):
  `"sd"` (default) for standard deviations and correlations, or
  `"variance"` for variances and covariances.
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  defines the symbols.
- [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
  keeps the table width set by `table_width` when
  `pretty_widths = TRUE`. Before,
  [`pretty_widths()`](https://wjschne.github.io/apa7/reference/pretty_widths.md)
  reset it to the full width, so in HTML a table could be stretched
  wider than its columns.
- Decimal-aligned columns in
  [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
  tables are centered in HTML as well as in Word.
- Named character vectors can select columns in every argument that
  takes tidyselect selections (e.g., the items in `scales` and `reverse`
  of
  [`apa_reliability()`](https://wjschne.github.io/apa7/reference/apa_reliability.md),
  or `variables` of
  [`apa_descriptives()`](https://wjschne.github.io/apa7/reference/apa_descriptives.md)):
  the names are ignored. Before, tidyselect read them as new names and
  gave the error “Can’t rename variables in this context”, so a lookup
  vector such as `c(A1 = "Am indifferent to the feelings of others")`
  could not be used.
- [`apa_performance()`](https://wjschne.github.io/apa7/reference/apa_performance.md),
  [`apa_performance_comparison()`](https://wjschne.github.io/apa7/reference/apa_performance_comparison.md),
  and
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  with a list of models no longer pass on insight’s warning about
  variable names quoted in backticks (e.g., `` `SAT-V` ``), which read
  well in tables and give the same results as other names. Other
  warnings are passed on as before.
- [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md)
  centers every summary column (e.g., *SD*, which was off-center when
  its values had different numbers of digits).
- [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md)
  lines up correlations on the decimal point when there are no star
  columns (`star_significant = FALSE` or `bold_significant = TRUE`), and
  lines up the summary columns (e.g., *M*, Skewness, and Kurtosis) on
  the decimal point.
- [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
  no longer re-formats row labels that
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  (for `lme4` and `glmmTMB` models) and
  [`apa_htest()`](https://wjschne.github.io/apa7/reference/apa_htest.md)
  have already formatted. Before, random-effect labels such as “Subject:
  *r* (Constant, Days)” lost their spacing and commas in flextables.
- The documentation of `table_width` in
  [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md),
  [`apa_style()`](https://wjschne.github.io/apa7/reference/apa_style.md),
  and
  [`apa_chisq()`](https://wjschne.github.io/apa7/reference/apa_chisq.md)
  now says that it is a proportion of the container’s width (from 0 to
  1), not a width in pixels. A `table_width` that is not a valid width
  now gives a clear error instead of flextable’s “width is \> 1”.

## apa7 0.2.0 *2026-10-18*

CRAN release: 2026-11-21

- New
  [`apa_anova()`](https://wjschne.github.io/apa7/reference/apa_anova.md)
  makes APA-style ANOVA source tables for `aov` and `lm` models (Type 1
  sums of squares, or Types 2 and 3 via
  [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html)),
  repeated-measures `aov` models with `Error()` strata, `afex` models,
  and tables from [`stats::anova()`](https://rdrr.io/r/stats/anova.html)
  and [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html). Effect
  sizes (partial, non-partial, or generalized *η*², *ω*², or *ε*²) and
  their two-sided confidence intervals come from `effectsize`.

- New
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  writes the text of a table note. For
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  tables of regression models (including lists of models and `lme4`
  models), it says which robust standard errors were used, how
  mixed-model degrees of freedom were approximated, and that ordinal
  thresholds are not tested, and it defines the standardized
  coefficient, exponentiated coefficient (e.g., *OR*), confidence
  interval, and *R*² change columns. For
  [`apa_loadings()`](https://wjschne.github.io/apa7/reference/apa_loadings.md)
  and
  [`apa_factor_cor()`](https://wjschne.github.io/apa7/reference/apa_factor_cor.md)
  tables, it gives the extraction method and rotation, says that oblique
  loadings are pattern coefficients, and explains omitted and bolded
  loadings and the *h*², *u*², and Complexity columns. For
  [`apa_anova()`](https://wjschne.github.io/apa7/reference/apa_anova.md)
  tables, it gives the type of sums of squares, which effects have
  sphericity-corrected degrees of freedom (`afex` models), what the
  effect size and *MSE* stand for, and what stars or bold values mean.
  With apaquarto, use it as `#| apa-note: !expr apa7::apa_note(tbl)`.
  Without apaquarto, `apa_flextable(note = TRUE)` places the note below
  the table.

- [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  now returns the ANOVA table for `aov`, repeated-measures `aov`, and
  `afex` models. Before, `aov` models were treated as `lm` models and
  gave a coefficients table.

- [`apa_loadings()`](https://wjschne.github.io/apa7/reference/apa_loadings.md)
  has been reworked:

  - It now returns an `apa7_tbl`, so it renders as a flextable in Quarto
    and R Markdown. It supports
    [`psych::principal()`](https://rdrr.io/pkg/psych/man/principal.html)
    as well as [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html),
    and output from
    [`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html)
    and
    [`parameters::principal_components()`](https://easystats.github.io/parameters/reference/principal_components.html)
    (including unrotated components from
    [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html)).
  - New `bold_max` argument bolds the largest loading in each row.
  - New `factor_names` argument relabels the factors.
  - New `variance` argument adds rows for the sum of squared loadings
    and the proportion and cumulative proportion of variance explained,
    with a rule above them.
  - New `communality`, `digits`, and `format_variable_names` arguments.
    [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
    no longer reformats item text in the `Variable` column.
  - Small loadings are now blank strings rather than `NA`.
  - The unused `column_formats` argument has been removed.

- New
  [`apa_factor_cor()`](https://wjschne.github.io/apa7/reference/apa_factor_cor.md)
  makes a table of factor correlations from obliquely rotated
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html),
  [`psych::principal()`](https://rdrr.io/pkg/psych/man/principal.html),
  and
  [`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html)
  solutions.

- [`column_format()`](https://wjschne.github.io/apa7/reference/column_format.md)
  gains an `abbreviation` slot that says what a header stands for (e.g.,
  `"standardized coefficient"` for β).
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  defines every abbreviated header in a table from these slots, for any
  table type, so setting the slot in `column_formats` adds or changes a
  definition. The default column formats define common abbreviations
  (e.g., CI, *AIC*, *BIC*, RMSEA, effect sizes, and *R*² variants), and
  [`column_formats()`](https://wjschne.github.io/apa7/reference/column_formats.md)
  gains a `get_abbreviations` getter. New column formats `h2`, `u2`, and
  `Complexity` set the headers of those
  [`apa_loadings()`](https://wjschne.github.io/apa7/reference/apa_loadings.md)
  columns.

- [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
  gains a `note` argument: `TRUE` adds the note from
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  below the table, and a character string adds a custom note. The note
  does not affect column widths.

- [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
  leaves alone any columns named in a `"no_format_columns"` attribute of
  the data, and draws a rule above the row named in a `"rule_above"`
  attribute.

- Partial *η*² columns (`Eta2_partial`) are now headed *η*²_(p). New
  column formats: `Sum_Squares`, `Mean_Square`, `Eta2_generalized`,
  `Omega2`, `Omega2_partial`, `Epsilon2`, and `Epsilon2_partial`.

- [`make_apaquarto()`](https://wjschne.github.io/apa7/reference/make_apaquarto.md)
  and
  [`install_apaquarto()`](https://wjschne.github.io/apa7/reference/install_apaquarto.md)
  now list missing packages separately and ask a short “Install them
  now?” question, so the prompt is readable in the console (e.g., R on
  macOS). New `ask` argument installs missing packages without asking
  when `FALSE`. If installation is declined, the function stops and
  prints the
  [`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
  command to run.

## apa7 0.1.3 *2026-05-25*

CRAN release: 2026-05-26

- Used AI (Claude) and old-fashioned effort to generate a more complete
  set of unit tests, fix minor bugs, and make documentation more
  consistent.
- User prompted to install any missing packages when running the
  `make_apaquarto` shiny app locally.

## apa7 0.1.1 *2025-09-07*

- Fix `align_chr` so that setting `format_numeric_character = FALSE`
  works.
- `list_column` gains a `unite` parameter that joins list and text
  columns.
- `add_star_column` no longer add stars if the column value is missing.

## apa7 0.1.0 *2025-08-19*

CRAN release: 2025-09-10

- Initial Cran Release

## apa7 0.0.0.9002

- removed apaquarto installation functions

## apa7 0.0.0.9001

- Added S7 objects for automatic column formats

## apa7 0.0.0.9000

- Initial development version
