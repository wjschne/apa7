# format model parameters in APA style

format model parameters in APA style

## Usage

``` r
# S3 method for class 'aov'
apa_parameters(
  fit,
  predictor_parameters = NULL,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  ...
)

# S3 method for class 'aovlist'
apa_parameters(
  fit,
  predictor_parameters = NULL,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  ...
)

# S3 method for class 'afex_aov'
apa_parameters(
  fit,
  predictor_parameters = NULL,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  ...
)

# S3 method for class 'gam'
apa_parameters(
  fit,
  predictor_parameters = c("Parameter", "Coefficient", "SE", "Exp_Coefficient", "t", "z",
    "df_error", "edf", "F", "Chi2", "p"),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  exponentiate = NULL,
  terms = c("all", "parametric", "smooth"),
  ...
)

# S3 method for class 'bam'
apa_parameters(
  fit,
  predictor_parameters = c("Parameter", "Coefficient", "SE", "Exp_Coefficient", "t", "z",
    "df_error", "edf", "F", "Chi2", "p"),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  exponentiate = NULL,
  terms = c("all", "parametric", "smooth"),
  ...
)

# S3 method for class 'lavaan'
apa_parameters(
  fit,
  predictor_parameters = NULL,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  standardize = TRUE,
  variances = FALSE,
  intercepts = FALSE,
  thresholds = FALSE,
  ci = 0.95,
  groups = c("columns", "rows"),
  ...
)

# S3 method for class 'rma'
apa_parameters(
  fit,
  predictor_parameters = c("Parameter", "Coefficient", "SE", "CI", "Exp_Coefficient",
    "z", "t", "df_error", "p"),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  studies = NULL,
  weights = TRUE,
  transform = TRUE,
  ...
)

# S3 method for class 'multinom'
apa_parameters(
  fit,
  predictor_parameters = c("Parameter", "Coefficient", "SE", "Exp_Coefficient", "z", "p"),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  exponentiate = TRUE,
  category_headings = TRUE,
  ...
)

# S3 method for class 'zeroinfl'
apa_parameters(
  fit,
  predictor_parameters = c("Parameter", "Coefficient", "SE", "Exp_Coefficient", "z", "p"),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  exponentiate = TRUE,
  ...
)

# S3 method for class 'hurdle'
apa_parameters(
  fit,
  predictor_parameters = c("Parameter", "Coefficient", "SE", "Exp_Coefficient", "z", "p"),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  exponentiate = TRUE,
  ...
)

apa_parameters(
  fit,
  predictor_parameters = c("Coefficient", "SE", "Std_Coefficient", "t", "df_error", "p"),
  starred = NULL,
  bolded = NULL,
  column_formats = NULL,
  t_with_df = TRUE,
  ...
)

# S3 method for class 'lm'
apa_parameters(
  fit,
  predictor_parameters = c("Parameter", "Coefficient", "SE", "Std_Coefficient", "t",
    "df_error", "p"),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  ...
)

# S3 method for class 'glm'
apa_parameters(
  fit,
  predictor_parameters = c("Parameter", "Coefficient", "SE", "Exp_Coefficient", "t", "z",
    "df_error", "p"),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  exponentiate = NULL,
  ...
)

# S3 method for class 'polr'
apa_parameters(
  fit,
  predictor_parameters = c("Parameter", "Coefficient", "SE", "Exp_Coefficient", "z", "p"),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  exponentiate = NULL,
  thresholds = TRUE,
  ...
)

# S3 method for class 'clm'
apa_parameters(
  fit,
  predictor_parameters = c("Parameter", "Coefficient", "SE", "Exp_Coefficient", "z", "p"),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  exponentiate = NULL,
  thresholds = TRUE,
  ...
)

# S3 method for class 'merMod'
apa_parameters(
  fit,
  predictor_parameters = c("Parameter", "Coefficient", "SE", "Exp_Coefficient", "t", "z",
    "df_error", "p"),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  exponentiate = NULL,
  random = TRUE,
  effect_headings = TRUE,
  random_scale = c("sd", "variance"),
  ...
)

# S3 method for class 'glmmTMB'
apa_parameters(
  fit,
  predictor_parameters = c("Parameter", "Coefficient", "SE", "Exp_Coefficient", "t", "z",
    "df_error", "p"),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  exponentiate = NULL,
  random = TRUE,
  effect_headings = TRUE,
  random_scale = c("sd", "variance"),
  ...
)

# S3 method for class 'lme'
apa_parameters(
  fit,
  predictor_parameters = c("Parameter", "Coefficient", "SE", "t", "df_error", "p"),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  random = TRUE,
  effect_headings = TRUE,
  random_scale = c("sd", "variance"),
  ...
)

# S3 method for class 'list'
apa_parameters(
  fit,
  predictor_parameters = NULL,
  starred = "deltaR2",
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  performance_parameters = c("R2", "deltaR2"),
  performance_layout = c("columns", "rows"),
  ...
)
```

## Arguments

- fit:

  model fit object

- predictor_parameters:

  predictor parameters to display. If named vector, column names will be
  vector names. Columns not present in the model output (e.g., `t` for a
  model tested with *z*) are silently dropped. For lists of models,
  `NULL` (default) uses the default for each model's class. Add `"CI"`
  to show confidence intervals as \[*LL*, *UL*\] (headed, e.g., "95%
  CI"), and, for `glm`, `polr`, and `clm` models, `"Exp_CI"` to show
  confidence intervals of exponentiated coefficients (headed, e.g., "95%
  CI for *OR*"). Confidence intervals are not shown by default.

- starred:

  columns to star with significant p_values

- bolded:

  columns to bold, if significant

- column_formats:

  column_formats object to format columns. If NULL, the default
  column_formats is used.

- t_with_df:

  if TRUE, the t column will be displayed with degrees of freedom in
  parentheses. If FALSE, only the t value is displayed.

- ...:

  arguments passed to methods. For `lm`, `glm`, `polr`, and `clm`
  models, further arguments are passed to
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).
  For example, `vcov = "HC3"` gives heteroskedasticity-consistent
  (robust) standard errors, and the test statistics, *p*-values, and
  confidence intervals are based on them. `ci = .90` sets the confidence
  level.

- exponentiate:

  if `NULL` (default), exponentiated coefficients are added when the
  model's link function is logit or log (i.e., odds ratios, risk ratios,
  or incidence rate ratios). If `TRUE`, exponentiated coefficients are
  always added. If `FALSE`, they are never added.

- terms:

  for generalized additive models
  ([`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html)), which terms to
  show: `"all"` (default) for the parametric terms and the smooth terms
  under their own headings, `"parametric"`, or `"smooth"`

- standardize:

  for `lavaan` models, if `TRUE` (default), fully standardized estimates
  (\\\beta\\) are displayed with their standard errors and *p*-values.
  If `FALSE`, unstandardized estimates (*B*) are displayed.

- variances, intercepts:

  for `lavaan` models, if `TRUE`, sections for (residual) variances or
  intercepts are added. Both are `FALSE` by default.

- thresholds:

  for ordinal models, if `TRUE` (default), threshold (cutpoint) rows are
  displayed below the predictors. For `lavaan` models, if `TRUE`, a
  section for the thresholds of ordinal indicators is added (`FALSE` by
  default).

- ci:

  for `lavaan` models, confidence level of intervals shown when
  `predictor_parameters` includes `"CI"`

- groups:

  for multiple-group `lavaan` models, `"columns"` (default) shows the
  groups side by side, each under a column spanner, and `"rows"` stacks
  them, each under a heading row.

- studies:

  for meta-analyses
  ([`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)),
  if `TRUE`, the table has a row for each study (its effect size,
  confidence interval, and weight) followed by the pooled estimate. If
  `FALSE`, the table has the model's coefficients (the pooled estimate,
  or the intercept and moderators of a meta-regression). If `NULL`
  (default), `TRUE` for models without moderators and `FALSE` for
  meta-regressions.

- weights:

  for the study rows of meta-analyses, if `TRUE` (default), each study's
  percentage of the total weight is shown

- transform:

  for meta-analyses, if `TRUE` (default), effect sizes computed on a
  transformed scale are shown on their original scale: log risk ratios,
  odds ratios, incidence rate ratios, and ratios of means are
  exponentiated, and Fisher's *z*-transformed correlations are converted
  back to correlations. In meta-regressions, exponentiated coefficients
  are added in a separate column. If `FALSE`, estimates stay on the
  scale on which they were analyzed.

- category_headings:

  for multinomial models, if `TRUE` (default), the coefficients of each
  outcome category are introduced by a heading row that names the
  comparison (e.g., "Part time vs. Not working"), with the predictors
  indented below it. If `FALSE`, each row is labeled with its category
  and predictor (e.g., "Part time: Children").

- random:

  for mixed models, if `TRUE` (default), random-effect standard
  deviations and correlations (and the residual standard deviation, for
  linear mixed models) are displayed below the fixed effects.

- effect_headings:

  for mixed models, if `TRUE` (default), the fixed and random effects
  are introduced by heading rows ("Fixed effects" and "Random effects"),
  with the effects indented below them. Headings are added only when
  random effects are displayed. The random-effects heading names the
  statistic of the *SD* (or variance) rows (e.g., "Random effects
  (*SD*)"), and each cluster (grouping factor) gets a subheading with
  its terms indented below it; correlation (or covariance) rows name
  their own statistic (e.g., "*r* (Constant, Days)"). Without headings,
  each random-effect label names its cluster and statistic (e.g.,
  "Subject: *SD* (Days)").

- random_scale:

  for mixed models, `"sd"` (default) displays random effects as standard
  deviations (*SD*) and correlations (*r*), and `"variance"` displays
  them as variances (*Var*) and covariances (*Cov*).

- performance_parameters:

  for lists of models, model comparison metrics passed to
  [`apa_performance_comparison()`](https://wjschne.github.io/apa7/reference/apa_performance_comparison.md).
  If `NULL`, no metrics are added. By default, `R2` and `deltaR2`; for
  lists of mixed models (`lme4`), the default also includes the
  conditional *R*^2^ and its change (`R2_conditional`,
  `deltaR2_conditional`), and `R2` is the marginal *R*^2^.

- performance_layout:

  for lists of models, where the model comparison metrics go:
  `"columns"` (default) adds a column for each metric, with its value on
  the model's heading row; `"rows"` adds a row for each metric below the
  model's parameters (and random effects), labeled in the variable
  column (e.g., *R*^2^ and Δ*R*^2^) with its value in the first value
  column, as in many hierarchical regression tables. Metrics without a
  value (e.g., the change in *R*^2^ of the first model) are left out.

## Value

tibble

## Details

For `aov` models (including repeated-measures models with an `Error()`
term) and `afex` models, `apa_parameters()` returns the ANOVA table from
[`apa_anova()`](https://wjschne.github.io/apa7/reference/apa_anova.md).
`predictor_parameters` and `t_with_df` are ignored, and other arguments
in `...` (e.g., `effect_size`, `type`) are passed to
[`apa_anova()`](https://wjschne.github.io/apa7/reference/apa_anova.md).
To display regression coefficients instead, refit the model with
[`stats::lm()`](https://rdrr.io/r/stats/lm.html).

For generalized additive models fit with
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) or
[`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html), the table has
the parametric terms (coefficients, standard errors, and their *t* or
*z* tests) and the smooth terms (effective degrees of freedom and their
approximate *F* or \\\chi^2\\ tests), as reported by
[`summary()`](https://rdrr.io/r/base/summary.html), under the headings
"Parametric terms" and "Smooth terms". Smooth terms are labeled by their
type and variables (e.g., "s(Age)" or "te(Longitude, Latitude)"), and
smooths of a factor-by variable are labeled by level (e.g., "s(Age):
Group = Treatment"). Coefficients of log and logit links are
exponentiated by default, as for
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) models.
[`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
names the method used to estimate the smoothing parameters.

For structural equation models and confirmatory factor analyses fit with
`lavaan`, estimates come from
[`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html)
(`standardize = TRUE`, default) or
[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
(`standardize = FALSE`). Each kind of parameter is introduced by a
heading row: "Factor loadings" (e.g., Visual \\\rightarrow\\ x1),
"Regressions" (e.g., Speed \\\leftarrow\\ Visual), "Correlations" (or
"Covariances", if unstandardized; e.g., Visual \\\leftrightarrow\\
Textual), and "Defined parameters" (e.g., indirect effects defined with
`:=`), followed, if requested, by variances, intercepts, and thresholds.
The default columns are the estimate, its standard error, and its
*p*-value; `"z"`, `"CI"`, and (with `standardize = TRUE`)
`"Coefficient"` for unstandardized estimates can be added to
`predictor_parameters`. Unstandardized parameters fixed to set the scale
of a factor (e.g., marker loadings fixed at 1) are not tested. Variable
names are shown as written in the model, with the first letter
capitalized and underscores replaced by spaces.

For multiple-group models (e.g., `group = "school"`), the groups are
shown side by side by default, with one row for each parameter and a
column spanner for each group, so that the groups can be compared row by
row. With `groups = "rows"`, each group has a heading row, with the
kinds of parameters indented below it. A defined parameter that uses the
labels of one group is shown with that group. One that combines groups
(e.g., the difference between two groups' paths) is shown in the first
group's columns, or under an "Across groups" heading.
[`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
gives the size of each group and names the parameters constrained to be
equal across groups (`group.equal`), and any exceptions
(`group.partial`). Standardized estimates of equal parameters can still
differ across groups, because standardization uses each group's
variances.

For meta-analyses fit with
[`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)
(random-effects or equal-effects models, without or with moderators),
the table of a model without moderators has a row for each study,
labeled by the study labels (`slab`), with its effect size, confidence
interval, and percentage of the total weight, followed by the pooled
estimate under a rule. The column of effect sizes is named by the
measure given to
[`metafor::escalc()`](https://wviechtb.github.io/metafor/reference/escalc.html)
(e.g., *g* for standardized mean differences, *r* for correlations, and
*OR* for odds ratios).
[`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
names the measure and the model (including the estimator of \\\tau^2\\),
and gives the test of the pooled estimate, its prediction interval, and
the test of heterogeneity. The table of a meta-regression has the
intercept and moderators, as for other regression models.
[`apa_performance()`](https://wjschne.github.io/apa7/reference/apa_performance.md)
gives the heterogeneity statistics.

Multilevel meta-analyses fit with
[`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html)
(e.g., several effect sizes nested in studies, with
`random = ~ 1 | study / effect`) get the same tables, with a row for
each effect size.
[`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
names the levels and their numbers of units, and gives each level's
variance component (\\\sigma^2\\) and the multilevel \\I^2\\ (Cheung,
2014). Random effects with an `~ inner | outer` structure (e.g.,
correlated outcomes within studies) are not yet supported.

For multinomial logistic regression models
([`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html)), each
outcome category except the reference category (the first level of the
response) has its own block of coefficients, introduced by a heading row
such as "Part time vs. Not working". Coefficients are log odds of the
category relative to the reference category, tested with Wald *z* tests,
and their exponentiated values (shown by default) are odds ratios
relative to the reference category (sometimes called relative risk
ratios), labeled *OR*. To compare the categories with a different
reference, relevel the response (e.g., with
[`stats::relevel()`](https://rdrr.io/r/stats/relevel.html)) before
fitting the model.
[`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
names the reference category.

For zero-inflated and hurdle count models fit with
[`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) and
[`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html), the count
model and the zero model are each introduced by a heading row ("Count
model" and "Zero-inflation model" or "Zero hurdle model"). Coefficients
are tested with Wald *z* tests. Only the count model's coefficients are
exponentiated (by default), giving incidence rate ratios (*IRR*),
because the zero model has a different link and meaning. In a
zero-inflated model, the zero model predicts excess zeros (zeros not
produced by the count model); in a hurdle model, it predicts whether the
count is greater than zero.
[`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
says which, names the count distribution, and gives the negative
binomial dispersion parameter (\\\theta\\).

For `glm` objects (including
[`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html)), the test
statistic is *z* for families with a fixed dispersion parameter
(binomial, Poisson, negative binomial) and *t* for families with an
estimated dispersion parameter (Gaussian, Gamma, inverse Gaussian, quasi
families). Whichever statistic applies is displayed. The exponentiated
coefficient column (`Exp_Coefficient`) is labeled *OR* for logit links,
*IRR* for log links in count models (Poisson, quasi-Poisson, negative
binomial), *RR* for log links in binomial models, and Exp(*B*)
otherwise.

For ordinal regression models
([`MASS::polr`](https://rdrr.io/pkg/MASS/man/polr.html) and
[`ordinal::clm`](https://rdrr.io/pkg/ordinal/man/clm.html)),
coefficients are tested with Wald *z* tests. (`parameters` reports *t*
for `polr`, but the test is asymptotically normal, and `clm` reports *z*
for the same model.) With a logit link, exponentiated coefficients are
cumulative odds ratios, labeled *OR*. Threshold rows show only the
estimate, its standard error, and its confidence interval, because their
exponentiated values are not odds ratios, and testing whether a
threshold equals 0 is not meaningful. Confidence intervals (`"CI"`,
`"Exp_CI"`) are Wald *z* intervals, consistent with the *z* tests.

For mixed models fit with `lme4` (`lmer` and `glmer`), fixed effects are
displayed as for `lm` and `glm` models. For linear mixed models, *t*
tests and confidence intervals use Kenward-Roger degrees of freedom by
default (`ci_method = "kenward"`, which requires the pbkrtest package);
pass another `ci_method` (e.g., `"satterthwaite"`, which requires
lmerTest, or `"wald"`) to change this. When the degrees of freedom
differ across fixed effects, they are shown in a *df* column rather than
in the *t* column header. Random-effect rows show only the estimated
standard deviation (*SD*) or correlation (*r*), or with
`random_scale = "variance"`, the variance (*Var*) or covariance (*Cov*),
grouped by cluster (grouping factor) under a subheading for each
cluster, with nested clusters named, e.g., "Classroom in School". The
fixed and random effects are introduced by heading rows
(`effect_headings = TRUE`). The random-effects heading names the
statistic of the *SD* or variance rows (e.g., "Random effects (*SD*)"),
and correlation or covariance rows name their own (e.g., "*r* (Constant,
Days)").
[`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
defines the symbols.

For mixed models fit with
[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html),
fixed effects are tested with *z* tests, and random effects are
displayed as for `lme4` models. Coefficients of log and logit links are
exponentiated by default (e.g., *IRR* for Poisson and negative binomial
models). When the model has a zero-inflation part (`ziformula`) or a
dispersion part (`dispformula`), each part is introduced by a heading
row ("Conditional model", "Zero-inflation model", or "Dispersion
model"), and only the conditional model's coefficients are
exponentiated. A dispersion parameter that is not modeled (i.e., without
a `dispformula`) is not displayed.

For linear mixed models fit with
[`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html), fixed effects
are tested with *t* tests whose degrees of freedom are computed by nlme
from the grouping levels at which each effect varies, and random effects
are displayed as for `lme4` models. Parameters of residual correlation
structures (`correlation`, e.g.,
[`nlme::corAR1()`](https://rdrr.io/pkg/nlme/man/corAR1.html)) and
variance functions (`weights`, e.g.,
[`nlme::varIdent()`](https://rdrr.io/pkg/nlme/man/varIdent.html)) are
displayed below the residual *SD*, and
[`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
names the structures.

## References

Cheung, M. W.-L. (2014). Modeling dependent effect sizes with
three-level meta-analyses: A structural equation modeling approach.
*Psychological Methods, 19*(2), 211–229.
[doi:10.1037/a0032968](https://doi.org/10.1037/a0032968)

## Examples

``` r
# \donttest{
if (requireNamespace("mgcv", quietly = TRUE)) {
  mgcv::gam(mpg ~ am + s(hp, k = 5) + s(wt, k = 5), data = mtcars,
            method = "REML") |>
    apa_parameters()
}
#> # A tibble: 6 × 7
#>   Variable         `*B*`   `*SE*` `*t*(25.43)` `*edf*` `*F*`  `*p*`  
#>   <chr>            <chr>   <chr>  <chr>        <chr>   <chr>  <chr>  
#> 1 Parametric terms ""      ""     ""           ""      ""     ""     
#> 2  Constant        "19.86" "0.69" "28.67"      ""      ""     "<.001"
#> 3  Am              " 0.57" "1.43" " 0.40"      ""      ""     " .70 "
#> 4 Smooth terms     ""      ""     ""           ""      ""     ""     
#> 5  s(Hp)           ""      ""     ""           "2.36"  "5.46" " .006"
#> 6  s(Wt)           ""      ""     ""           "2.21"  "5.83" " .005"
# }
# \donttest{
if (requireNamespace("lavaan", quietly = TRUE)) {
  model <- "visual =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed =~ x7 + x8 + x9
            speed ~ visual + textual"
  lavaan::sem(model, data = lavaan::HolzingerSwineford1939) |>
    apa_parameters()

  # Multiple groups, side by side
  lavaan::cfa(
    "visual =~ x1 + x2 + x3",
    data = lavaan::HolzingerSwineford1939,
    group = "school",
    group.equal = "loadings"
  ) |>
    apa_parameters()
}
#> # A tibble: 4 × 7
#>   Parameter   `Pasteur_&beta;` `Pasteur_*SE*` `Pasteur_*p*` `Grant-White_&beta;`
#>   <chr>       <chr>            <chr>          <chr>         <chr>               
#> 1 Factor loa… ""               ""             ""            ""                  
#> 2  Visual → … ".61"            "0.07"         "<.001"       ".60"               
#> 3  Visual → … ".48"            "0.07"         "<.001"       ".52"               
#> 4  Visual → … ".72"            "0.08"         "<.001"       ".77"               
#> # ℹ 2 more variables: `Grant-White_*SE*` <chr>, `Grant-White_*p*` <chr>
# }
# \donttest{
if (requireNamespace("metafor", quietly = TRUE) &&
    requireNamespace("metadat", quietly = TRUE)) {
  d_bcg <- metafor::escalc(
    measure = "RR",
    ai = tpos, bi = tneg, ci = cpos, di = cneg,
    data = metadat::dat.bcg,
    slab = paste(author, year)
  )
  fit_meta <- metafor::rma(yi, vi, data = d_bcg)
  apa_parameters(fit_meta)
  apa_performance(fit_meta)

  # Meta-regression
  metafor::rma(yi, vi, mods = ~ ablat, data = d_bcg) |>
    apa_parameters()
}
#> # A tibble: 2 × 7
#>   Variable `*B*` `*SE*` `95% CI`       `*RR*` `*z*` `*p*`
#>   <chr>    <chr> <chr>  <chr>          <chr>  <chr> <chr>
#> 1 Constant  0.25 0.25   [−0.24,  0.74] 1.29    1.01  .31 
#> 2 Ablat    −0.03 0.01   [−0.04, −0.01] 0.97   −4.04 <.001
# }
# \donttest{
if (requireNamespace("nnet", quietly = TRUE) &&
    requireNamespace("carData", quietly = TRUE)) {
  d <- carData::Womenlf
  d$partic <- relevel(d$partic, ref = "not.work")
  nnet::multinom(partic ~ hincome + children, data = d, trace = FALSE) |>
    apa_parameters()
}
#> # A tibble: 8 × 6
#>   Variable              `*B*`   `*SE*` `*OR*` `*z*`   `*p*`  
#>   <chr>                 <chr>   <chr>  <chr>  <chr>   <chr>  
#> 1 fulltime vs. not.work ""      ""     ""     ""      ""     
#> 2  Constant             " 1.98" "0.48" "7.26" " 4.10" "<.001"
#> 3  Hincome              "−0.10" "0.03" "0.91" "−3.46" "<.001"
#> 4  Children–Present     "−2.56" "0.36" "0.08" "−7.06" "<.001"
#> 5 parttime vs. not.work ""      ""     ""     ""      ""     
#> 6  Constant             "−1.43" "0.59" "0.24" "−2.42" " .02 "
#> 7  Hincome              " 0.01" "0.02" "1.01" " 0.29" " .77 "
#> 8  Children–Present     " 0.02" "0.47" "1.02" " 0.05" " .96 "
# }
# \donttest{
if (requireNamespace("pscl", quietly = TRUE)) {
  data("bioChemists", package = "pscl")
  pscl::zeroinfl(art ~ fem + kid5 + ment | ment,
                 data = bioChemists, dist = "negbin") |>
    apa_parameters()
}
#> # A tibble: 8 × 6
#>   Variable             `*B*`   `*SE*` `*IRR*` `*z*`   `*p*`  
#>   <chr>                <chr>   <chr>  <chr>   <chr>   <chr>  
#> 1 Count model          ""      ""     ""      ""      ""     
#> 2  Constant            " 0.49" "0.07" "1.64"  " 6.92" "<.001"
#> 3  Fem–Women           "−0.23" "0.07" "0.80"  "−3.18" " .002"
#> 4  Kid 5               "−0.13" "0.05" "0.88"  "−2.73" " .006"
#> 5  Ment                " 0.02" "0.00" "1.02"  " 7.04" "<.001"
#> 6 Zero-inflation model ""      ""     ""      ""      ""     
#> 7  Constant            "−0.79" "0.35" ""      "−2.27" " .02 "
#> 8  Ment                "−0.62" "0.25" ""      "−2.49" " .01 "
# }
lm(mpg ~ cyl + wt, data = mtcars) |>
   apa_parameters() |>
   apa_flextable()


.cl-d8bad469{}.cl-fd7caf76{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f148d26d{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-7a9068f9{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-8dbfbcff{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-e57ace8c{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:21.3pt;padding-right:18.3pt;line-height: 1.15;background-color:transparent;}.cl-d6fae38b{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:24.7pt;padding-right:21.7pt;line-height: 1.15;background-color:transparent;}.cl-f64cd92c{width:1.52in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5142cfdb{width:1.073in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-454a5cee{width:0.87in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-01fd1ccc{width:0.893in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-d48697c5{width:1.52in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-30c3c808{width:1.073in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-800109ce{width:0.87in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c2dd6283{width:0.893in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c82b0ca6{width:1.073in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f4203d0c{width:1.52in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4aad9382{width:1.073in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7cd5ba8d{width:0.87in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-80d04b5b{width:0.893in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5e9d89c6{width:1.073in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Variable
```
