# format model parameters in APA style

format model parameters in APA style

## Usage

``` r
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

- thresholds:

  for ordinal models, if `TRUE` (default), threshold (cutpoint) rows are
  displayed below the predictors.

- random:

  for mixed models, if `TRUE` (default), random-effect standard
  deviations and correlations (and the residual standard deviation, for
  linear mixed models) are displayed below the fixed effects.

- performance_parameters:

  for lists of models, model comparison metrics passed to
  [`apa_performance_comparison()`](https://wjschne.github.io/apa7/reference/apa_performance_comparison.md).
  If `NULL`, no metrics are added. By default, `R2` and `deltaR2`; for
  lists of mixed models (`lme4`), the default also includes the
  conditional *R*^2^ and its change (`R2_conditional`,
  `deltaR2_conditional`), and `R2` is the marginal *R*^2^.

## Value

tibble

## Details

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
tests and confidence intervals use Kenward–Roger degrees of freedom by
default (`ci_method = "kenward"`, which requires the pbkrtest package);
pass another `ci_method` (e.g., `"satterthwaite"`, which requires
lmerTest, or `"wald"`) to change this. When the degrees of freedom
differ across fixed effects, they are shown in a *df* column rather than
in the *t* column header. Random-effect rows show only the estimated
standard deviation (*SD*) or correlation (*r*), labeled by grouping
factor.

## Examples

``` r
lm(mpg ~ cyl + wt, data = mtcars) |>
   apa_parameters() |>
   apa_flextable()


.cl-d8bad469{}.cl-fd7caf76{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f148d26d{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-86359788{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-5b771c80{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-af91c7b5{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:21.3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-7b3643af{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:24.7pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-f64cd92c{width:1.52in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5142cfdb{width:1.073in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-454a5cee{width:0.87in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-01fd1ccc{width:0.893in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-d48697c5{width:1.52in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-30c3c808{width:1.073in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-800109ce{width:0.87in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c2dd6283{width:0.893in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c82b0ca6{width:1.073in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f4203d0c{width:1.52in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4aad9382{width:1.073in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7cd5ba8d{width:0.87in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-80d04b5b{width:0.893in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5e9d89c6{width:1.073in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Variable
```
