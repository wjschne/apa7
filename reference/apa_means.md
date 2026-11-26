# Estimated marginal means, contrasts, and slopes tables

Makes APA-style tables of estimated marginal means, contrasts between
them (e.g., pairwise comparisons after an ANOVA), and simple slopes
(e.g., to probe a moderation effect), computed with the
[modelbased](https://easystats.github.io/modelbased/) package.

## Usage

``` r
apa_means(
  fit,
  by = NULL,
  ...,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  format_variable_names = TRUE
)

apa_contrasts(
  fit,
  contrast = NULL,
  by = NULL,
  ...,
  effect_size = NULL,
  sd = NULL,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  format_variable_names = TRUE
)

apa_slopes(
  fit,
  trend = NULL,
  by = NULL,
  ...,
  johnson_neyman = NULL,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  format_variable_names = TRUE
)
```

## Arguments

- fit:

  a model, or output of
  [`modelbased::estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.html)
  (for `apa_means()`),
  [`modelbased::estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.html)
  (for `apa_contrasts()`), or
  [`modelbased::estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.html)
  (for `apa_slopes()`)

- by:

  variables whose levels (or values) the estimates are computed at
  (e.g., `by = Species`, `by = c(am, cyl)`, `by = "hp=[sd]"`, or
  `by = list(hp = "[sd]")`). Variable names can be quoted or unquoted,
  but values (e.g., `"hp=[fivenum]"`) must be quoted. In `apa_means()`
  and `apa_slopes()`, the last variable labels the rows and the others
  get heading rows; in `apa_contrasts()`, all of them get heading rows.
  Passed to the modelbased function.

- ...:

  other arguments passed to the modelbased function when `fit` is a
  model (e.g., `p_adjust = "holm"`, `predict = "link"`, or
  `estimate = "average"`)

- starred:

  columns to which significance stars are added (e.g., `"p"` or
  `"Estimate"`)

- bolded:

  columns that are bolded when *p* \\\le\\ .05

- column_formats:

  [`column_formats()`](https://wjschne.github.io/apa7/reference/column_formats.md)
  object. If `NULL` (default), the defaults are used. The estimate
  column is named `"Estimate"`.

- format_variable_names:

  if `TRUE` (default), variable names are formatted (e.g.,
  `"Sepal.Length"` becomes "Sepal Length"). Factor levels are shown as
  they are.

- contrast:

  variable whose levels (or values) are compared (e.g.,
  `contrast = cyl`), quoted or unquoted. Passed to
  [`modelbased::estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.html).

- effect_size:

  in `apa_contrasts()`, whether to add Cohen's *d* and its confidence
  interval. If `NULL` (default), *d* is added when it can be computed
  (models of continuous outcomes with an identity link, such as
  [`lm()`](https://rdrr.io/r/stats/lm.html), or any model when `sd` is
  given). If `TRUE`, an error is given when it cannot be computed.

- sd:

  in `apa_contrasts()`, the standard deviation that differences are
  divided by to compute Cohen's *d*. If `NULL` (default), the model's
  residual standard deviation is used (not for mixed models).

- trend:

  predictor whose slopes are estimated (e.g., `trend = wt`), quoted or
  unquoted. Passed to
  [`modelbased::estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.html).

- johnson_neyman:

  in `apa_slopes()`, whether the note gives the Johnson-Neyman
  intervals. If `NULL` (default), they are given when they can be
  computed (see Details). If `TRUE`, an error is given when they cannot
  be computed.

## Value

`apa7_tbl` (tibble)

## Details

`fit` is either a model or the output of
[`modelbased::estimate_means()`](https://easystats.github.io/modelbased/reference/estimate_means.html),
[`modelbased::estimate_contrasts()`](https://easystats.github.io/modelbased/reference/estimate_contrasts.html),
or
[`modelbased::estimate_slopes()`](https://easystats.github.io/modelbased/reference/estimate_slopes.html).
With a model, `by`, `contrast`, `trend`, and the arguments in `...`
(e.g., `p_adjust`) are passed to the modelbased function, so
`apa_contrasts(fit, contrast = "group", p_adjust = "holm")` is the same
as
`apa_contrasts(modelbased::estimate_contrasts(fit, contrast = "group", p_adjust = "holm"))`.

Rows are labeled by the levels of the last focal variable (in
`apa_means()`), by the levels compared (in `apa_contrasts()`, e.g., "6
\\-\\ 4"), or by the levels of the moderator (in `apa_slopes()`). Any
other variables in `by` get heading rows, with the rows below them
indented. Numeric levels (e.g., values of a continuous moderator) are
rounded to 2 decimals.

Values set with a modelbased shortcut are named, with the value in
parentheses: `"x=[sd]"` (or `"x=[meansd]"`) gives "\\-\\1*SD*", "*M*",
and "+1*SD*"; `"x=[mad]"` gives "\\-\\1*MAD*", "*Mdn*", and "+1*MAD*";
`"x=[fivenum]"` gives "Min", "*Q*~1~", "*Mdn*", "*Q*~3~", and "Max";
`"x=[quartiles]"` gives "*Q*~1~", "*Mdn*", and "*Q*~3~"; and
`"x=[minmax]"` gives "Min" and "Max". Contrasts between such values
(e.g., `contrast = "x=[sd]"`) are labeled by name (e.g., "+1*SD* \\-\\
*M*"), and the note gives the values. With a model, a numeric variable
named in `by` without values (e.g., `by = "x"`) is set to its mean and 1
*SD* above and below it, as with `by = "x=[sd]"` (rather than the
modelbased default of 10 values, which suits plots better than tables).

[`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
says what the values are, how the other predictors were held constant,
on which scale the estimates are (for models with a link function, such
as logistic regression), which level was subtracted from which, and how
*p*-values were adjusted for multiple comparisons.

In `apa_contrasts()`, contrasts from models of continuous outcomes with
an identity link (e.g., [`lm()`](https://rdrr.io/r/stats/lm.html) and
[`aov()`](https://rdrr.io/r/stats/aov.html)) get Cohen's *d* with its
confidence interval: each difference divided by the model's residual
standard deviation (\\\hat{\sigma}\\), as in `emmeans::eff_size()`. The
interval accounts for the uncertainty in \\\hat{\sigma}\\. For mixed
models, there is no single right standard deviation to divide by, so *d*
is shown only when it is given with `sd` (e.g., the square root of the
sum of the variance components and the residual variance). *d* is not
computed for logistic and other generalized linear models.

In `apa_slopes()`, when the slope of `trend` depends on a single
continuous moderator in `by` through a two-way interaction (e.g.,
`lm(y ~ x * m)`), the note gives the Johnson-Neyman intervals (Johnson &
Neyman, 1936): the values of the moderator, within its observed range,
at which the slope is significant. The boundaries are computed exactly
from the coefficients and their covariance matrix (the values of the
moderator at which the slope's test statistic equals its critical
value), with *t* and the residual degrees of freedom for
[`lm()`](https://rdrr.io/r/stats/lm.html) models and the normal
distribution for other models (e.g., mixed models). They are not
computed for models with a link function (e.g., logistic regression), or
when the slope also depends on other variables (e.g., a three-way
interaction).

Bayesian models are not yet supported.

## References

Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery
rate: A practical and powerful approach to multiple testing. *Journal of
the Royal Statistical Society: Series B (Methodological), 57*(1),
289–300.
[doi:10.1111/j.2517-6161.1995.tb02031.x](https://doi.org/10.1111/j.2517-6161.1995.tb02031.x)

Holm, S. (1979). A simple sequentially rejective multiple test
procedure. *Scandinavian Journal of Statistics, 6*(2), 65–70.

Johnson, P. O., & Neyman, J. (1936). Tests of certain linear hypotheses
and their application to some educational problems. *Statistical
Research Memoirs, 1*, 57–93.

Kramer, C. Y. (1956). Extension of multiple range tests to group means
with unequal numbers of replications. *Biometrics, 12*(3), 307–310.
[doi:10.2307/3001469](https://doi.org/10.2307/3001469)

Tukey, J. W. (1994). The problem of multiple comparisons. In H. I. Braun
(Ed.), *The collected works of John W. Tukey: Vol. VIII. Multiple
comparisons, 1948–1983* (pp. 1–300). Chapman & Hall.

## Examples

``` r
if (requireNamespace("modelbased", quietly = TRUE) &&
    requireNamespace("marginaleffects", quietly = TRUE)) {
  fit <- lm(Sepal.Width ~ Species * Sepal.Length, data = iris)

  # Means of each species, with sepal length held at its mean
  apa_means(fit, by = "Species")

  # Pairwise comparisons, with Holm-adjusted p-values
  apa_contrasts(fit, contrast = "Species", p_adjust = "holm")

  # Simple slopes of sepal length in each species
  apa_slopes(fit, trend = "Sepal.Length", by = "Species")

  # The output of modelbased works, too
  modelbased::estimate_contrasts(fit, contrast = "Species") |>
    apa_contrasts() |>
    apa_note()
}
#> [1] "Values are differences between estimated marginal means of Sepal Width (the first level minus the second). Sepal Length is held at its mean (5.84). *d* = difference divided by the residual standard deviation of the model (0.27); its confidence interval accounts for the uncertainty in the residual standard deviation. *p*-values are not adjusted for multiple comparisons. CI = confidence interval."
```
