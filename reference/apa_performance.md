# format model performance metrics in APA style

format model performance metrics in APA style

Fit indices of lavaan models

## Usage

``` r
apa_performance(fit, metrics = c("R2", "Sigma"), column_formats = NULL)

# S3 method for class 'gam'
apa_performance(
  fit,
  metrics = c("R2_adjusted", "Deviance_explained", "AIC"),
  column_formats = NULL
)

# S3 method for class 'bam'
apa_performance(
  fit,
  metrics = c("R2_adjusted", "Deviance_explained", "AIC"),
  column_formats = NULL
)

# S3 method for class 'lavaan'
apa_performance(
  fit,
  metrics = c("Chi2", "Chi2_df", "p_Chi2", "CFI", "TLI", "RMSEA", "RMSEA_CI", "SRMR"),
  column_formats = NULL
)

# S3 method for class 'rma'
apa_performance(fit, metrics = NULL, column_formats = NULL)

# S3 method for class 'multinom'
apa_performance(fit, metrics = c("R2", "AIC"), column_formats = NULL)

# S3 method for class 'zeroinfl'
apa_performance(fit, metrics = c("R2", "AIC"), column_formats = NULL)

# S3 method for class 'hurdle'
apa_performance(fit, metrics = c("R2", "AIC"), column_formats = NULL)

# S3 method for class 'lm'
apa_performance(fit, metrics = c("R2", "Sigma"), column_formats = NULL)

# S3 method for class 'glm'
apa_performance(fit, metrics = c("R2", "AIC"), column_formats = NULL)

# S3 method for class 'polr'
apa_performance(fit, metrics = c("R2", "AIC"), column_formats = NULL)

# S3 method for class 'clm'
apa_performance(fit, metrics = c("R2", "AIC"), column_formats = NULL)

# S3 method for class 'merMod'
apa_performance(
  fit,
  metrics = c("R2_marginal", "R2_conditional", "ICC"),
  column_formats = NULL
)

# S3 method for class 'glmmTMB'
apa_performance(
  fit,
  metrics = c("R2_marginal", "R2_conditional", "ICC"),
  column_formats = NULL
)

# S3 method for class 'lme'
apa_performance(
  fit,
  metrics = c("R2_marginal", "R2_conditional", "ICC"),
  column_formats = NULL
)

# S3 method for class 'list'
apa_performance(fit, metrics = NULL, column_formats = NULL)
```

## Arguments

- fit:

  a fitted model of any class that
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  accepts (except `aov`, `aovlist`, and `afex_aov`), or a named list of
  models

- metrics:

  fit statistics to display, named as in
  [`performance::model_performance()`](https://easystats.github.io/performance/reference/model_performance.html)
  (e.g., `"R2"`, `"R2_adjusted"`, `"AIC"`, `"BIC"`, `"RMSE"`,
  `"Sigma"`). The defaults depend on the model: `R2` and `Sigma` for
  `lm`; `R2` (a pseudo-*R*^2^) and `AIC` for `glm`, ordinal,
  multinomial, zero-inflated, and hurdle models; `R2_marginal`,
  `R2_conditional`, and `ICC` for mixed models; `R2_adjusted`,
  `Deviance_explained`, and `AIC` for generalized additive models; and
  the fit indices listed under `apa_performance.lavaan()` for `lavaan`
  models. For meta-analyses and lists, `NULL` (default) chooses them.

- column_formats:

  column_formats object to format columns. If NULL, the default
  column_formats is used.

## Value

`apa7_tbl` (tibble)

## Details

For generalized additive models
([`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html) and
[`mgcv::bam`](https://rdrr.io/pkg/mgcv/man/bam.html)), the metrics are
the adjusted *R*^2^ (`"R2_adjusted"`), the percentage of the deviance
explained (`"Deviance_explained"`), the number of observations (`"n"`),
`"AIC"`, and `"BIC"`, as reported by
[`summary()`](https://rdrr.io/r/base/summary.html). The defaults are
`"R2_adjusted"`, `"Deviance_explained"`, and `"AIC"`.

For structural equation models and confirmatory factor analyses fit with
`lavaan` (e.g.,
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) or
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html)), the default
metrics are the model \\\chi^2\\ test (`"Chi2"`, `"Chi2_df"`,
`"p_Chi2"`), the comparative fit index (`"CFI"`), the Tucker-Lewis index
(`"TLI"`), the root mean square error of approximation with its
confidence interval (`"RMSEA"`, `"RMSEA_CI"`), and the standardized root
mean square residual (`"SRMR"`). `"AIC"` and `"BIC"` are also available,
and `"all"` selects every metric. Values come from
[`lavaan::fitMeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html).
When the model was fit with a robust estimator (e.g.,
`estimator = "MLR"` or `"WLSMV"`), the scaled \\\chi^2\\ test and the
robust (or, if unavailable, scaled) CFI, TLI, and RMSEA are displayed.
[`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
gives the sample size, the estimator, and how missing data were handled,
and defines the abbreviations.

For meta-analyses
([`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)),
the metrics are the number of studies (`"k"`), the test of residual
heterogeneity (`"QE"` and `"p_QE"`, with its *df* in the header),
\\\tau^2\\ (`"tau2"`) and \\\tau\\ (`"tau"`), \\I^2\\ (`"I2"`), \\H^2\\
(`"H2"`), confidence intervals of \\\tau^2\\ and \\I^2\\ (`"tau2_CI"`
and `"I2_CI"`, from
[`stats::confint()`](https://rdrr.io/r/stats/confint.html)), and, for
meta-regressions, the omnibus test of the moderators (`"QM"` and
`"p_QM"`) and the proportion of heterogeneity they explain (`"R2"`). The
defaults are `"k"`, `"QE"`, `"p_QE"`, `"tau2"`, and `"I2"`, plus `"QM"`,
`"p_QM"`, and `"R2"` for meta-regressions. Equal-effects models have no
\\\tau^2\\.

For multilevel meta-analyses
([`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html)),
`"k"` is the number of effect sizes, and the variance components are
reported for each level: \\\sigma^2\\ (`"sigma2"`), \\\sigma\\
(`"sigma"`), and their confidence intervals (`"sigma2_CI"`), with the
level as a subscript. `"I2"` is the multilevel \\I^2\\ for all levels
together, and `"I2_levels"` gives it for each level (Cheung, 2014). The
defaults are `"k"`, `"QE"`, `"p_QE"`, `"sigma2"`, and `"I2"`, plus
`"QM"` and `"p_QM"` for meta-regressions.

For multinomial logistic regression models
([`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html)), `R2`
is McFadden's pseudo-*R*^2^, and the default metrics are `R2` and `AIC`.

For zero-inflated and hurdle models
([`pscl::zeroinfl`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) and
[`pscl::hurdle`](https://rdrr.io/pkg/pscl/man/hurdle.html)), `R2` is
computed by
[`performance::r2_zeroinflated()`](https://easystats.github.io/performance/reference/r2_zeroinflated.html),
and the default metrics are `R2` and `AIC`.

For `glm` objects, `R2` is the pseudo-*R*^2^ that
[`performance::r2()`](https://easystats.github.io/performance/reference/r2.html)
selects for the model family (e.g., Tjur's *R*^2^ for logistic
regression and Nagelkerke's *R*^2^ for count models). The header names
the pseudo-*R*^2^ that was used. Because the residual standard deviation
is not meaningful for most GLMs, the default metrics are `R2` and `AIC`.

For mixed models
([`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html),
[`lme4::glmer`](https://rdrr.io/pkg/lme4/man/glmer.html),
[`glmmTMB::glmmTMB`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html), and
[`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html)), the default
metrics are the marginal *R*^2^ (variance explained by the fixed
effects), the conditional *R*^2^ (fixed and random effects), and the
intraclass correlation (ICC), as computed by
[`performance::model_performance()`](https://easystats.github.io/performance/reference/model_performance.html).

For a list of models, each model's metrics are displayed in a row,
labeled in a `Model` column by the list's names (or "Model 1", "Model
2", ... for unnamed elements). If `metrics` is `NULL` (default), each
model's default metrics are used.

## References

McFadden, D. (1974). Conditional logit analysis of qualitative choice
behavior. In P. Zarembka (Ed.), *Frontiers in econometrics* (pp.
105–142). Academic Press.

Nagelkerke, N. J. D. (1991). A note on a general definition of the
coefficient of determination. *Biometrika, 78*(3), 691–692.
[doi:10.1093/biomet/78.3.691](https://doi.org/10.1093/biomet/78.3.691)

Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for
obtaining \\R^2\\ from generalized linear mixed-effects models. *Methods
in Ecology and Evolution, 4*(2), 133–142.
[doi:10.1111/j.2041-210x.2012.00261.x](https://doi.org/10.1111/j.2041-210x.2012.00261.x)

Satorra, A., & Bentler, P. M. (1994). Corrections to test statistics and
standard errors in covariance structure analysis. In A. von Eye & C. C.
Clogg (Eds.), *Latent variables analysis: Applications for developmental
research* (pp. 399–419). Sage.

Tjur, T. (2009). Coefficients of determination in logistic regression
models—A new proposal: The coefficient of discrimination. *The American
Statistician, 63*(4), 366–372.
[doi:10.1198/tast.2009.08210](https://doi.org/10.1198/tast.2009.08210)

Yuan, K.-H., & Bentler, P. M. (2000). Three likelihood-based methods for
mean and covariance structure analysis with nonnormal missing data.
*Sociological Methodology, 30*(1), 165–200.
[doi:10.1111/0081-1750.00078](https://doi.org/10.1111/0081-1750.00078)

## Examples

``` r
lm(mpg ~ cyl + wt, data = mtcars) |>
   apa_performance() |>
   apa_flextable()


.cl-d8bad469{}.cl-f148d26d{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-733a321e{font-family:'Times New Roman';font-size:7.8pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;vertical-align:4.08pt;}.cl-fd7caf76{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-8812a095{font-family:'Times New Roman';font-size:7.8pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;vertical-align:-0.84pt;}.cl-7a9068f9{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-8dbfbcff{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-cfb6e4f4{width:2.875in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-58ccb284{width:3.625in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-0f968896{width:2.875in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-73ed64fd{width:3.625in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


R2
```
