# Fit indices of lavaan models

Fit indices of lavaan models

format model performance metrics in APA style

## Usage

``` r
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

apa_performance(fit, metrics = c("R2", "Sigma"), column_formats = NULL)

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

  model fit object

- metrics:

  performance metrics. Default is R2 and Sigma

- column_formats:

  column_formats object to format columns. If NULL, the default
  column_formats is used.

## Value

tibble

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

## Examples

``` r
# \donttest{
if (requireNamespace("lavaan", quietly = TRUE)) {
  model <- "visual =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed =~ x7 + x8 + x9"
  fit_cfa <- lavaan::cfa(model, data = lavaan::HolzingerSwineford1939)
  apa_performance(fit_cfa)
}
#> # A tibble: 1 × 8
#>   `*&chi;*^2^` `*df*` `*p*` CFI   TLI   RMSEA `90% CI`   SRMR 
#>   <chr>        <chr>  <chr> <chr> <chr> <chr> <chr>      <chr>
#> 1 85.31        24     <.001 .93   .90   .09   [.07, .11] .07  
# }
lm(mpg ~ cyl + wt, data = mtcars) |>
   apa_performance() |>
   apa_flextable()


.cl-d8bad469{}.cl-f148d26d{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-733a321e{font-family:'Times New Roman';font-size:7.8pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;vertical-align:4.08pt;}.cl-fd7caf76{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-8812a095{font-family:'Times New Roman';font-size:7.8pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;vertical-align:-0.84pt;}.cl-7a9068f9{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-8dbfbcff{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-cfb6e4f4{width:2.875in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-58ccb284{width:3.625in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-0f968896{width:2.875in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-73ed64fd{width:3.625in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


R2
```
