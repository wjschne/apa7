# format model comparison metrics in APA style

format model comparison metrics in APA style

## Usage

``` r
apa_performance_comparison(
  ...,
  metrics = NULL,
  starred = NA,
  column_formats = NULL
)
```

## Arguments

- ...:

  model fit objects, or a list of them. Models are labeled by the list's
  names (or the argument names), and "Model 1", "Model 2", ... for
  unnamed models.

- metrics:

  performance metrics. If `NULL` (default), R2, deltaR2, F, and p for
  models with an estimated dispersion parameter (e.g., `lm`, Gaussian or
  quasi-family `glm`), and R2, deltaR2, Chi2, df, and p for GLMs with a
  fixed dispersion parameter (binomial, Poisson, negative binomial). Use
  "all" for all available metrics.

- starred:

  columns to star with significant p_values

- column_formats:

  column_formats object to format columns. If NULL, the default
  column_formats is used.

## Value

tibble

## Details

Nested models are compared with *F* tests
([`performance::test_wald()`](https://easystats.github.io/performance/reference/test_performance.html))
unless every model is a GLM with a fixed dispersion parameter, in which
case likelihood ratio tests
([`performance::test_likelihoodratio()`](https://easystats.github.io/performance/reference/test_performance.html))
are used. For GLMs, `R2` is the pseudo-*R*^2^ that
[`performance::r2()`](https://easystats.github.io/performance/reference/r2.html)
selects for the model family, and the header names it (e.g.,
*R*^2^~Tjur~).

Mixed models ([`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) and
[`lme4::glmer`](https://rdrr.io/pkg/lme4/man/glmer.html)) are compared
with likelihood ratio tests. `R2` is the marginal *R*^2^ (variance
explained by the fixed effects), and by default the conditional *R*^2^
(fixed and random effects) and its change (`R2_conditional`,
`deltaR2_conditional`) are shown too, because adding random effects
changes the conditional but not the marginal *R*^2^.

Nested `lavaan` models (e.g., the configural, metric, and scalar models
of a measurement invariance analysis) are compared each with the one
before it, so they should be listed from the least to the most
constrained. Each row shows the fit of the model (any of the metrics of
[`apa_performance.lavaan()`](https://wjschne.github.io/apa7/reference/apa_performance.md)),
followed, under a "Change from previous model" spanner, by the
chi-square difference test from
[`lavaan::lavTestLRT()`](https://rdrr.io/pkg/lavaan/man/lavTestLRT.html)
(`"deltaChi2"`, `"deltaChi2_df"`, `"p_deltaChi2"`) and the changes in
fit indices (`"deltaCFI"`, `"deltaTLI"`, `"deltaRMSEA"`, `"deltaSRMR"`),
shown with three decimals. The default metrics are `"Chi2"`,
`"Chi2_df"`, `"CFI"`, `"RMSEA"`, `"RMSEA_CI"`, `"SRMR"`, `"deltaChi2"`,
`"deltaChi2_df"`, `"p_deltaChi2"`, `"deltaCFI"`, and `"deltaRMSEA"`.
With a robust estimator (e.g., MLR or WLSMV), the difference test is
scaled (e.g., the Satorra-Bentler scaled difference test), and the
changes in fit indices are computed from their robust versions.
[`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
names the difference test. `starred` is ignored for `lavaan` models.

## Examples

``` r
m1 <- lm(mpg ~ cyl, data = mtcars)
m2 <- lm(mpg ~ cyl + wt, data = mtcars)
apa_performance_comparison(list(`Model 1` =m1, `Model 3` =m2)) |>
   apa_flextable()


.cl-d8bad469{}.cl-fd7caf76{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f148d26d{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-733a321e{font-family:'Times New Roman';font-size:7.8pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;vertical-align:4.08pt;}.cl-7a9068f9{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-8dbfbcff{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-2a091054{width:1.868in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-932ac912{width:0.889in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-310cc404{width:1.007in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c0f3dcf8{width:1.353in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-0237d6dd{width:1.383in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e1c93cb9{width:1.868in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7a365840{width:0.889in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6475565f{width:1.007in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7ab40080{width:1.353in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-128e1b68{width:1.383in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3ce83d03{width:1.868in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bdcc594f{width:0.889in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4000b958{width:1.007in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4673973c{width:1.353in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-9eb48196{width:1.383in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Model
```
