# format model performance metrics in APA style

format model performance metrics in APA style

## Usage

``` r
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

For `glm` objects, `R2` is the pseudo-*R*^2^ that
[`performance::r2()`](https://easystats.github.io/performance/reference/r2.html)
selects for the model family (e.g., Tjur's *R*^2^ for logistic
regression and Nagelkerke's *R*^2^ for count models). The header names
the pseudo-*R*^2^ that was used. Because the residual standard deviation
is not meaningful for most GLMs, the default metrics are `R2` and `AIC`.

For mixed models ([`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html)
and [`lme4::glmer`](https://rdrr.io/pkg/lme4/man/glmer.html)), the
default metrics are the marginal *R*^2^ (variance explained by the fixed
effects), the conditional *R*^2^ (fixed and random effects), and the
intraclass correlation (ICC), as computed by
[`performance::model_performance()`](https://easystats.github.io/performance/reference/model_performance.html).

For a list of models, each model's metrics are displayed in a row,
labeled in a `Model` column by the list's names (or "Model 1", "Model
2", ... for unnamed elements). If `metrics` is `NULL` (default), each
model's default metrics are used.

## Examples

``` r
lm(mpg ~ cyl + wt, data = mtcars) |>
   apa_performance() |>
   apa_flextable()


.cl-d8bad469{}.cl-f148d26d{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-733a321e{font-family:'Times New Roman';font-size:7.2pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;vertical-align:super;}.cl-fd7caf76{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-8812a095{font-family:'Times New Roman';font-size:7.2pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;vertical-align:sub;}.cl-86359788{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-5b771c80{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-cfb6e4f4{width:2.875in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-58ccb284{width:3.625in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-0f968896{width:2.875in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-73ed64fd{width:3.625in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


R2
```
