# Format ANOVA tables in APA style

Makes an analysis of variance source table with sums of squares, degrees
of freedom, mean squares, *F* tests, *p*-values, and effect sizes.
Values come from
[`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html),
and effect sizes come from the `effectsize` package (e.g.,
[`effectsize::eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html)).

## Usage

``` r
apa_anova(
  fit,
  anova_parameters = c("Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
    "Effect_Size"),
  effect_size = c("eta", "omega", "epsilon"),
  partial = TRUE,
  generalized = FALSE,
  type = NULL,
  ci = 0.95,
  residual_label = "Error",
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  ...
)

# S3 method for class 'lm'
apa_anova(
  fit,
  anova_parameters = c("Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
    "Effect_Size"),
  effect_size = c("eta", "omega", "epsilon"),
  partial = TRUE,
  generalized = FALSE,
  type = NULL,
  ci = 0.95,
  residual_label = "Error",
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  ...
)

# S3 method for class 'aovlist'
apa_anova(
  fit,
  anova_parameters = c("Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
    "Effect_Size"),
  effect_size = c("eta", "omega", "epsilon"),
  partial = TRUE,
  generalized = FALSE,
  type = NULL,
  ci = 0.95,
  residual_label = "Error",
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  ...
)

# S3 method for class 'anova'
apa_anova(
  fit,
  anova_parameters = c("Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
    "Effect_Size"),
  effect_size = c("eta", "omega", "epsilon"),
  partial = TRUE,
  generalized = FALSE,
  type = NULL,
  ci = 0.95,
  residual_label = "Error",
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  ...
)

# S3 method for class 'afex_aov'
apa_anova(
  fit,
  anova_parameters = c("Parameter", "df", "df_error", "Mean_Square", "F", "p",
    "Effect_Size"),
  effect_size = c("eta", "omega", "epsilon"),
  partial = TRUE,
  generalized = TRUE,
  type = NULL,
  ci = 0.95,
  residual_label = "Error",
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  ...
)

# S3 method for class 'merMod'
apa_anova(
  fit,
  anova_parameters = mixed_anova_parameters,
  effect_size = NULL,
  partial = TRUE,
  generalized = FALSE,
  type = NULL,
  ci = 0.95,
  residual_label = "Error",
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  ddf = c("Kenward-Roger", "Satterthwaite"),
  ...
)

# Default S3 method
apa_anova(fit, ...)
```

## Arguments

- fit:

  an `aov` or `lm` model, a repeated-measures `aov` model with an
  `Error()` term (class `aovlist`), an `afex` model (class `afex_aov`),
  a linear mixed model from
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) or
  [`lmerTest::lmer()`](https://rdrr.io/pkg/lmerTest/man/lmer.html), or
  an ANOVA table from
  [`stats::anova()`](https://rdrr.io/r/stats/anova.html),
  [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html), or
  lmerTest's [`anova()`](https://rdrr.io/r/stats/anova.html).

- anova_parameters:

  columns to display. `"Effect_Size"` stands for whichever effect size
  `effect_size`, `partial`, and `generalized` select, and `"CI"` adds
  its confidence interval as \[*LL*, *UL*\]. Columns not in the ANOVA
  table are silently dropped.

- effect_size:

  effect size to display: `"eta"` (\\\eta^2\\, default), `"omega"`
  (\\\omega^2\\), or `"epsilon"` (\\\epsilon^2\\). If `NULL`, no effect
  size is computed.

- partial:

  if `TRUE` (default), partial effect sizes (e.g., \\\eta^2_p\\) are
  displayed. For one-way designs, partial and non-partial effect sizes
  are the same.

- generalized:

  if `TRUE`, generalized \\\eta^2\\ (\\\eta^2_G\\) is displayed instead,
  which is comparable across between- and within-subjects designs. Only
  for `effect_size = "eta"`.

- type:

  type of sums of squares for `aov` and `lm` models: `1` (sequential,
  default, as in
  [`stats::anova()`](https://rdrr.io/r/stats/anova.html)), `2`, or `3`.
  Types 2 and 3 are computed with
  [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html). Type 3 tests
  are interpretable only when factors have sum-to-zero contrasts (e.g.,
  `contr.sum`) and covariates are centered. For linear mixed models, the
  type of hypothesis tests: `3` (default, as in lmerTest), `2`, or `1`.
  Ignored for ANOVA tables, which already have a type.

- ci:

  confidence level for the effect size interval, shown if
  `anova_parameters` includes `"CI"`. Intervals are two-sided, so a 90%
  interval excludes 0 when the *F* test is significant at \\\alpha\\ =
  .05, and its lower bound is 0 otherwise. Intervals are not shown for
  generalized \\\eta^2\\, because those computed by `effectsize` can
  exclude the point estimate.

- residual_label:

  label for residual (error) rows

- starred:

  columns to star with significant *p*-values (e.g., `"F"`)

- bolded:

  columns to bold, if significant

- column_formats:

  column_formats object to format columns. If NULL, the default
  column_formats is used.

- ...:

  further arguments passed to
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html)

- ddf:

  for linear mixed models, method of approximating the denominator
  degrees of freedom: `"Kenward-Roger"` (default) or `"Satterthwaite"`

## Value

`apa7_tbl` (tibble)

## Details

For repeated-measures models fit with `aov(y ~ a * b + Error(id / b))`,
each error stratum is introduced by a heading row ("Between subjects" or
"Within subjects"), and each stratum's error term follows its effects.

Model comparisons (e.g., `anova(m1, m2)`) are not ANOVA source tables.
Use
[`apa_performance_comparison()`](https://wjschne.github.io/apa7/reference/apa_performance_comparison.md)
for them.

For `afex` models
([`afex::aov_car()`](https://rdrr.io/pkg/afex/man/aov_car.html),
[`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html),
[`afex::aov_4()`](https://rdrr.io/pkg/afex/man/aov_car.html)), each
effect has its own error term, so the table shows each effect's degrees
of freedom (*df*~effect~, *df*~error~), mean squared error (*MSE*), *F*,
*p*, and effect size, as in `afex`'s own `nice()` tables. Degrees of
freedom and *p*-values reflect the sphericity correction set in `afex`
(Greenhouse-Geisser by default), and generalized \\\eta^2\\ is the
default effect size, as in `afex`.

For linear mixed models fit with
[`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) or
[`lmerTest::lmer()`](https://rdrr.io/pkg/lmerTest/man/lmer.html), *F*
tests of the fixed effects come from the lmerTest package, which
approximates the denominator degrees of freedom with Kenward-Roger's
method (default, as in
[`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md),
which requires the pbkrtest package) or Satterthwaite's method
(`ddf = "Satterthwaite"`). Tests are Type III by default, as in
lmerTest, whose Type III tests do not depend on how factors are coded.
The table shows each effect's degrees of freedom (*df*~effect~,
*df*~error~), *F*, and *p*. Sums of squares and effect sizes have no
agreed-upon definition in mixed models, so they are not shown by
default. With `effect_size = "eta"` (or `"omega"` or `"epsilon"`),
partial effect sizes are computed from *F* and its degrees of freedom
(e.g.,
[`effectsize::F_to_eta2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.html)),
which only approximates them. ANOVA tables made by lmerTest's
[`anova()`](https://rdrr.io/r/stats/anova.html) are displayed the same
way. Generalized linear mixed models
([`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html)) have no *F*
tests; use
[`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
for them.

## Examples

``` r
d <- mtcars
d$cyl <- factor(d$cyl)
d$am <- factor(d$am)
fit <- aov(mpg ~ cyl * am, data = d)
apa_anova(fit)
#> # A tibble: 4 × 7
#>   Source   `*SS*` `*df*` `*MS*` `*F*`   `*p*`   `*&eta;*^2^~p~`
#>   <chr>    <chr>  <chr>  <chr>  <chr>   <chr>   <chr>          
#> 1 Cyl      824.78  2     412.39 "44.85" "<.001" ".78"          
#> 2 Am        36.77  1      36.77 " 4.00" " .06 " ".13"          
#> 3 Cyl × Am  25.44  2      12.72 " 1.38" " .27 " ".10"          
#> 4 Error    239.06 26       9.19 ""      ""      ""             

# Omega squared with a 90% confidence interval
apa_anova(
  fit,
  anova_parameters = c(
    "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
    "Effect_Size", "CI"
  ),
  effect_size = "omega",
  ci = .90
)
#> # A tibble: 4 × 8
#>   Source   `*SS*` `*df*` `*MS*` `*F*`   `*p*`   `*&omega;*^2^~p~` `90% CI`    
#>   <chr>    <chr>  <chr>  <chr>  <chr>   <chr>   <chr>             <chr>       
#> 1 Cyl      824.78  2     412.39 "44.85" "<.001" ".73"             "[.56, .82]"
#> 2 Am        36.77  1      36.77 " 4.00" " .06 " ".09"             "[.00, .29]"
#> 3 Cyl × Am  25.44  2      12.72 " 1.38" " .27 " ".02"             "[.00, .13]"
#> 4 Error    239.06 26       9.19 ""      ""      ""                ""          

# Type 2 sums of squares (requires the car package)
if (requireNamespace("car", quietly = TRUE)) {
  apa_anova(fit, type = 2)
}
#> # A tibble: 4 × 7
#>   Source   `*SS*` `*df*` `*MS*` `*F*`   `*p*`   `*&eta;*^2^~p~`
#>   <chr>    <chr>  <chr>  <chr>  <chr>   <chr>   <chr>          
#> 1 Cyl      456.40  2     228.20 "24.82" "<.001" ".66"          
#> 2 Am        36.77  1      36.77 " 4.00" " .06 " ".13"          
#> 3 Cyl × Am  25.44  2      12.72 " 1.38" " .27 " ".10"          
#> 4 Error    239.06 26       9.19 ""      ""      ""             
if (requireNamespace("afex", quietly = TRUE)) {
  data(obk.long, package = "afex")
  fit_afex <- afex::aov_ez(
    "id", "value", obk.long,
    between = "treatment", within = "phase", fun_aggregate = mean
  )
  apa_anova(fit_afex)
}
#> Registered S3 method overwritten by 'lme4':
#>   method           from
#>   na.action.merMod car 
#> Contrasts set to contr.sum for the following variables: treatment
#> # A tibble: 3 × 7
#>   Effect        `*df*~effect~` `*df*~error~` `*MSE*` `*F*` `*p*` `*&eta;*^2^~G~`
#>   <chr>         <chr>          <chr>         <chr>   <chr> <chr> <chr>          
#> 1 Treatment     2.00           13.00         6.41     2.91  .09  .27            
#> 2 Phase         1.74           22.64         0.81    19.29 <.001 .21            
#> 3 Treatment × … 3.48           22.64         0.81     5.43  .004 .13            
if (requireNamespace("lmerTest", quietly = TRUE) &&
  requireNamespace("pbkrtest", quietly = TRUE)) {
  fit_lmer <- lme4::lmer(
    Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy
  )
  apa_anova(fit_lmer)
}
#> # A tibble: 1 × 5
#>   Effect `*df*~effect~` `*df*~error~` `*F*` `*p*`
#>   <chr>  <chr>          <chr>         <chr> <chr>
#> 1 Days   1              17.00         45.85 <.001
```
