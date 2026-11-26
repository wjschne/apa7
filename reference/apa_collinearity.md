# Collinearity diagnostics table

Makes an APA-style table of the variance inflation factor (*VIF*) and
tolerance of each predictor in a regression model, as computed by
[`performance::check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.html).

## Usage

``` r
apa_collinearity(
  x,
  columns = c("VIF", "VIF_CI", "Tolerance"),
  flag = NULL,
  ci = 0.95,
  format_variable_names = TRUE,
  digits = 2,
  ...
)
```

## Arguments

- x:

  a regression model with at least two predictors (any model that
  [`performance::check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.html)
  accepts, e.g., `lm`, `glm`, `lme4`, `glmmTMB`, or `pscl` models), or
  the output of
  [`performance::check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.html)

- columns:

  columns to display, in order: any of `"VIF"`, `"VIF_CI"` (confidence
  interval of *VIF*), `"SE_factor"` (\\\sqrt{VIF}\\, the factor by which
  the standard error is inflated), `"Tolerance"`, and `"Tolerance_CI"`

- flag:

  `NULL` (default) or a number: *VIF*s at or above it are bolded (e.g.,
  `flag = 5` or `flag = 10`), and the note says so

- ci:

  confidence level of the intervals

- format_variable_names:

  if `TRUE` (default), term names are formatted as in
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  (e.g., "factor(cyl)" becomes "Cyl", and "wt:hp" becomes "Wt \\\times\\
  Hp"). Set to `FALSE` to keep the names exactly as
  [`performance::check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.html)
  gives them.

- digits:

  number of decimal places (unaffected by `apa7_defaults(accuracy = )`)

- ...:

  arguments passed to
  [`performance::check_collinearity()`](https://easystats.github.io/performance/reference/check_collinearity.html)
  (e.g., `component`)

## Value

`apa7_tbl` (tibble)

## Details

The *VIF* of a predictor is the factor by which the variance of its
coefficient is inflated by its correlations with the other predictors.
Tolerance is 1/*VIF*, the proportion of the predictor's variance that
the other predictors do not explain. Common rules of thumb treat *VIF*s
above 5 or 10 as signs of problematic collinearity; `flag` bolds them.

For terms with more than 1 degree of freedom (e.g., factors with more
than two levels), *VIF* is the generalized *VIF* (Fox & Monette, 1992),
and `"SE_factor"` is \\GVIF^{1/(2df)}\\, which is comparable across
terms with different degrees of freedom. The note names these terms.

In models with interactions, the *VIF*s of the interaction and its
component terms are high in part because product terms are correlated
with their components. This kind of collinearity does not affect the
test of the interaction, and centering the predictors reduces it. When
any of these *VIF*s is at or above `flag` (or 5, if `flag` is `NULL`),
the note says so.

Models with several parts (e.g., the count and zero-inflation models of
[`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) or
[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html))
get a heading row for each part.

## References

Fox, J., & Monette, G. (1992). Generalized collinearity diagnostics.
*Journal of the American Statistical Association, 87*(417), 178–183.
[doi:10.1080/01621459.1992.10475190](https://doi.org/10.1080/01621459.1992.10475190)

## Examples

``` r
fit <- lm(mpg ~ wt + hp + disp + factor(cyl), data = mtcars)
apa_collinearity(fit)
#> # A tibble: 4 × 4
#>   Variable `*VIF*` `95% CI for *VIF*` Tolerance
#>   <chr>    <chr>   <chr>              <chr>    
#> 1 Wt        5.37   [2.60,  7.30]      .19      
#> 2 Hp        3.53   [1.79,  4.80]      .28      
#> 3 Disp     12.90   [5.99, 17.49]      .08      
#> 4 Cyl       8.99   [4.43, 12.57]      .11      

# Bold VIFs of 10 or more, and show the note
apa_collinearity(fit, flag = 10) |>
  apa_note()
#> [1] "For Cyl, which has more than 1 *df*, *VIF* is the generalized *VIF* (Fox & Monette, 1992). Tolerance = 1/*VIF*, the proportion of a predictor's variance not explained by the other predictors. *VIF* = variance inflation factor; CI = confidence interval. *VIF*s of 10 or more are bolded."
```
