# Factor correlation table

Makes an APA-style table of the correlations among factors (or
components) from an obliquely rotated solution: a
[`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) or
[`psych::principal()`](https://rdrr.io/pkg/psych/man/principal.html)
object, or the output of
[`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html)
or
[`parameters::principal_components()`](https://easystats.github.io/parameters/reference/principal_components.html).
For a `lavaan` model, the table shows the correlations among the latent
variables in the fully standardized solution. When latent variables are
linked by regression paths, these correlations are implied by the model.

## Usage

``` r
apa_factor_cor(fit, factor_names = NULL, digits = 2)
```

## Arguments

- fit:

  a [`psych::fa`](https://rdrr.io/pkg/psych/man/fa.html),
  [`psych::principal`](https://rdrr.io/pkg/psych/man/principal.html),
  [`parameters::factor_analysis`](https://easystats.github.io/parameters/reference/principal_components.html),
  [`parameters::principal_components`](https://easystats.github.io/parameters/reference/principal_components.html),
  or `lavaan` object

- factor_names:

  names for the factors. Either an unnamed character vector in the order
  the factors appear in the table (psych orders factors by variance
  explained), or a named vector that maps the original names to new
  names (e.g., `c(PA1 = "Verbal")`). The unnamed form is safer: after an
  oblimin rotation, the labels psych assigns (PA1, PA2, ...) can change
  from one run to the next, but the order of the factors does not.

- digits:

  number of decimal places (unaffected by `apa7_defaults(accuracy = )`)

## Value

`apa7_tbl` (tibble)

## Details

The factors are numbered in the first column, and the numbers are used
as column headers. Only the lower triangle is shown. The factors are in
the same order as in
[`apa_loadings()`](https://wjschne.github.io/apa7/reference/apa_loadings.md).

## Examples

``` r
fit <- psych::fa(Harman74.cor$cov, 4, fm = "pa", rotate = "oblimin")
#> Loading required namespace: GPArotation
apa_factor_cor(
  fit,
  factor_names = c("Verbal", "Spatial", "Speed", "Memory")
)
#> # A tibble: 4 × 5
#>   Factor     `1`   `2`    `3`    `4`  
#>   <chr>      <chr> <chr>  <chr>  <chr>
#> 1 1. Verbal  —     ""     ""     ""   
#> 2 2. Spatial  .41  "—"    ""     ""   
#> 3 3. Speed    .30  " .27" "—"    ""   
#> 4 4. Memory   .40  " .38" " .32" "—"  
```
