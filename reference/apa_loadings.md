# Factor loadings table

Makes an APA-style table of factor loadings from an exploratory factor
analysis, a principal components analysis, or a confirmatory factor
analysis fit with `lavaan`.

## Usage

``` r
apa_loadings(
  fit,
  sort_loading = TRUE,
  min_loading = 0.2,
  factor_names = NULL,
  variance = FALSE,
  communality = FALSE,
  uniqueness = FALSE,
  complexity = FALSE,
  bold_max = FALSE,
  format_variable_names = TRUE,
  digits = 2
)
```

## Arguments

- fit:

  a [`psych::fa`](https://rdrr.io/pkg/psych/man/fa.html),
  [`psych::principal`](https://rdrr.io/pkg/psych/man/principal.html),
  [`parameters::factor_analysis`](https://easystats.github.io/parameters/reference/principal_components.html),
  [`parameters::principal_components`](https://easystats.github.io/parameters/reference/principal_components.html),
  or `lavaan` object

- sort_loading:

  sort the rows so that variables are grouped by the factor with their
  largest loading, and by the size of that loading within each factor
  (as in
  [`psych::fa.sort()`](https://rdrr.io/pkg/psych/man/fa.sort.html)). If
  `FALSE`, the rows keep the order of the model (for `lavaan` models,
  the order of the model syntax).

- min_loading:

  minimum absolute loading to display. Use `NA` or 0 to show all
  loadings. Defaults to .2, except for `lavaan` models, whose estimated
  loadings are all shown.

- factor_names:

  names for the factors. Either an unnamed character vector in the order
  the factors appear in the table (psych orders factors by variance
  explained), or a named vector that maps the original names to new
  names (e.g., `c(PA1 = "Verbal")`). The unnamed form is safer: after an
  oblimin rotation, the labels psych assigns (PA1, PA2, ...) can change
  from one run to the next, but the order of the factors does not.

- variance:

  if `TRUE`, rows are added for the variance explained by each factor
  (sums of squared loadings, proportion, and cumulative proportion). Not
  to be confused with the `variances` argument of
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  for `lavaan` models, which adds residual variances.

- communality:

  add a communality (*h*^2^) column

- uniqueness:

  add a uniqueness (*u*^2^) column

- complexity:

  add a column with Hofmann's complexity index

- bold_max:

  bold the largest displayed loading (in absolute value) in each row

- format_variable_names:

  format the variable names with the `Variable` column format. Set to
  `FALSE` to keep item text exactly as it is.

- digits:

  number of decimal places for loadings (unaffected by
  `apa7_defaults(accuracy = )`)

## Value

`apa7_tbl` (tibble)

## Details

`fit` can be a [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) or
[`psych::principal()`](https://rdrr.io/pkg/psych/man/principal.html)
object, or the output of
[`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html)
or
[`parameters::principal_components()`](https://easystats.github.io/parameters/reference/principal_components.html).
For the parameters functions, the loadings come from the underlying
model, so their `sort` and `threshold` settings do not matter: use
`sort_loading` and `min_loading` instead. Unrotated
[`parameters::principal_components()`](https://easystats.github.io/parameters/reference/principal_components.html)
output wraps a [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html)
model. Its loadings are the correlations between the variables and the
components when the variables are standardized (the default,
`standardize = TRUE`).

For a [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) or
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) model, the
loadings are the fully standardized estimates (as in
[`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html)),
and loadings fixed at 0 are left blank. As for exploratory analyses, the
rows are sorted by the factor with each variable's largest loading and
by the size of that loading (use `sort_loading = FALSE` to keep the
order of the model syntax). Every estimated loading is shown, unless
`min_loading` is set. The communality is the proportion of an
indicator's variance explained by the factors (\\R^2\\), and the
uniqueness is its standardized residual variance. Variable and factor
names are shown as written in the model, with the first letter
capitalized and underscores replaced by spaces. Loadings of higher-order
factors (regressions among latent variables) are not part of this table;
see
[`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md).
Multiple-group models are not yet supported.

Loadings smaller in absolute value than `min_loading` are left blank.
With `bold_max = TRUE`, the largest displayed loading in each row is
bolded.

Rows for variance explained (`variance = TRUE`) are added below the
loadings: the sum of squared loadings, the proportion of total variance,
and the cumulative proportion (the last only when there is more than one
factor).
[`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
draws a rule above these rows. With an oblique rotation the factors are
correlated, so the variance explained by each factor overlaps with the
others and the cumulative proportion is only approximate. Report the
factor correlations with
[`apa_factor_cor()`](https://wjschne.github.io/apa7/reference/apa_factor_cor.md).

The variable names are formatted (e.g., title case) when
`format_variable_names = TRUE`. Either way,
[`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
leaves the `Variable` column as it is, so item text is never changed
again when the table is rendered.

## Examples

``` r
fit <- psych::fa(Harman74.cor$cov, 4, fm = "pa", rotate = "varimax")
apa_loadings(
  fit,
  factor_names = c("Verbal", "Spatial", "Speed", "Memory")
)
#> # A tibble: 24 × 5
#>    Variable               Verbal Spatial Speed  Memory
#>    <chr>                  <chr>  <chr>   <chr>  <chr> 
#>  1 Sentence Completion    ".81"  ""      ""     ""    
#>  2 Word Meaning           ".81"  ""      ""     ".22" 
#>  3 Pargraph Comprehension ".76"  " .21"  ""     ".23" 
#>  4 General Information    ".73"  ""      " .22" ""    
#>  5 Word Classification    ".57"  " .34"  " .23" ""    
#>  6 Visual Perception      ""     " .68"  ""     ""    
#>  7 Paper Form Board       ""     " .55"  ""     ""    
#>  8 Flags                  ".23"  " .53"  ""     ""    
#>  9 Series Completion      ".37"  " .52"  " .23" ".22" 
#> 10 Cubes                  ""     " .45"  ""     ""    
#> # ℹ 14 more rows

# With rows for variance explained and bolded primary loadings
apa_loadings(fit, variance = TRUE, communality = TRUE, bold_max = TRUE)
#> # A tibble: 27 × 6
#>    Variable               PA1       PA3        PA2    PA4   `*h*^2^`
#>    <chr>                  <chr>     <chr>      <chr>  <chr> <chr>   
#>  1 Sentence Completion    "**.81**" ""         ""     ""    .73     
#>  2 Word Meaning           "**.81**" ""         ""     ".22" .74     
#>  3 Pargraph Comprehension "**.76**" " .21"     ""     ".23" .68     
#>  4 General Information    "**.73**" ""         " .22" ""    .64     
#>  5 Word Classification    "**.57**" " .34"     " .23" ""    .51     
#>  6 Visual Perception      ""        " **.68**" ""     ""    .55     
#>  7 Paper Form Board       ""        " **.55**" ""     ""    .34     
#>  8 Flags                  ".23"     " **.53**" ""     ""    .35     
#>  9 Series Completion      ".37"     " **.52**" " .23" ".22" .51     
#> 10 Cubes                  ""        " **.45**" ""     ""    .23     
#> # ℹ 17 more rows

# Principal components
psych::principal(Harman74.cor$cov, 4) |>
  apa_loadings()
#> # A tibble: 24 × 5
#>    Variable               RC1    RC3    RC2    RC4  
#>    <chr>                  <chr>  <chr>  <chr>  <chr>
#>  1 Sentence Completion    " .85" ""     ""     ""   
#>  2 Word Meaning           " .84" ""     ""     ""   
#>  3 Pargraph Comprehension " .81" ""     ""     ".21"
#>  4 General Information    " .79" ""     " .22" ""   
#>  5 Word Classification    " .64" " .31" " .24" ""   
#>  6 Problem Reasoning      " .42" " .41" ""     ".29"
#>  7 Visual Perception      ""     " .71" " .23" ""   
#>  8 Paper Form Board       ""     " .66" ""     ""   
#>  9 Flags                  " .25" " .62" ""     ""   
#> 10 Cubes                  ""     " .59" ""     ""   
#> # ℹ 14 more rows

# parameters output
parameters::principal_components(mtcars[, 1:7], n = 2) |>
  apa_loadings()
#> # A tibble: 7 × 3
#>   Variable PC1   PC2   
#>   <chr>    <chr> <chr> 
#> 1 Cyl       .96  ""    
#> 2 Disp      .95  ""    
#> 3 Mpg      −.93  ""    
#> 4 Wt        .88  "−.35"
#> 5 Hp        .87  " .36"
#> 6 Drat     −.75  " .48"
#> 7 Qsec     −.54  "−.81"

# Confirmatory factor analysis
if (requireNamespace("lavaan", quietly = TRUE)) {
  hs_model <- "visual =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed =~ x7 + x8 + x9"
  fit_cfa <- lavaan::cfa(hs_model, data = lavaan::HolzingerSwineford1939)
  apa_loadings(fit_cfa, communality = TRUE)
  apa_factor_cor(fit_cfa)
}
#> # A tibble: 3 × 4
#>   Factor     `1`   `2`    `3`  
#>   <chr>      <chr> <chr>  <chr>
#> 1 1. Visual  —     ""     ""   
#> 2 2. Textual  .46  "—"    ""   
#> 3 3. Speed    .47  " .28" "—"  
```
