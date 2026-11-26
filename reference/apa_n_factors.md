# Factor retention table

Makes an APA-style table of how many factors (or components) each method
suggests retaining, as computed by
[`parameters::n_factors()`](https://easystats.github.io/parameters/reference/n_factors.html).

## Usage

``` r
apa_n_factors(
  fit,
  table = c("methods", "agreement"),
  bold_consensus = TRUE,
  format_method_names = TRUE,
  components = NULL,
  ...
)
```

## Arguments

- fit:

  the output of
  [`parameters::n_factors()`](https://easystats.github.io/parameters/reference/n_factors.html),
  or a data frame passed to it

- table:

  `"methods"` (default) for a row per method, or `"agreement"` for a row
  per suggested number of factors

- bold_consensus:

  if `TRUE` (default), the numbers of factors supported by the most
  methods are bolded (methods table only)

- format_method_names:

  if `TRUE` (default), method names are spelled out (e.g., "Velicer's
  MAP" becomes "Velicer's minimum average partial (MAP)" and "beta"
  becomes "Multiple regression (*β*)"). Set to `FALSE` to keep the names
  exactly as
  [`parameters::n_factors()`](https://easystats.github.io/parameters/reference/n_factors.html)
  gives them.

- components:

  if `TRUE`, the table and note speak of components rather than factors.
  If `NULL` (default), components are assumed when `fit` is a data frame
  and `type = "PCA"` is passed in `...`. The output of
  [`parameters::n_factors()`](https://easystats.github.io/parameters/reference/n_factors.html)
  and
  [`parameters::n_components()`](https://easystats.github.io/parameters/reference/n_factors.html)
  does not record which was estimated, so set `components = TRUE` for
  the output of
  [`parameters::n_components()`](https://easystats.github.io/parameters/reference/n_factors.html).

- ...:

  arguments passed to
  [`parameters::n_factors()`](https://easystats.github.io/parameters/reference/n_factors.html)
  when `fit` is a data frame (e.g., `type`, `rotation`, `algorithm`, and
  `package`)

## Value

`apa7_tbl` (tibble)

## Details

`fit` can be the output of
[`parameters::n_factors()`](https://easystats.github.io/parameters/reference/n_factors.html),
or a data frame, in which case
[`parameters::n_factors()`](https://easystats.github.io/parameters/reference/n_factors.html)
is run with the arguments in `...` (e.g., `type = "PCA"` for components,
or `package` to choose the packages whose methods are used: by default
nFactors and psych, which must be installed).

With `table = "methods"` (default), each row is a method and the number
of factors it suggests, in the order of the number of factors. The
numbers supported by the most methods are bolded
(`bold_consensus = TRUE`). With `table = "agreement"`, each row is a
number of factors suggested by at least one method, with the number of
methods that suggest it and the cumulative proportion of variance
explained by that many factors.

The note names the number of factors supported by the most methods and
those methods. Methods differ greatly in how well they work, and they
often disagree: parallel analysis and Velicer's minimum average partial
(MAP) test are generally recommended, while the Kaiser criterion
(eigenvalues greater than 1) and the chi-square tests (Bartlett's,
Anderson's, and Lawley's) tend to suggest too many factors. The number
of methods that agree is not by itself a good reason to retain a number
of factors.

## References

Horn, J. L. (1965). A rationale and test for the number of factors in
factor analysis. *Psychometrika, 30*(2), 179–185.
[doi:10.1007/BF02289447](https://doi.org/10.1007/BF02289447)

Kaiser, H. F. (1960). The application of electronic computers to factor
analysis. *Educational and Psychological Measurement, 20*(1), 141–151.
[doi:10.1177/001316446002000116](https://doi.org/10.1177/001316446002000116)

Velicer, W. F. (1976). Determining the number of components from the
matrix of partial correlations. *Psychometrika, 41*(3), 321–327.
[doi:10.1007/BF02293557](https://doi.org/10.1007/BF02293557)

## Examples

``` r
# \donttest{
if (requireNamespace("nFactors", quietly = TRUE)) {
  d <- na.omit(psych::bfi[, 1:25])
  apa_n_factors(d)

  # One row per suggested number of factors
  nf <- parameters::n_factors(d)
  apa_n_factors(nf, table = "agreement")

  apa_n_factors(nf) |>
    apa_note()
}
#> [1] "The numbers of factors supported by the most methods are 6, 7, and 24, each by 3 of 19 methods. The numbers of factors supported by the most methods are bolded."
# }
```
