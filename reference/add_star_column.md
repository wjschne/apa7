# Adds stars next to a column based on p-values

Adds stars next to a column based on p-values

## Usage

``` r
add_star_column(
  data,
  ...,
  p = "p",
  merge = FALSE,
  superscript = TRUE,
  star = "\\*",
  alpha = c(0.05, 0.01, 0.001),
  first_alpha_marginal = FALSE,
  add_trailing_space = FALSE,
  prefix = "\\"
)
```

## Arguments

- data:

  data.frame or tibble

- ...:

  Column name or tidyselect function. Select columns

- p:

  Column name or tidyselect function. Select p-value column name

- merge:

  merge and balance columns (default: `FALSE`)

- superscript:

  make as superscript

- star:

  text for making stars

- alpha:

  vector of thresholds

- first_alpha_marginal:

  if TRUE, the first alpha value is treated as marginal and gets a
  dagger instead of a star

- add_trailing_space:

  if TRUE, adds a trailing space after the stars (default: FALSE)

- prefix:

  usually backslashes to prevent markdown from interpreting asterisks as
  bullets or italics

## Value

data.frame

## Examples

``` r
data.frame(b = c(1.4,2.2),
           p = c(.54, .02)) |>
 add_star_column(b, p)
#>     b bapa7starcolumn    p papa7starcolumn
#> 1 1.4                 0.54                
#> 2 2.2           ^\\*^ 0.02           ^\\*^
```
