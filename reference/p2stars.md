# Convert p-values to stars

Convert p-values to stars

## Usage

``` r
p2stars(
  p,
  alpha = c(0.05, 0.01, 0.001),
  first_alpha_marginal = FALSE,
  superscript = FALSE,
  add_trailing_space = FALSE,
  prefix = "\\"
)
```

## Arguments

- p:

  vector of numbers

- alpha:

  vector of thresholds

- first_alpha_marginal:

  if TRUE, the first alpha value is treated as marginal and gets a
  dagger instead of a star

- superscript:

  make as superscript

- add_trailing_space:

  if TRUE, adds a trailing space after the stars (default: FALSE)

- prefix:

  usually backslashes to prevent markdown from interpreting asterisks as
  bullets or italics

## Value

character vector

## Examples

``` r
p2stars(c(.32, .02, .005),
        alpha = c(.05, .01))
#> [1] ""       "\\*"    "\\*\\*"
```
