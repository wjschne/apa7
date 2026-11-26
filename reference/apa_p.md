# p-value in APA format

p-value in APA format

## Usage

``` r
apa_p(
  p,
  inline = FALSE,
  markdown = TRUE,
  min_digits = 2,
  max_digits = 3,
  align = FALSE
)
```

## Arguments

- p:

  probability

- inline:

  if `TRUE`, the symbol and relation are included (e.g., *p* = .04 or
  *p* \< .001), for use in running text (one *p*-value at a time). If
  `FALSE` (default), only the number is returned (e.g., .04 or \<.001),
  for use in tables.

- markdown:

  if `TRUE` (default), the output is markdown (e.g., `*p*` in italics).
  If `FALSE`, it is plain text, which can be used in LaTeX.

- min_digits:

  minimum number of digits to round to. Default is 2.

- max_digits:

  maximum number of digits to round to. Default is 3.

- align:

  decimal alignment if `TRUE`

## Value

character vector

## Examples

``` r
# Values less than .001 are <.001
apa_p(.0002)
#> [1] "<.001"
# Values between .001 and .01 are rounded to 3 digits
apa_p(.002)
#> [1] ".002"
# Values between .01 and .995 are rounded to 2 digits
apa_p(.02)
#> [1] ".02"
apa_p(.22)
#> [1] ".22"
apa_p(.994)
#> [1] ".99"
# Values above .995 are >.99
apa_p(.999)
#> [1] "\\>.99"
# Rounding to 3 digits
apa_p(.2341, min_digits = 3)
#> [1] ".234"
apa_p(.0123, min_digits = 3)
#> [1] ".012"
apa_p(.00123, min_digits = 3)
#> [1] ".001"
apa_p(.000123, min_digits = 3)
#> [1] "<.001"
apa_p(.991, min_digits = 3)
#> [1] ".991"
apa_p(.9991, min_digits = 3)
#> [1] ".999"
apa_p(.9995, min_digits = 3)
#> [1] "\\>.999"
```
