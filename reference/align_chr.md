# Align text on center text (default is decimal)

Align text on center text (default is decimal)

## Usage

``` r
align_chr(
  x,
  accuracy = NULL,
  trim_leading_zeros = FALSE,
  drop0trailing = FALSE,
  add_plusses = FALSE,
  padding_character = NULL,
  center = ".",
  format_integers = FALSE,
  side = c("both", "left", "right"),
  NA_value = "",
  format_numeric_character = FALSE,
  ...
)
```

## Arguments

- x:

  vector (numeric or character)

- accuracy:

  number to round to. If NULL, the current default accuracy set with
  [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md)
  will be used.

- trim_leading_zeros:

  if TRUE (default), trims leading zeros, otherwise keeps them

- drop0trailing:

  Drop trailing zeros

- add_plusses:

  if TRUE (default), adds a plus to positive numbers

- padding_character:

  character to use for padding, default is `&numsp;` (figure space)

- center:

  text on which to align text. Center on decimal by default, but can be
  any text.

- format_integers:

  If TRUE, integers will be formatted with digits

- side:

  side on which to make text of equal width

- NA_value:

  value to replace NA

- format_numeric_character:

  format character variables with numeric content

- ...:

  additional arguments passed to
  [`signs::signs()`](https://benjaminwolfe.github.io/signs/reference/signs.html)

## Value

character vector

## Examples

``` r
align_chr(c(1, 10, 100))
#> [1] "  1" " 10" "100"
```
