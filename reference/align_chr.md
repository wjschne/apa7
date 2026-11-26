# Align text on center text (default is decimal)

`align_chr()` formats numbers and pads them on both sides of the decimal
point (or another `center` character) so that every value has the same
width. When the column is centered, the decimal points line up.

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

  if `TRUE`, leading zeros are removed (e.g., .45 instead of 0.45), as
  APA style requires for values that cannot exceed 1 (correlations,
  proportions, *p*-values). If `FALSE` (default), they are kept.

- drop0trailing:

  Drop trailing zeros

- add_plusses:

  if `TRUE`, positive numbers get a plus sign. If `FALSE` (default),
  they do not.

- padding_character:

  character to use for padding, default is `&numsp;` (figure space).
  With the default, text is padded to equal rendered width in the
  default font family (see
  [`num_pad()`](https://wjschne.github.io/apa7/reference/num_pad.md) and
  the `width_aware_padding` argument of
  [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md)).

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

## Details

With a few minor details omitted, `align_chr()` does roughly this:

1.  If `format_numeric_character = TRUE`, converts text made only of
    digits, minus signs, and periods to numbers.

2.  Formats numbers with
    [`signs::signs()`](https://benjaminwolfe.github.io/signs/reference/signs.html):
    rounds them to `accuracy`, replaces hyphens with true minus signs,
    and adds commas to large numbers. Plus signs are added if
    `add_plusses = TRUE`, leading zeros are removed if
    `trim_leading_zeros = TRUE`, and trailing zeros are dropped if
    `drop0trailing = TRUE`. Whole numbers are not given decimals unless
    `format_integers = TRUE`.

3.  Splits each value at the first `center` character, then pads the
    part before it on the left and the part after it on the right
    (`side` limits the padding to one side), so that every value has the
    same width on each side of the center.

By default, values are padded with figure spaces, which are as wide as a
digit. With this default, if `width_aware_padding = TRUE` in
[`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md)
(the default) and the default font is installed, values are instead
padded to the same *rendered* width in that font (see
[`num_pad()`](https://wjschne.github.io/apa7/reference/num_pad.md)), so
that values with minus signs, commas, or markdown still line up. Other
`padding_character` values are counted in characters.

The padded values line up only if every cell in the column is aligned
the same way.
[`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
centers the columns of numbers and lines up padded values (see its
Details).

## Examples

``` r
align_chr(c(1, 10, 100))
#> [1] "  1" " 10" "100"
```
