# Set defaults for apa7 package

Set defaults for apa7 package

## Usage

``` r
apa7_defaults(
  accuracy = NULL,
  font_family = NULL,
  intercept_text = NULL,
  column_formats = NULL,
  number_formatter = NULL,
  trim_leading_zero = NULL,
  width_aware_padding = NULL,
  reset = FALSE
)
```

## Arguments

- accuracy:

  numeric (default: .01)

- font_family:

  font family

- intercept_text:

  what to call the intercept

- column_formats:

  column formatting functions

- number_formatter:

  default function to format numbers

- trim_leading_zero:

  default function to trim leading zeros from numbers

- width_aware_padding:

  if `TRUE` (default),
  [`align_chr()`](https://wjschne.github.io/apa7/reference/align_chr.md)
  pads text to equal rendered width in `font_family` (when the font is
  installed). If `FALSE`, text is padded with one figure space per
  missing character.

- reset:

  if `TRUE`, reset all defaults (except as specified)

## Value

previous defaults

## Examples

``` r
apa7_defaults(accuracy = .001)
# Reset to package defaults
apa7_defaults(reset = TRUE)
```
