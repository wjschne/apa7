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
