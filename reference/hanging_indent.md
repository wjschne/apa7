# Return markdown text with hanging indent

Return markdown text with hanging indent

## Usage

``` r
hanging_indent(
  x,
  indent = 4,
  width = 30,
  space = NULL,
  newline = "\\\\\n",
  whitespace_only = FALSE,
  wrap_equal_width = FALSE
)
```

## Arguments

- x:

  text

- indent:

  number of `space` characters that indent the second and later lines

- width:

  maximum number of characters per line

- space:

  indenting character. If `NULL` (default), a non-breaking space.

- newline:

  text inserted at each line break (default: a markdown hard line break,
  a backslash followed by a newline)

- whitespace_only:

  if `TRUE`, lines break only at spaces. If `FALSE` (default), they can
  also break after hyphens and slashes (see
  [`stringr::str_wrap()`](https://stringr.tidyverse.org/reference/str_wrap.html)).

- wrap_equal_width:

  if `TRUE`, lines are made roughly equal in width (see
  [`str_wrap_equal()`](https://wjschne.github.io/apa7/reference/str_wrap_equal.md)).
  If `FALSE` (default), each line is filled up to `width`.

## Value

character vector

## Examples

``` r
hanging_indent(
  "Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Erlbaum."
)
#> [1] "Cohen, J. (1988). Statistical\\\n    power analysis for the\\\n    behavioral sciences (2nd ed.).\\\n    Erlbaum."
```
