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

  number of spaces to indent

- width:

  number of characters to break lines

- space:

  indenting space character (defaults to non-breaking space)

- newline:

  text for creating new line

- whitespace_only:

  wrapping spaces only

- wrap_equal_width:

  Attempts to split lines to make them of approximately equal width

## Value

character vector

## Examples

``` r
hanging_indent("Hello Darkness, my old friend. I've come to talk with you again.")
#> [1] "Hello Darkness, my old friend.\\\n    I've come to talk with you\\\n    again."
```
