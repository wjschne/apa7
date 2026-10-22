# Pads text on the left or right so that the width is the same for each element of the vector

By default, text is padded with one `padding_character` per missing
visible character. Markdown markup (e.g., `**`, `*`, `^`, `~`, and
backslash escapes) is not counted. If `font_family` names an installed
font, text is instead padded to equal *rendered* width in that font,
using a combination of figure, punctuation, thin, and hair spaces
(`padding_character` is then ignored). Superscripts and subscripts are
measured at 2/3 of the font size.

## Usage

``` r
num_pad(
  x,
  pad_left = TRUE,
  padding_character = "&numsp;",
  NA_value = "",
  font_family = NULL
)
```

## Arguments

- x:

  vector of text

- pad_left:

  if TRUE (default), pads on the left, otherwise pads on the right

- padding_character:

  character to use for padding, default is `&numsp;` (figure space)

- NA_value:

  value to replace NA

- font_family:

  font in which to measure text widths. If `NULL` (default) or `NA`, or
  if the font is not installed, widths are counted in characters.

## Value

character vector

## Examples

``` r
num_pad(c("a", "bb"))
#> [1] "&numsp;a" "bb"      
num_pad(c("−1", "1"), font_family = "Times New Roman")
#> [1] "−1"       "&numsp;1"
```
