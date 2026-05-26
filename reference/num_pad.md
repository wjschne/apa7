# Pads text on the left or right so that the width is the same for each element of the vector

Pads text on the left or right so that the width is the same for each
element of the vector

## Usage

``` r
num_pad(x, pad_left = TRUE, padding_character = "&numsp;", NA_value = "")
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

## Value

character vector

## Examples

``` r
num_pad(c("a", "bb"))
#> [1] "&numsp;a" "bb"      
```
