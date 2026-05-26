# Prefix text with figure spaces to balance star text

Prefix text with figure spaces to balance star text

## Usage

``` r
star_balance(x, star = "\\*", superscript = TRUE)
```

## Arguments

- x:

  character vector

- star:

  star text

- superscript:

  Place superscript text if `TRUE`

## Value

character vector

## Examples

``` r

star_balance(".05\\^\\*\\*\\^")
#> [1] "^&numsp;&numsp;^.05\\^\\*\\*\\^"
```
