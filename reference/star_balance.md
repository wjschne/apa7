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
# Starred values get one figure space per star on the left,
# so that the numbers line up when the column is centered
star_balance(c(".32", ".04\\^\\*\\^", ".005\\^\\*\\*\\^"))
#> [1] ".32"                              "^&numsp;^.04\\^\\*\\^"           
#> [3] "^&numsp;&numsp;^.005\\^\\*\\*\\^"
```
