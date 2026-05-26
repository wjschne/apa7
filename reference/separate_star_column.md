# Add columns that separate significance stars from numbers

Add columns that separate significance stars from numbers

## Usage

``` r
separate_star_column(
  data,
  ...,
  superscript = TRUE,
  star = "\\*",
  star_replace = "\\\\*"
)
```

## Arguments

- data:

  data.frame or tibble

- ...:

  Column name or tidyselect function. Select columns

- superscript:

  make stars superscript

- star:

  character to use for stars (default: "\\")

- star_replace:

  character to replace stars with (default: "\\\*")

## Value

data.frame or tibble

## Examples

``` r
tibble::tibble(x = c(".45", ".58*", ".68**"),
               y = c(1,2,3),
               z = 4:6) |>
               separate_star_column(x)
#> # A tibble: 3 × 4
#>   x     xapa7starcolumn     y     z
#>   <chr> <chr>           <dbl> <int>
#> 1 .45    NA                 1     4
#> 2 .58   "^\\*^"             2     5
#> 3 .68   "^\\*\\*^"          3     6
```
