# Like `stringr::str_wrap`, but attempts to create lines of equal width

Like
[`stringr::str_wrap`](https://stringr.tidyverse.org/reference/str_wrap.html),
but attempts to create lines of equal width

## Usage

``` r
str_wrap_equal(x, max_width = 30L, sep = "\n")
```

## Arguments

- x:

  character

- max_width:

  maximum line width

- sep:

  separation character

## Value

character

## Examples

``` r
str_wrap_equal("This function attempts to split the string into lines with roughly equal width.")
#> [1] "This function attempts to\nsplit the string into lines\nwith roughly equal width."
```
