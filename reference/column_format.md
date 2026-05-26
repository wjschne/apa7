# Column format class

This class is used to define the format of columns in tables, including
the name, header, latex representation, and a formatter function.

## Usage

``` r
column_format(
  name = character(0),
  header = character(0),
  latex = character(0),
  formatter = function() NULL
)
```

## Arguments

- name:

  name of column

- header:

  markdown representation of header name

- latex:

  latex representation of header name

- formatter:

  function that formats the column values. It should take a vector of
  values and return a character vector of formatted values.

## Value

column_format object

## Examples

``` r
R2 <- column_format(
         "R2",
         header = "*R*^2^",
         latex = "$R^2$",
         formatter = \(x, accuracy = the$accuracy, ...) {
                       align_chr(x,
                                 accuracy = accuracy,
                                 trim_leading_zeros = TRUE,
                                 ...)
                       })
R2
#> 
#> ── column_format ──
#> 
#> # A tibble: 1 × 4
#>   name  header latex formatter
#>   <chr> <chr>  <chr> <list>   
#> 1 R2    *R*^2^ $R^2$ <fn>     
R2@header
#> [1] "*R*^2^"
R2@formatter
#> function (x, accuracy = the$accuracy, ...) 
#> {
#>     align_chr(x, accuracy = accuracy, trim_leading_zeros = TRUE, 
#>         ...)
#> }
#> <environment: 0x55d132848790>
```
