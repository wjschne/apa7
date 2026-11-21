# Column format class

This class is used to define the format of columns in tables, including
the name, header, latex representation, a formatter function, and what
the header abbreviates.

## Usage

``` r
column_format(
  name = character(0),
  header = character(0),
  latex = character(0),
  formatter = function() NULL,
  abbreviation = character(0)
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

- abbreviation:

  what the header stands for, used by
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  to define the abbreviations in a table's note (e.g.,
  `"standardized coefficient"` defines the `&beta;` header as
  `"&beta; = standardized coefficient"`). The term defined is the
  header, unless the string is named (e.g.,
  `c(CI = "confidence interval")` defines "CI" for a header such as "95%
  CI"). Leave empty (the default) for symbols that need no definition,
  such as *M*, *SD*, *t*, and *p*.

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
#> # A tibble: 1 × 5
#>   name  header latex formatter abbreviation
#>   <chr> <chr>  <chr> <list>    <chr>       
#> 1 R2    *R*^2^ $R^2$ <fn>      NA          
R2@header
#> [1] "*R*^2^"
R2@formatter
#> function (x, accuracy = the$accuracy, ...) 
#> {
#>     align_chr(x, accuracy = accuracy, trim_leading_zeros = TRUE, 
#>         ...)
#> }
#> <environment: 0x631970da3e60>

# Define an abbreviation in table notes
SS <- column_formats()$Sum_Squares
SS@abbreviation <- "sum of squares"
```
