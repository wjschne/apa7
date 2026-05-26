# Prepend column spanner labels to data column labels

Prepend column spanner labels to data column labels

## Usage

``` r
column_spanner_label(data, label, ..., relocate = TRUE)
```

## Arguments

- data:

  data.frame or tibble

- label:

  character of column spanner

- ...:

  columns (i.e., one or more tidyselect functions and/or a vector of
  quoted or unquoted variable names)

- relocate:

  relocate columns with same spanner label to be adjacent

## Value

data.frame or tibble

## Examples

``` r
d <- data.frame(y = 1:3, x1 = 2:4, x2 = 3:5)

# Unquoted variable names
column_spanner_label(d, "Label", c(x1, x2))
#>   y Label_x1 Label_x2
#> 1 1        2        3
#> 2 2        3        4
#> 3 3        4        5
# Character values (quoted variable names)
column_spanner_label(d, "Label", c("x1", "x2"))
#>   y Label_x1 Label_x2
#> 1 1        2        3
#> 2 2        3        4
#> 3 3        4        5
# Tidyselect function (e.g., starts_with, ends_with, contains)
column_spanner_label(d, "Label", dplyr::starts_with("x"))
#>   y Label_x1 Label_x2
#> 1 1        2        3
#> 2 2        3        4
#> 3 3        4        5
# Tidyselect range
column_spanner_label(d, "Label", x1:x2)
#>   y Label_x1 Label_x2
#> 1 1        2        3
#> 2 2        3        4
#> 3 3        4        5
# Selected variables are relocated after the first selected variable
column_spanner_label(d, "Label", c(x2, y))
#>   x1 Label_x2 Label_y
#> 1  2        3       1
#> 2  3        4       2
#> 3  4        5       3
```
