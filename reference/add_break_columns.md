# Add break columns

Add break columns

## Usage

``` r
add_break_columns(
  d,
  ...,
  .before = FALSE,
  omit_first = FALSE,
  omit_last = FALSE
)
```

## Arguments

- d:

  data.frame or tibble

- ...:

  Column name or tidyselect function. Select columns

- .before:

  insert break columns before selected columns (defaults to FALSE)

- omit_first:

  omit the first break column

- omit_last:

  omit the last break column

## Value

data.frame or tibble

## Examples

``` r
d <- data.frame(x_n = 3, x_mean = 4,
            y_n = 5, y_mean = 6,
            z_n = 4, z_mean = 4)
# Unquoted variable names
add_break_columns(d, x_mean)
#>   x_n x_mean apa7breakcolumn1 y_n y_mean z_n z_mean
#> 1   3      4               NA   5      6   4      4

# Character vector
add_break_columns(d, c("y_n", "z_n"),  .before = TRUE)
#>   x_n x_mean apa7breakcolumn1 y_n y_mean apa7breakcolumn2 z_n z_mean
#> 1   3      4               NA   5      6               NA   4      4

# Tidyselect function (contains, starts_with, ends_with,
# matches, num_range, all_of, any_of)
# Insert columns after all columns
# ending with "_mean" except the last instance
add_break_columns(d,
                  dplyr::ends_with("_mean"),
                  omit_last = TRUE)
#>   x_n x_mean apa7breakcolumn1 y_n y_mean apa7breakcolumn2 z_n z_mean
#> 1   3      4               NA   5      6               NA   4      4
```
