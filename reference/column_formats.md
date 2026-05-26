# Create a set of column formats

Returns an S7 object that contains a list of `column_format` objects
that can be used to format parameters in APA style.

## Usage

``` r
column_formats(
  .data = NULL,
  accuracy = NULL,
  intercept_text = NULL,
  starred_columns = character(0),
  variable_labels = character(0),
  custom_columns = NULL
)
```

## Arguments

- .data:

  list of `column_format` objects

- accuracy:

  numeric (passed to scales::number)

- intercept_text:

  describe intercept

- starred_columns:

  which columns get p-value stars

- variable_labels:

  named vector of variable names (with vector names as labels). For
  example, c(`Parental Income` = "parental_income", `Number of Siblings`
  = "n_siblings")

- custom_columns:

  named list of column_formats to add or replace existing columns

## Value

column_formats

## Slots

- `get_column_names`:

  getter for column names

- `get_headers`:

  getter for column headers

- `get_latex`:

  getter for column latex headers

- `get_formatters`:

  getter for column formatters

- `get_header_rename`:

  getter for column names with headers as names

- `get_header_rename_latex`:

  getter for column names with latex headers as names

- `get_tibble`:

  getter for tibble with column names, headers, latex headers, and
  formatters

## Examples

``` r
my_formatter <- column_formats()
my_formatter$Coefficient@formatter <- \(x) round(x, 2)
my_formatter$Coefficient@formatter(2.214)
#> [1] 2.21
```
