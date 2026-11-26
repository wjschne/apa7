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

  rounding accuracy of the formatters (e.g., `.01` for two decimals). If
  `NULL` (default), the `accuracy` set with
  [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md).

- intercept_text:

  label of the intercept row (e.g., `"Constant"` or `"Intercept"`)

- starred_columns:

  names of columns to which significance stars are added

- variable_labels:

  named character vector that relabels variables: the names are the
  labels and the values are the variable names (e.g.,
  `c("Parental Income" = "parental_income", "Number of Siblings" = "n_siblings")`)

- custom_columns:

  named list of
  [`column_format()`](https://wjschne.github.io/apa7/reference/column_format.md)
  objects that are added, or that replace the formats of the same name

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

- `get_abbreviations`:

  getter for the abbreviations defined by the column formats, as a
  character vector of definitions named by the term defined

## Examples

``` r
my_formats <- column_formats()
my_formats$Coefficient@formatter <- \(x) round(x, 2)
my_formats$Coefficient@formatter(2.214)
#> [1] 2.21
```
