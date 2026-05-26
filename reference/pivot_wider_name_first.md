# A wrapper for tidyr::pivot_wider that creates columns names as `name_variable` instead of `variable_name`

The default for `names_vary` is "slowest" instead of the usual
"fastest".

## Usage

``` r
pivot_wider_name_first(
  data,
  ...,
  id_cols = NULL,
  id_expand = FALSE,
  names_from = name,
  names_prefix = "",
  names_sep = "_",
  names_sort = FALSE,
  names_vary = "slowest",
  names_expand = FALSE,
  names_repair = "check_unique",
  values_from = value,
  values_fill = NULL,
  values_fn = NULL,
  unused_fn = NULL
)
```

## Arguments

- data:

  A data frame to pivot.

- ...:

  Additional arguments passed on to methods.

- id_cols:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  A set of columns that uniquely identify each observation. Typically
  used when you have redundant variables, i.e. variables whose values
  are perfectly correlated with existing variables.

  Defaults to all columns in `data` except for the columns specified
  through `names_from` and `values_from`. If a tidyselect expression is
  supplied, it will be evaluated on `data` after removing the columns
  specified through `names_from` and `values_from`.

- id_expand:

  Should the values in the `id_cols` columns be expanded by
  [`expand()`](https://tidyr.tidyverse.org/reference/expand.html) before
  pivoting? This results in more rows, the output will contain a
  complete expansion of all possible values in `id_cols`. Implicit
  factor levels that aren't represented in the data will become
  explicit. Additionally, the row values corresponding to the expanded
  `id_cols` will be sorted.

- names_from, values_from:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  A pair of arguments describing which column (or columns) to get the
  name of the output column (`names_from`), and which column (or
  columns) to get the cell values from (`values_from`).

  If `values_from` contains multiple values, the value will be added to
  the front of the output column.

- names_prefix:

  String added to the start of every variable name. This is particularly
  useful if `names_from` is a numeric vector and you want to create
  syntactic variable names.

- names_sep:

  If `names_from` or `values_from` contains multiple variables, this
  will be used to join their values together into a single string to use
  as a column name.

- names_sort:

  Should the column names be sorted? If `FALSE`, the default, column
  names are ordered by first appearance.

- names_vary:

  When `names_from` identifies a column (or columns) with multiple
  unique values, and multiple `values_from` columns are provided, in
  what order should the resulting column names be combined?

  - `"fastest"` varies `names_from` values fastest, resulting in a
    column naming scheme of the form:
    `value1_name1, value1_name2, value2_name1, value2_name2`. This is
    the default.

  - `"slowest"` varies `names_from` values slowest, resulting in a
    column naming scheme of the form:
    `value1_name1, value2_name1, value1_name2, value2_name2`.

- names_expand:

  Should the values in the `names_from` columns be expanded by
  [`expand()`](https://tidyr.tidyverse.org/reference/expand.html) before
  pivoting? This results in more columns, the output will contain column
  names corresponding to a complete expansion of all possible values in
  `names_from`. Implicit factor levels that aren't represented in the
  data will become explicit. Additionally, the column names will be
  sorted, identical to what `names_sort` would produce.

- names_repair:

  What happens if the output has invalid column names? The default,
  `"check_unique"` is to error if the columns are duplicated. Use
  `"minimal"` to allow duplicates in the output, or `"unique"` to
  de-duplicated by adding numeric suffixes. See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for more options.

- values_fill:

  Optionally, a (scalar) value that specifies what each `value` should
  be filled in with when missing.

  This can be a named list if you want to apply different fill values to
  different value columns.

- values_fn:

  Optionally, a function applied to the value in each cell in the
  output. You will typically use this when the combination of `id_cols`
  and `names_from` columns does not uniquely identify an observation.

  This can be a named list if you want to apply different aggregations
  to different `values_from` columns.

- unused_fn:

  Optionally, a function applied to summarize the values from the unused
  columns (i.e. columns not identified by `id_cols`, `names_from`, or
  `values_from`).

  The default drops all unused columns from the result.

  This can be a named list if you want to apply different aggregations
  to different unused columns.

  `id_cols` must be supplied for `unused_fn` to be useful, since
  otherwise all unspecified columns will be considered `id_cols`.

  This is similar to grouping by the `id_cols` then summarizing the
  unused columns using `unused_fn`.

## Value

data.frame

## Examples

``` r
d <- data.frame(Model = paste0("Model","_", 1:2), t = 1:2, p = c(.05, .45))
pivot_wider_name_first(d, names_from = Model, values_from = c(t, p))
#> # A tibble: 1 × 4
#>   Model_1_t Model_1_p Model_2_t Model_2_p
#>       <int>     <dbl>     <int>     <dbl>
#> 1         1      0.05         2      0.45
# For comparison:
tidyr::pivot_wider(d, names_from = Model, values_from = c(t, p))
#> # A tibble: 1 × 4
#>   t_Model_1 t_Model_2 p_Model_1 p_Model_2
#>       <int>     <int>     <dbl>     <dbl>
#> 1         1         2      0.05      0.45
```
