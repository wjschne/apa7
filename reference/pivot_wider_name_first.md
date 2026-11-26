# A wrapper for tidyr::pivot_wider that creates column names as `name_variable` instead of `variable_name`

`pivot_wider_name_first()` reshapes data from long to wide like
[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html),
but it puts the value of `names_from` first in the new column names
(e.g., `Model 1_t` instead of `t_Model 1`). In
[`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md),
the `names_from` values then become column spanners.

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

  `"slowest"` (default) to keep the columns of each `names_from` value
  together, or `"fastest"` to interleave them (see
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html))

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

## Details

The new column names are made of `names_prefix`, the value of
`names_from`, `names_sep`, and the name of the `values_from` column, in
that order (e.g., `names_prefix = "Model "` gives `Model 1_t`). Only one
`names_from` column is allowed.

Unlike in
[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html),
`names_vary` defaults to `"slowest"`, which keeps the columns of each
`names_from` value next to each other (e.g., `Model 1_t`, `Model 1_p`,
`Model 2_t`, `Model 2_p`). Two settings keep
[`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
from making one spanner per `names_from` value:
`names_vary = "fastest"`, which interleaves the columns (`Model 1_t`,
`Model 2_t`, `Model 1_p`, `Model 2_p`), and a `names_sep` other than an
underscore, which
[`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
does not split into header rows.

## Examples

``` r
d <- data.frame(Model = 1:2, t = c(2.1, 0.8), p = c(.04, .45))
pivot_wider_name_first(d, names_from = Model, values_from = c(t, p),
                       names_prefix = "Model ")
#> # A tibble: 1 × 4
#>   `Model 1_t` `Model 1_p` `Model 2_t` `Model 2_p`
#>         <dbl>       <dbl>       <dbl>       <dbl>
#> 1         2.1        0.04         0.8        0.45
# For comparison:
tidyr::pivot_wider(d, names_from = Model, values_from = c(t, p))
#> # A tibble: 1 × 4
#>     t_1   t_2   p_1   p_2
#>   <dbl> <dbl> <dbl> <dbl>
#> 1   2.1   0.8  0.04  0.45
```
