# Use flextable::dim_pretty to fit column widths

Use flextable::dim_pretty to fit column widths

## Usage

``` r
pretty_widths(
  x,
  min_width = 0.05,
  unit = c("in", "cm", "mm"),
  table_width = NULL
)
```

## Arguments

- x:

  flextable

- min_width:

  minimum width of columns

- unit:

  Can be `in`, `cm`, or `mm`

- table_width:

  width of table, in `unit`. If `NULL` (default), the width of the text
  area set with `apa7_defaults(page_width = )` (6.5 inches unless
  changed). Use 0 to keep the widths that fit the contents.

## Value

flextable::flextable
