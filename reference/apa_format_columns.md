# Format data columns

`apa_format_columns()` formats the columns of a data frame of results
and renames their headers in APA style, using a set of
[`column_formats()`](https://wjschne.github.io/apa7/reference/column_formats.md).
[`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
calls it when `auto_format_columns = TRUE`.

## Usage

``` r
apa_format_columns(
  data,
  column_formats = NULL,
  no_format_columns = NULL,
  rename_headers = TRUE,
  latex_headers = FALSE,
  format_separated_headers = TRUE,
  sep = "_",
  accuracy = NULL
)
```

## Arguments

- data:

  data set (data.frame or tibble)

- column_formats:

  `column_formats` object. If NULL, the current default formatter set
  with
  [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md)
  will be used.

- no_format_columns:

  Column name or tidyselect function. selected columns are not formatted

- rename_headers:

  if `TRUE`, rename headers with markdown or latex

- latex_headers:

  if `TRUE`, rename headers with latex instead of markdown

- format_separated_headers:

  if `TRUE`, format headers with separated names. For example, if the
  formatter formats column `R2` as `*R*^2^`, then `Model 1_R2` becomes
  `Model 1_*R*^2^`)

- sep:

  separator for separated headers (default is "\_")

- accuracy:

  numeric (default: NULL, uses the current default accuracy set with
  [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md)).
  If not NULL, sets the accuracy for the formatter.

## Value

tibble

## Details

Each column is matched by name to a
[`column_format()`](https://wjschne.github.io/apa7/reference/column_format.md)
in `column_formats` (e.g., `p`, `SE`, `beta`, or `R2`). The values of a
matched column are formatted by the format's formatter, and its name is
replaced by the format's markdown header (e.g., `beta` becomes
`*&beta;*`), or by its LaTeX header if `latex_headers = TRUE`. Columns
without a format, and columns named in `no_format_columns`, are left as
they are.

If `format_separated_headers = TRUE` (default), a column name with a
separator (`sep`, an underscore by default) is matched by its last part,
so `Model 1_R2` is formatted like `R2` and becomes `Model 1_*R*^2^`. In
[`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md),
the part before the separator becomes a column spanner. Format names
that contain an underscore themselves (e.g., `R2_adjusted`) are matched
whole.

A few columns are matched by a pattern or combined:

- `CI`, `CI_low`, and `CI_high` (e.g., from
  [`parameters::parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html))
  become a single column of intervals, with a header such as "95% CI"
  that gives the confidence level stored in `CI`.

- If there are `t` and `df_error` columns, the degrees of freedom are
  moved into the `t` header (e.g., *t*(28)), and `df_error` is dropped.

- A column named like `t(28)` is formatted with the `t_df` format (and
  gets the header *t*(28)), and one named like `90% CI` with the
  `CI_percent` format.

To see the default formats, run `column_formats()@get_tibble`. The
article [Building Tables by
Hand](https://wjschne.github.io/apa7/articles/summarize.html) shows how
to change them.

## Examples

``` r
lm(mpg ~ cyl + wt, data = mtcars) |>
  parameters::parameters() |>
  apa_format_columns() |>
  apa_flextable()


.cl-d8bad469{}.cl-fd7caf76{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f148d26d{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-7a9068f9{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-8dbfbcff{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-33305459{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:17.8pt;padding-right:14.8pt;line-height: 1.15;background-color:transparent;}.cl-64a8df86{width:1.245in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-31bfd6f5{width:0.879in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-655c8c36{width:0.713in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-adacec5f{width:1.905in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5e963800{width:1.245in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6ca801b5{width:0.879in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-935977b6{width:0.713in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-95adb009{width:1.905in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ef0e4251{width:0.879in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3c4de083{width:1.245in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-454e10c7{width:0.879in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f0af60db{width:0.713in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-36aeb2ce{width:1.905in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1d056958{width:0.879in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Variable
```
