# Style `flextable::flextable` object according to APA style

Style
[`flextable::flextable`](https://davidgohel.github.io/flextable/reference/flextable.html)
object according to APA style

## Usage

``` r
apa_style(
  x,
  font_family = NULL,
  font_size = NULL,
  text_color = "black",
  border_color = "black",
  border_width = 0.5,
  line_spacing = NULL,
  horizontal_padding = 3,
  table_align = "left",
  header_align_vertical = c("top", "middle", "bottom"),
  layout = "autofit",
  table_width = NULL,
  markdown = TRUE,
  markdown_header = markdown,
  markdown_body = markdown,
  no_markdown_columns = NULL,
  no_markdown_columns_header = no_markdown_columns,
  separate_headers = TRUE
)
```

## Arguments

- x:

  a
  [`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)

- font_family:

  font family (e.g., `"Times New Roman"` or `"Arial"`). If `NULL`
  (default), the font set with `apa7_defaults(font_family = )` (Times
  New Roman unless changed).

- font_size:

  font size, in points. If `NULL` (default), the size set with
  `apa7_defaults(font_size = )` (12 unless changed).

- text_color:

  color of the text: an R color name (e.g., `"black"`, the default) or a
  hex code (e.g., `"#333333"`)

- border_color:

  color of the rules, in the same forms as `text_color`

- border_width:

  width of the rules, in points (default: 0.5)

- line_spacing:

  spacing between lines of the header and body (e.g., 2 for double
  spacing or 1 for single spacing). If `NULL` (default), the spacing set
  with `apa7_defaults(line_spacing = )` (2 unless changed). Table notes
  are single-spaced.

- horizontal_padding:

  space to the left and right of each cell's contents, in points
  (default: 3)

- table_align:

  alignment of the table on the page: `"left"` (default), `"center"`, or
  `"right"`

- header_align_vertical:

  vertical alignment of header text when header cells differ in height:
  `"top"` (default), `"middle"`, or `"bottom"`

- layout:

  `"autofit"` (default) lets the column widths adjust to the contents in
  Word; `"fixed"` keeps the widths as set (see
  [`flextable::set_table_properties()`](https://davidgohel.github.io/flextable/reference/set_table_properties.html))

- table_width:

  width of the table, in the same forms in every apa7 function: a
  proportion from 0 to 1 of the default width (the full width of a
  portrait page unless changed with `apa7_defaults(table_width = )`),
  `"page"` (or `"portrait"`) for the full width of a portrait page,
  `"landscape"` for a landscape page, `"column"` for one column of a
  two-column page, or a named number: a proportion of one of these
  widths (e.g., `c(landscape = .8)`) or a width in `inches`, `cm`, or
  `mm` (e.g., `c(inches = 5)`). With 0, the columns are only as wide as
  their contents need. If `NULL` (default), the width set by the code
  chunk's `apa7-width` option in Quarto (e.g.,
  `#| apa7-width: landscape`, or `#| apa7-width: {landscape: .8}` for a
  named number) or `apa7.width` option in R Markdown, or `"page"` for a
  chunk with apaquarto's `apa-twocolumn: true`; otherwise, the
  `table_width` set with
  [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md)
  (1 unless changed). The widths of the pages and the column are set
  with the `page_width`, `landscape_width`, and `column_width` arguments
  of
  [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md).
  A number greater than 1 is read as inches, with a warning to write it
  as `c(inches = )`.

- markdown:

  if `TRUE` (default), text in the header and body is converted from
  markdown (e.g., `*p*` becomes an italic *p*)

- markdown_header, markdown_body:

  whether the header or the body is converted from markdown (both
  default to `markdown`)

- no_markdown_columns:

  body columns whose text is shown as it is, not converted from markdown
  (unquoted or quoted names, or tidyselect helpers)

- no_markdown_columns_header:

  column headers that are not converted from markdown. By default, the
  same columns as `no_markdown_columns`.

- separate_headers:

  if `TRUE` (default) and the table has one header row, column names
  with an underscore are split into a column spanner and a header (e.g.,
  `Model 1_B` becomes "B" under a "Model 1" spanner)

## Value

flextable::flextable

## Examples

``` r
d <- data.frame(x = 1:3, y = 4:6)
flextable::flextable(d) |>
  apa_style()


.cl-eefbcfb5{table-layout:auto;width:100%;}.cl-fd7caf76{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-7a9068f9{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-6e29b6b7{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-aa2adb3c{background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c16497ad{background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-46dbcd25{background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


x
```
