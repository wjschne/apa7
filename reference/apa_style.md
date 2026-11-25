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

  object

- font_family:

  font family

- font_size:

  font size, in points. If `NULL` (default), the size set with
  `apa7_defaults(font_size = )` (12 unless changed).

- text_color:

  text color

- border_color:

  border color

- border_width:

  border width in pixels

- line_spacing:

  spacing between lines of the header and body (e.g., 2 for double
  spacing or 1 for single spacing). If `NULL` (default), the spacing set
  with `apa7_defaults(line_spacing = )` (2 unless changed). Table notes
  are single-spaced.

- horizontal_padding:

  horizontal padding (in pixels)

- table_align:

  table alignment ("left", "center", "right")

- header_align_vertical:

  vertical alignment of headers. Can be "top", "middle", or "bottom"

- layout:

  table layout ("autofit", "fixed")

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

  apply markdown formatting to header and body

- markdown_header:

  apply markdown formatting to header

- markdown_body:

  apply markdown formatting to body

- no_markdown_columns:

  body columns that should not be treated as markdown

- no_markdown_columns_header:

  column headers that should not be treated as markdown

- separate_headers:

  separate headers into column spanner labels

## Value

object

## Examples

``` r
d <- data.frame(x = 1:3, y = 4:6)
flextable::flextable(d) |>
  apa_style()


.cl-eefbcfb5{table-layout:auto;width:100%;}.cl-fd7caf76{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-7a9068f9{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-6e29b6b7{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-aa2adb3c{background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c16497ad{background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-46dbcd25{background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


x
```
