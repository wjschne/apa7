# Make contingency table with chi-square test of independence

Make contingency table with chi-square test of independence

## Usage

``` r
apa_chisq(
  data,
  note = NULL,
  row_title_column = NULL,
  row_title_prefix = "",
  row_title_sep = " ",
  row_title_align = "center",
  row_title_border = list(color = "gray20", style = "solid", width = 1),
  left_column_padding = 20,
  cwidth = 0.75,
  cheight = 0.25,
  separate_headers = TRUE,
  apa_style = TRUE,
  font_family = NULL,
  font_size = NULL,
  text_color = "black",
  border_color = "black",
  border_width = 0.5,
  line_spacing = NULL,
  horizontal_padding = 3,
  table_align = "left",
  layout = "autofit",
  table_width = 1,
  markdown = TRUE,
  markdown_header = markdown,
  markdown_body = markdown,
  auto_format_columns = TRUE,
  column_formats = NULL,
  pretty_widths = TRUE,
  suppress_warnings = TRUE,
  ...
)
```

## Arguments

- data:

  A two-column data.frame or tibble

- note:

  Custom note (overrides automatic note.)

- row_title_column:

  Column name or tidyselect function. column to group rows

- row_title_prefix:

  text to be added to each title

- row_title_sep:

  separator for prefix

- row_title_align:

  alignment of row title ("left", "center", "right")

- row_title_border:

  list of flextable styles

- left_column_padding:

  Number of points the left column is padded (only relevant when there
  is a `row_title_column` and `row_title_align = "left"`)

- cwidth:

  initial cell width in inches

- cheight:

  initial cell height in inches

- separate_headers:

  separate header rows (default: `TRUE`)

- apa_style:

  apply `apa_style` function (default: `TRUE`)

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

- layout:

  table layout ("autofit", "fixed")

- table_width:

  proportion of the width of the container (e.g., the page or text
  column) that the table occupies, from 0 to 1: 1 (default) is the full
  width, and .5 is half the width. With `pretty_widths = TRUE`, the
  column widths are also set to fill the same proportion of the width of
  the text area, which is 6.5 inches (letter-size paper with 1-inch
  margins) unless changed with `apa7_defaults(page_width = )` (e.g.,
  `apa7_defaults(page_width = "a4")`).

- markdown:

  apply markdown formatting to header and body

- markdown_header:

  apply markdown formatting to header

- markdown_body:

  apply markdown formatting to body

- auto_format_columns:

  if true, will attempt to format some columns automatically

- column_formats:

  a column_formats object

- pretty_widths:

  apply `pretty_widths` function

- suppress_warnings:

  Suppress any warnings if true.

- ...:

  arguments passed to `apa_style`

## Value

flextable::flextable

## Examples

``` r
apa_chisq(mtcars[, c("am", "gear")])


.cl-d8bad469{}.cl-fd7caf76{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f148d26d{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-2c5a1875{font-family:'DejaVu Sans';font-size:11pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-5430bca3{font-family:'DejaVu Sans';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-56775354{font-family:'DejaVu Sans';font-size:7.2pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;vertical-align:3.74pt;}.cl-7a9068f9{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-8dbfbcff{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-6054d6f2{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:27.3pt;padding-right:24.3pt;line-height: 1.15;background-color:transparent;}.cl-0e7c069b{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1.164;background-color:transparent;}.cl-aee05d18{width:1.324in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-21678254{width:0.924in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-674696d8{width:1.802in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-b9056ad2{width:0.647in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c9003547{width:1.324in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-cd0ec083{width:0.924in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bc6a980d{width:1.802in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-534ea76a{width:0.647in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5dc004b7{width:1.324in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-0fbb37b6{width:0.924in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6f89e66a{width:1.802in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-2854efa2{width:0.647in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-921749a8{width:1.324in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-9fc98278{width:0.924in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-8ce1e06a{width:1.802in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6f13e7eb{width:0.647in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}



gear
```
