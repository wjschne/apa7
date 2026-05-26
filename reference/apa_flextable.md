# Convert data to flextable consistent with APA style

The `apa_flextable` function performs a number of formatting operations
on the data before and after the data are sent to flextable. See
Details.

## Usage

``` r
apa_flextable(
  data,
  row_title_column = NULL,
  row_title_align = "left",
  row_title_prefix = "",
  row_title_sep = " ",
  row_title_border = list(color = "gray20", style = "solid", width = 1),
  left_column_padding = 20,
  col_keys = colnames(data),
  cwidth = 0.75,
  cheight = 0.25,
  header_align_vertical = c("top", "middle", "bottom"),
  separate_headers = TRUE,
  apa_style = TRUE,
  font_family = NULL,
  font_size = 12,
  text_color = "black",
  border_color = "black",
  border_width = 0.5,
  line_spacing = 2,
  horizontal_padding = 3,
  table_align = "left",
  layout = "autofit",
  table_width = 1,
  markdown = TRUE,
  markdown_header = markdown,
  markdown_body = markdown,
  no_markdown_columns = NULL,
  no_markdown_columns_header = NULL,
  no_format_columns = NULL,
  auto_format_columns = TRUE,
  column_formats = NULL,
  pretty_widths = TRUE,
  add_breaks_between_spanners = TRUE,
  ...
)
```

## Arguments

- data:

  data.frame or tibble

- row_title_column:

  Column name or tidyselect function. column to group rows

- row_title_align:

  alignment of row title ("left", "center", "right")

- row_title_prefix:

  text to be added to each title

- row_title_sep:

  separator for prefix

- row_title_border:

  list of flextable styles

- left_column_padding:

  Number of points the left column is padded (only relevant when there
  is a `row_title_column` and `row_title_align = "left"`)

- col_keys:

  column keys passed to flextable (defaults data column names)

- cwidth:

  initial cell width in inches

- cheight:

  initial cell height in inches

- header_align_vertical:

  vertical alignment of headers. Can be "top", "middle", or "bottom"

- separate_headers:

  separate header rows (default: `TRUE`)

- apa_style:

  apply `apa_style` function (default: `TRUE`)

- font_family:

  font family

- font_size:

  font size

- text_color:

  text color

- border_color:

  border color

- border_width:

  border width in pixels

- line_spacing:

  spacing between lines

- horizontal_padding:

  horizontal padding (in pixels)

- table_align:

  table alignment ("left", "center", "right")

- layout:

  table layout ("autofit", "fixed")

- table_width:

  table width (in pixels, 0 for auto)

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

- no_format_columns:

  Column name or tidyselect function. selected columns are not formatted

- auto_format_columns:

  if true, will attempt to format some columns automatically

- column_formats:

  a column_formats object

- pretty_widths:

  apply `pretty_widths` function

- add_breaks_between_spanners:

  add breaks between spanners if TRUE

- ...:

  arguments passed to `apa_style`

## Value

flextable::flextable

## Details

Roughly speaking, `apa_flextable` performs these operations by default:

1.  Apply as_grouped_data and restructure row titles, if `row_title` is
    specified.

2.  Format data with apa_format_columns if `auto_format_columns = TRUE`

3.  Separate headers into multiple header rows if
    `separate_headers = TRUE`

4.  Apply
    [`flextable::flextable`](https://davidgohel.github.io/flextable/reference/flextable.html)

5.  Apply
    [`flextable::surround`](https://davidgohel.github.io/flextable/reference/surround.html)
    to make borders to separate row groups, if any.

6.  Apply the `apa_style` function (table formatting and markdown
    conversion) if `apa_style = TRUE`

7.  Apply `pretty_widths` if `pretty_widths = TRUE`

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(tidyr)
library(flextable)
mtcars %>%
  dplyr::select(vs, am, gear, carb) |>
  tidyr::pivot_longer(-vs,  names_to = "Variable") |>
  dplyr::summarise(Mean = round(mean(value), 2),
                   SD = round(sd(value), 2),
                   .by = c(Variable,vs)) |>
  dplyr::mutate(vs = factor(vs, levels = 0:1, labels = c("Automatic", "Manual"))) |>
  apa_flextable(row_title_column= vs,  row_title_align = "center") |>
  align(j = 2:3, align = "center")


.cl-262a160c{}.cl-26228a54{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-26228a68{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-26256d64{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-26256d6e{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-26258db2{width:2.821in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-26258dbc{width:2.021in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-26258dc6{width:1.659in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-26258dc7{width:2.821in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-26258dc8{width:2.021in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-26258dd0{width:1.659in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-26258dd1{width:2.821in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-26258dda{width:2.821in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(51, 51, 51, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-26258ddb{width:2.021in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(51, 51, 51, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-26258de4{width:1.659in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(51, 51, 51, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-26258de5{width:2.821in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(51, 51, 51, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-26258dee{width:2.021in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(51, 51, 51, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-26258def{width:1.659in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(51, 51, 51, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-26258df0{width:2.821in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-26258e02{width:2.021in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-26258e03{width:1.659in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Variable
```
