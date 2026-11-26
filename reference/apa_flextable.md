# Convert data to flextable consistent with APA style

The `apa_flextable` function formats a data frame and converts it to a
flextable in APA style. See Details for the steps it takes.

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
  font_size = NULL,
  text_color = "black",
  border_color = "black",
  border_width = 0.5,
  line_spacing = NULL,
  horizontal_padding = 3,
  table_align = "left",
  layout = "autofit",
  table_width = NULL,
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
  note = NULL,
  ...
)
```

## Arguments

- data:

  data.frame or tibble

- row_title_column:

  column whose values become heading rows that group the rows below them
  (unquoted or quoted name, or a tidyselect helper). The column itself
  is not displayed. Rows should already be sorted by it.

- row_title_align:

  alignment of the heading rows: `"left"` (default), `"center"`, or
  `"right"`

- row_title_prefix:

  text placed before each heading (e.g., `"Group"` gives "Group A")

- row_title_sep:

  text between `row_title_prefix` and the heading (default: a space)

- row_title_border:

  border drawn around each group of rows: a list of `color`, `style`,
  and `width` (in points), as in
  [`officer::fp_border()`](https://davidgohel.github.io/officer/reference/fp_border.html)

- left_column_padding:

  left padding of the first column's body rows, in points, which indents
  them under left-aligned heading rows (only used with
  `row_title_column` and `row_title_align = "left"`)

- col_keys:

  columns to display, in order (default: all columns of `data`), passed
  to
  [`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)

- cwidth, cheight:

  initial cell width and height in inches, passed to
  [`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html).
  With `pretty_widths = TRUE` (default), the widths are then fitted to
  the contents and `table_width`.

- header_align_vertical:

  vertical alignment of header text when header cells differ in height:
  `"top"` (default), `"middle"`, or `"bottom"`

- separate_headers:

  if `TRUE` (default), column names with an underscore become two header
  rows: the part before it is a column spanner over the columns that
  share it (e.g., `Model 1_B` and `Model 1_SE` share a "Model 1"
  spanner)

- apa_style:

  if `TRUE` (default),
  [`apa_style()`](https://wjschne.github.io/apa7/reference/apa_style.md)
  is applied (fonts, borders, spacing, and markdown)

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

  column headers that are not treated as markdown. If `NULL` (default),
  every header is treated as markdown. (In
  [`apa_style()`](https://wjschne.github.io/apa7/reference/apa_style.md),
  the default is `no_markdown_columns`. Here it differs because numeric
  columns are added to `no_markdown_columns`, and their headers still
  need markdown.)

- no_format_columns:

  Column name or tidyselect function. selected columns are not
  formatted. Columns named in a `"no_format_columns"` attribute of
  `data` (set by functions such as
  [`apa_loadings()`](https://wjschne.github.io/apa7/reference/apa_loadings.md))
  are not formatted either. Likewise, a rule is drawn above the first
  body row whose first-column text matches a `"rule_above"` attribute of
  `data`.

- auto_format_columns:

  if `TRUE` (default), columns whose names have a format in
  `column_formats` (e.g., `p`, `SE`, or `R2`) are formatted and given
  APA headers with
  [`apa_format_columns()`](https://wjschne.github.io/apa7/reference/apa_format_columns.md)

- column_formats:

  a
  [`column_formats()`](https://wjschne.github.io/apa7/reference/column_formats.md)
  object. If `NULL` (default), the column formats set with
  [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md).

- pretty_widths:

  if `TRUE` (default), column widths are fitted to the contents and to
  `table_width` with
  [`pretty_widths()`](https://wjschne.github.io/apa7/reference/pretty_widths.md)

- add_breaks_between_spanners:

  if `TRUE` (default), a narrow empty column separates adjacent column
  spanners (see `separate_headers`), so that their rules do not touch

- note:

  table note placed below the table, after "*Note*.". The note is
  single-spaced. If `TRUE`, the note is written by
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  from what a table function such as
  [`apa_anova()`](https://wjschne.github.io/apa7/reference/apa_anova.md),
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md),
  or
  [`apa_loadings()`](https://wjschne.github.io/apa7/reference/apa_loadings.md)
  recorded. A character string is used as the note (markdown is
  allowed), and `NULL` or `FALSE` (default) adds no note. Use this for
  documents that are not made with apaquarto. With apaquarto, use the
  `apa-note` chunk option instead (e.g.,
  `#| apa-note: !expr apa7::apa_note(tbl)`), so that the note is placed
  and styled by apaquarto and is not added twice.

- ...:

  passed to
  [`apa_style()`](https://wjschne.github.io/apa7/reference/apa_style.md).
  Every argument of
  [`apa_style()`](https://wjschne.github.io/apa7/reference/apa_style.md)
  is also an argument of `apa_flextable()`, so this is rarely needed.

## Value

flextable::flextable

## Details

With a few minor details omitted, `apa_flextable` does roughly this:

1.  Adds a break column between adjacent column spanners, if
    `add_breaks_between_spanners = TRUE` and `separate_headers = TRUE`.
    A break column is a narrow blank column without borders, so the
    rules under adjacent spanners do not touch.

2.  If `auto_format_columns = TRUE`, turns columns of whole numbers
    (e.g., degrees of freedom) into integers so that they have no
    decimals. Then formats the columns with
    [`apa_format_columns()`](https://wjschne.github.io/apa7/reference/apa_format_columns.md).
    Columns named in `no_format_columns`, or in a `"no_format_columns"`
    attribute of the data (set by functions such as
    [`apa_loadings()`](https://wjschne.github.io/apa7/reference/apa_loadings.md)),
    are left as they are.

3.  Makes the flextable. With `row_title_column`, it first restructures
    the data with
    [`flextable::as_grouped_data()`](https://davidgohel.github.io/flextable/reference/as_grouped_data.html),
    and each row title gets a rule above it (`row_title_border`).

4.  Separates the headers into spanners and deckered heads, if
    `separate_headers = TRUE`.

5.  Left-aligns the first column and centers the columns of numbers.

6.  Applies
    [`apa_style()`](https://wjschne.github.io/apa7/reference/apa_style.md)
    to style the table and convert markdown, if `apa_style = TRUE`.

7.  Aligns the row titles (`row_title_align`) and indents the rows under
    them (`left_column_padding`).

8.  Sets the column widths with
    [`pretty_widths()`](https://wjschne.github.io/apa7/reference/pretty_widths.md),
    if `pretty_widths = TRUE`.

9.  Lines up numbers padded by
    [`align_chr()`](https://wjschne.github.io/apa7/reference/align_chr.md)
    on their decimal points.

10. Draws a rule above the rows named in a `"rule_above"` attribute of
    the data (set by functions such as
    [`apa_loadings()`](https://wjschne.github.io/apa7/reference/apa_loadings.md)
    and
    [`apa_chisq()`](https://wjschne.github.io/apa7/reference/apa_chisq.md)).

11. Adds the note, if `note` is given. It is added last so that a long
    note does not widen the columns.

For a step-by-step version of this process, see the article [Building
Tables by Hand](https://wjschne.github.io/apa7/articles/summarize.html).

## References

American Psychological Association. (2020). *Publication manual of the
American Psychological Association* (7th ed.).
[doi:10.1037/0000165-000](https://doi.org/10.1037/0000165-000)

## Examples

``` r
# Means and SDs, with a heading row for each transmission type
d <- mtcars
d$am <- factor(d$am, levels = 0:1, labels = c("Automatic", "Manual"))
d[, c("am", "mpg", "hp", "wt")] |>
  tidyr::pivot_longer(-am, names_to = "Variable") |>
  dplyr::summarise(Mean = mean(value), SD = sd(value), .by = c(am, Variable)) |>
  dplyr::arrange(am) |>
  apa_flextable(row_title_column = am, row_title_align = "center")


.cl-d8bad469{}.cl-fd7caf76{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f148d26d{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-7a9068f9{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-8dbfbcff{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-45e32d07{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:60.1pt;padding-right:57.1pt;line-height: 1.15;background-color:transparent;}.cl-dfedf37f{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:51.9pt;padding-right:48.9pt;line-height: 1.15;background-color:transparent;}.cl-421a1a9d{width:2.558in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-76e4e006{width:2.127in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e169e5ca{width:1.816in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-89f9387b{width:2.558in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-8ce250dc{width:2.127in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e11f10e9{width:1.816in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-61f27041{width:2.558in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e70dd598{width:2.127in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-205f9145{width:1.816in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a97e4157{width:2.558in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(51, 51, 51, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6d6edc22{width:2.127in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(51, 51, 51, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7a246159{width:1.816in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(51, 51, 51, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-09686b73{width:2.558in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(51, 51, 51, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c47bae75{width:2.127in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(51, 51, 51, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5fe6f585{width:1.816in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(51, 51, 51, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-52264a18{width:2.558in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-acf0b881{width:2.127in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-3f9a815e{width:1.816in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Variable
```
