# Set defaults for apa7 package

Sets options that apply to all apa7 tables. Settings that are not given
are left as they are.

## Usage

``` r
apa7_defaults(
  accuracy = NULL,
  font_family = NULL,
  font_size = NULL,
  line_spacing = NULL,
  intercept_text = NULL,
  column_formats = NULL,
  number_formatter = NULL,
  trim_leading_zero = NULL,
  width_aware_padding = NULL,
  page_width = NULL,
  landscape_width = NULL,
  column_width = NULL,
  table_width = NULL,
  print = NULL,
  citations = NULL,
  reset = FALSE
)
```

## Arguments

- accuracy:

  rounding accuracy of numbers in tables (default: .01, for two
  decimals). Model tables such as
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  and
  [`apa_anova()`](https://wjschne.github.io/apa7/reference/apa_anova.md)
  use it. Descriptive tables such as
  [`apa_descriptives()`](https://wjschne.github.io/apa7/reference/apa_descriptives.md),
  [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md),
  and
  [`apa_loadings()`](https://wjschne.github.io/apa7/reference/apa_loadings.md)
  have their own `digits` argument instead (`digits = 2` is the same as
  `accuracy = .01`).

- font_family:

  font family

- font_size:

  font size of tables and their notes, in points (default: 12)

- line_spacing:

  spacing between the lines of tables' headers and bodies: 2 (default)
  for double spacing, 1.5, or 1 for single spacing. APA style allows any
  of these in tables. Table notes are always single-spaced.

- intercept_text:

  label of the intercept row in regression tables (default:
  `"Constant"`)

- column_formats:

  a
  [`column_formats()`](https://wjschne.github.io/apa7/reference/column_formats.md)
  object: the headers, formatters, and note definitions of table columns

- number_formatter:

  function that formats numbers that can exceed 1 in absolute value
  (e.g., coefficients, standard errors, and test statistics). It takes a
  numeric vector and `accuracy` and returns a character vector. By
  default,
  [`align_chr()`](https://wjschne.github.io/apa7/reference/align_chr.md)
  with the `accuracy` setting.

- trim_leading_zero:

  function that formats numbers that cannot exceed 1 in absolute value
  (e.g., correlations, standardized coefficients, and *R*^2^), which APA
  style writes without a leading zero. By default,
  [`align_chr()`](https://wjschne.github.io/apa7/reference/align_chr.md)
  with `trim_leading_zeros = TRUE`. Note that this is a function, unlike
  the `TRUE`/`FALSE` `trim_leading_zeros` argument of
  [`align_chr()`](https://wjschne.github.io/apa7/reference/align_chr.md).

- width_aware_padding:

  if `TRUE` (default),
  [`align_chr()`](https://wjschne.github.io/apa7/reference/align_chr.md)
  pads text to equal rendered width in `font_family` (when the font is
  installed). If `FALSE`, text is padded with one figure space per
  missing character.

- page_width:

  width of the text area of a portrait page, which tables fill with
  `table_width = "page"` (or `1`, by default): a number (in inches) or
  the name of a paper size, whose width less APA's 1-inch margins on
  each side is used (e.g., `"us-letter"` gives 6.5 inches, the default,
  and `"a4"` gives 6.27 inches). If not set, the width comes from the
  paper and margins of the document being rendered, if any (see
  Details). Paper sizes are those of Typst's `page` function (e.g.,
  `"a4"`, `"a5"`, `"us-letter"`, `"us-legal"`, `"jis-b5"`); see
  <https://typst.app/docs/reference/layout/page/#parameters-paper>.

- landscape_width:

  width of the text area of a landscape page, in inches, which tables
  fill with `table_width = "landscape"`. If `NA` (default), the height
  of the paper named by `page_width` less 1-inch margins (9 inches for
  `"us-letter"` and 9.69 inches for `"a4"`). When `page_width` is a
  number, the paper is taken to have the proportions of letter-size
  paper.

- column_width:

  width of one column of a two-column page, in inches, which tables fill
  with `table_width = "column"`. If `NA` (default), the width of a
  column in apaquarto's two-column journal mode (`documentmode: jou`):
  the paper width less 0.75-inch margins, less a gutter of 4% between
  the columns, divided by 2 (3.36 inches for `"us-letter"` and 3.25
  inches for `"a4"`). When `page_width` is a number, the paper is taken
  to be that number plus 1-inch margins.

- table_width:

  default width of tables, in any of the forms that the `table_width`
  argument of
  [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
  takes: `1` (default) for the full width of a portrait page, a
  proportion of it (e.g., `.8`), `"page"`, `"landscape"`, `"column"`, or
  a named number (e.g., `c(column = 1)`). In a two-column document,
  `table_width = "column"` makes tables as wide as a column, and a
  proportion given to a table function is then a proportion of the
  column. If not set, tables in a document in apaquarto's journal mode
  (`documentmode: jou`) are as wide as a column.

- print:

  how tables made by apa7 functions (see
  [apa7_tbl](https://wjschne.github.io/apa7/reference/apa7_tbl.md)) are
  displayed: `"auto"` (default) as tables in Quarto and R Markdown
  documents and as tibbles in the console, `"flextable"` as tables
  everywhere, or `"tibble"` as tibbles everywhere

- citations:

  how articles cited in table notes are written: `"auto"` (default),
  `"keys"`, or `"text"`. See
  [`apa7_write_bib()`](https://wjschne.github.io/apa7/reference/apa7_write_bib.md).

- reset:

  if `TRUE`, reset all defaults (except as specified)

## Value

the previous values of the settings that were changed (all of them with
`reset = TRUE`), invisibly, as a list that restores them when passed
back to `apa7_defaults()` (e.g., with
[`do.call()`](https://rdrr.io/r/base/do.call.html))

## Details

Each setting except `column_formats`, `number_formatter`, and
`trim_leading_zero` is stored as an R option (e.g.,
`apa7_defaults(page_width = "a4")` sets
`options(apa7.page_width = "a4")`), so it can also be set with
[`options()`](https://rdrr.io/r/base/options.html), including in an
`.Rprofile` file that runs at the start of every R session, before apa7
is loaded:

|                       |                            |                          |
|-----------------------|----------------------------|--------------------------|
| Argument              | Option                     | Default                  |
| `accuracy`            | `apa7.accuracy`            | `.01`                    |
| `font_family`         | `apa7.font_family`         | `"Times New Roman"`      |
| `font_size`           | `apa7.font_size`           | `12`                     |
| `line_spacing`        | `apa7.line_spacing`        | `2`                      |
| `intercept_text`      | `apa7.intercept_text`      | `"Constant"`             |
| `width_aware_padding` | `apa7.width_aware_padding` | `TRUE`                   |
| `page_width`          | `apa7.page_width`          | `6.5`                    |
| `landscape_width`     | `apa7.landscape_width`     | `NA` (from `page_width`) |
| `column_width`        | `apa7.column_width`        | `NA` (from `page_width`) |
| `table_width`         | `apa7.table_width`         | `1`                      |
| `print`               | `apa7.print`               | `"auto"`                 |
| `citations`           | `apa7.citations`           | `"auto"`                 |

`column_formats`, `number_formatter`, and `trim_leading_zero` are
objects that need apa7 to be loaded, so they are set only with
`apa7_defaults()` and last for the R session.

When `page_width` and `table_width` are not set, a Quarto or R Markdown
document being rendered supplies them: its `papersize` sets the paper,
and apaquarto's `documentmode` sets the margins (0.75 inches on each
side in journal mode, `jou`, and 1.5 inches on the left in thesis mode).
In journal mode, which sets the text in two columns, tables are as wide
as a column by default. A setting made with `apa7_defaults()` or
[`options()`](https://rdrr.io/r/base/options.html) overrides the
document.

The default column formats follow `accuracy`: changing it (with
`apa7_defaults()` or the `apa7.accuracy` option) changes the number of
decimals in tables. Setting `column_formats` without `accuracy` makes
the formats' own accuracy the `accuracy` setting.

## Examples

``` r
old <- apa7_defaults(accuracy = .001)

# Tables as wide as the text on A4 paper with 1-inch margins
apa7_defaults(page_width = "a4")

# A two-column document: tables are as wide as a column unless a table
# asks for another width (e.g., table_width = "page" in apa_flextable())
apa7_defaults(table_width = "column")

# Restore the earlier accuracy
do.call(apa7_defaults, old)

# Reset to package defaults
apa7_defaults(reset = TRUE)
```
