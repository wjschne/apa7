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
  print = NULL,
  citations = NULL,
  reset = FALSE
)
```

## Arguments

- accuracy:

  rounding accuracy of numbers in tables (default: .01, for two
  decimals)

- font_family:

  font family

- font_size:

  font size of tables and their notes, in points (default: 12)

- line_spacing:

  spacing between the lines of tables' headers and bodies: 2 (default)
  for double spacing, 1.5, or 1 for single spacing. APA style allows any
  of these in tables. Table notes are always single-spaced.

- intercept_text:

  what to call the intercept

- column_formats:

  column formatting functions

- number_formatter:

  default function to format numbers

- trim_leading_zero:

  default function to trim leading zeros from numbers

- width_aware_padding:

  if `TRUE` (default),
  [`align_chr()`](https://wjschne.github.io/apa7/reference/align_chr.md)
  pads text to equal rendered width in `font_family` (when the font is
  installed). If `FALSE`, text is padded with one figure space per
  missing character.

- page_width:

  width of the text area of the page, which tables fill when
  `table_width = 1` in
  [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
  and
  [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md): a
  number (in inches) or the name of a paper size, whose width less APA's
  1-inch margins on each side is used (e.g., `"us-letter"` gives 6.5
  inches, the default, and `"a4"` gives 6.27 inches). Paper sizes are
  those of Typst's `page` function (e.g., `"a4"`, `"a5"`, `"us-letter"`,
  `"us-legal"`, `"jis-b5"`); see
  <https://typst.app/docs/reference/layout/page/#parameters-paper>.

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

|                       |                            |                     |
|-----------------------|----------------------------|---------------------|
| Argument              | Option                     | Default             |
| `accuracy`            | `apa7.accuracy`            | `.01`               |
| `font_family`         | `apa7.font_family`         | `"Times New Roman"` |
| `font_size`           | `apa7.font_size`           | `12`                |
| `line_spacing`        | `apa7.line_spacing`        | `2`                 |
| `intercept_text`      | `apa7.intercept_text`      | `"Constant"`        |
| `width_aware_padding` | `apa7.width_aware_padding` | `TRUE`              |
| `page_width`          | `apa7.page_width`          | `6.5`               |
| `print`               | `apa7.print`               | `"auto"`            |
| `citations`           | `apa7.citations`           | `"auto"`            |

`column_formats`, `number_formatter`, and `trim_leading_zero` are
objects that need apa7 to be loaded, so they are set only with
`apa7_defaults()` and last for the R session.

The default column formats follow `accuracy`: changing it (with
`apa7_defaults()` or the `apa7.accuracy` option) changes the number of
decimals in tables. Setting `column_formats` without `accuracy` makes
the formats' own accuracy the `accuracy` setting.

## Examples

``` r
old <- apa7_defaults(accuracy = .001)

# Tables as wide as the text on A4 paper with 1-inch margins
apa7_defaults(page_width = "a4")

# Restore the earlier accuracy
do.call(apa7_defaults, old)

# Reset to package defaults
apa7_defaults(reset = TRUE)
```
