# APA-formatted correlation table

APA-formatted correlation table

## Usage

``` r
apa_cor(
  data,
  note = NULL,
  alpha = c(0.05, 0.01, 0.001),
  digits = 2,
  bold_significant = FALSE,
  star_significant = TRUE,
  significance_note = TRUE,
  output = c("flextable", "tibble"),
  font_family = NULL,
  font_size = NULL,
  text_color = "black",
  border_color = "black",
  border_width = 0.5,
  line_spacing = NULL,
  table_width = NULL,
  keep_empty_star_columns = TRUE,
  summary_functions = list(M = mean, SD = stats::sd),
  column_formats = NULL,
  method = "pearson",
  p_adjust = "none",
  partial = FALSE,
  ...,
  p_value
)
```

## Arguments

- data:

  data.frame or tibble with variables to be correlated, or the output of
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
  (see Details)

- note:

  note placed below the table, replacing the automatic note (for
  `output = "flextable"`)

- alpha:

  *p*-value thresholds for significance stars, from largest to smallest
  (default: `c(.05, .01, .001)`, for one, two, and three stars). With
  `bold_significant = TRUE`, only the first is used.

- digits:

  number of decimal places of the correlations (unaffected by
  `apa7_defaults(accuracy = )`)

- bold_significant:

  if `TRUE`, correlations with *p* below the first `alpha` are bolded
  instead of starred. If `FALSE` (default), they are not.

- star_significant:

  if `TRUE` (default), significant correlations get stars (one for each
  `alpha` they are below). Ignored when `bold_significant = TRUE`. Model
  tables such as
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  do the same with their `starred` and `bolded` arguments.

- significance_note:

  if `TRUE` (default), the note explains the stars (or the bolding) when
  any correlation is significant

- output:

  `"flextable"` (default) for a formatted table, or `"tibble"` for a
  tibble of the formatted values (without a note)

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

- keep_empty_star_columns:

  if `TRUE` (default), every correlation column keeps its star column,
  even when no correlation in it is significant, so that the columns
  have the same width. If `FALSE`, empty star columns are removed.

- summary_functions:

  Any named list of functions that summarize data columns (e.g.,
  `list(Mean = mean, SD = sd)`). Can also be a character vector of
  function names (e.g., `c("n", "M", "SD")`). Functions available in a
  character vector: IQR, Interquartile Range, Kurtosis, MAD, Median
  Absolute Deviation, M, Mean, Med, Median, n, N, Quantile, Range, SD,
  Skewness, Var, Variance

- column_formats:

  [`column_formats()`](https://wjschne.github.io/apa7/reference/column_formats.md)
  object. If `NULL` (default), the default column formats are used.

- method:

  kind of correlation (e.g., `"pearson"`, `"spearman"`, `"kendall"`,
  `"biweight"`, or `"percentage"`), passed to
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html).
  Not used for
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
  output.

- p_adjust:

  method of adjusting the *p*-values (and so the stars) for multiple
  comparisons (e.g., `"holm"`, `"bonferroni"`, or `"fdr"`). Unlike
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html),
  the default is `"none"`. Not used for
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
  output.

- partial:

  if `TRUE`, partial correlations, each controlling for all other
  variables in the table. Not used for
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
  output.

- ...:

  other arguments passed to
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
  (e.g., `robust = TRUE`, `winsorize = .2`, or `multilevel = TRUE`). Not
  used for
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
  output.

- p_value:

  deprecated; use `alpha`

## Value

flextable::flextable

## Details

`apa_cor()` computes the correlations with
[`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html).
By default, they are Pearson correlations of all pairwise complete
observations, and the stars are for unadjusted *p*-values. Use `method`,
`p_adjust`, `partial`, and the other arguments of
[`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
for other kinds of correlations. The column names of `data` are the
variable names in the table, and they can contain spaces and symbols
(e.g., `"Reading comprehension"`) for every kind of correlation:
`apa_cor()` gives the variables internal names while the correlations
are computed. Logical columns are correlated as 0 and 1.

The output of
[`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
can also be passed as `data`. The table shows its correlations and
*p*-values, and the summary columns are computed from the data that
[`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
keeps with its results. Note that
[`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
uses Holm's method by default, so the stars are for adjusted *p*-values
unless `p_adjust = "none"`, and that its partial and multilevel
correlations do not accept variable names with spaces. For those, use
`apa_cor(data, partial = TRUE)` instead.

The note says what kind of correlations are shown (unless they are
ordinary Pearson correlations) and how the *p*-values were adjusted for
multiple comparisons. Correlations computed by group, between two sets
of variables, or with Bayesian methods are not yet supported.

In apa7 0.2.0 and earlier, `apa_cor()` computed the correlations with
[`psych::corTest()`](https://rdrr.io/pkg/psych/man/corr.test.html). Its
`adjust` argument is still accepted as `p_adjust`, with a warning, and
`use = "complete"` removes incomplete rows, with a warning.

## References

Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery
rate: A practical and powerful approach to multiple testing. *Journal of
the Royal Statistical Society: Series B (Methodological), 57*(1),
289–300.
[doi:10.1111/j.2517-6161.1995.tb02031.x](https://doi.org/10.1111/j.2517-6161.1995.tb02031.x)

Holm, S. (1979). A simple sequentially rejective multiple test
procedure. *Scandinavian Journal of Statistics, 6*(2), 65–70.

## Examples

``` r
apa_cor(mtcars[, c("mpg", "am", "gear", "carb")])


.cl-d8bad469{}.cl-fd7caf76{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f148d26d{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-733a321e{font-family:'Times New Roman';font-size:7.8pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;vertical-align:4.08pt;}.cl-9c3421d6{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:0;line-height: 1.15;background-color:transparent;}.cl-f6483eb1{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:0;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-7a9068f9{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-bd61faf7{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:0;line-height: 1.15;background-color:transparent;}.cl-270f6c08{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:0;padding-right:0;line-height: 1.15;background-color:transparent;}.cl-0f1762d3{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1.15;background-color:transparent;}.cl-71516dcf{width:0.278in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e3426e03{width:0.788in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6d4752d4{width:0.863in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ec32e03d{width:0.715in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-d35498d3{width:0.635in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c1f27f4d{width:0.329in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-72dcc9d5{width:0.278in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-13c12c56{width:0.788in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5280e7a6{width:0.863in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-38df1fac{width:0.715in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-41788bdf{width:0.635in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-259316c1{width:0.329in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9a234ba{width:0.278in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-66e77ccc{width:0.788in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-dfcbe20d{width:0.863in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-2dc291a4{width:0.715in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-2a4e5e6b{width:0.635in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f3c36127{width:0.329in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-efedbba3{width:0.278in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c074472a{width:0.788in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-92c85c52{width:0.863in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e2cbf9{width:0.715in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-47404178{width:0.635in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-0ee38168{width:0.329in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Variable
```
