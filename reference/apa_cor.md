# APA-formatted correlation table

APA-formatted correlation table

## Usage

``` r
apa_cor(
  data,
  note = NULL,
  p_value = c(0.05, 0.01, 0.001),
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
  table_width = 1,
  keep_empty_star_columns = TRUE,
  summary_functions = list(M = mean, SD = stats::sd),
  column_formats = NULL,
  ...
)
```

## Arguments

- data:

  data.frame or tibble with variables to be correlated, or the output of
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
  (see Details)

- note:

  Custom note to appear below table. (Overrides automatic note.)

- p_value:

  p-value needed to be flagged as significant

- digits:

  Number of digits for rounding

- bold_significant:

  bold significant correlations

- star_significant:

  start significant correlations

- significance_note:

  If TRUE, place note at bottom of table that significant correlations
  are bolded.

- output:

  output type. Can be "flextable" or "tibble"

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

- table_width:

  proportion of the width of the page that the table occupies, from 0 to
  1: 1 (default) is the full width, and .5 is half the width. The column
  widths are scaled to fill this proportion of the width of the text
  area, which is 6.5 inches (letter-size paper with 1-inch margins)
  unless changed with `apa7_defaults(page_width = )`. With 0, the column
  widths are left as they are. In apa7 0.2.0 and earlier, `table_width`
  was a width in inches; values greater than 1 are still read as inches,
  with a warning.

- keep_empty_star_columns:

  Keep remove empty star columns (Default: `TRUE`)

- summary_functions:

  Any named list of functions that summarize data columns (e.g.,
  `list(Mean = mean, SD = sd)`). Can also be a character vector of
  function names (e.g., `c("n", "M", "SD")`). Functions available in a
  character vector: IQR, Interquartile Range, Kurtosis, MAD, Median
  Absolute Deviation, M, Mean, Med, Median, n, N, Quantile, Range, SD,
  Skewness, Var, Variance

- column_formats:

  column_formats object

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  parameters passed to psych::corTest (not used for
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
  output)

## Value

flextable::flextable

## Details

By default, `apa_cor()` computes Pearson correlations with
[`psych::corTest()`](https://rdrr.io/pkg/psych/man/corr.test.html), and
the stars are for unadjusted *p*-values.

For other kinds of correlations (e.g., Spearman, Kendall, robust, or
partial correlations), compute them with
[`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
and pass the result as `data`. The table shows its correlations and
*p*-values, and the summary columns are computed from the data that
[`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
keeps with its results. The note says what kind of correlations are
shown (unless they are ordinary Pearson correlations) and how the
*p*-values were adjusted for multiple comparisons:
[`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
uses Holm's method by default, so the stars are for adjusted *p*-values
unless `p_adjust = "none"`. Correlations computed by group, between two
sets of variables, or with Bayesian methods are not yet supported.

## Examples

``` r
apa_cor(mtcars[, c("mpg", "am", "gear", "carb")])


.cl-d8bad469{}.cl-fd7caf76{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f148d26d{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-733a321e{font-family:'Times New Roman';font-size:7.8pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;vertical-align:4.08pt;}.cl-9c3421d6{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:0;line-height: 1.15;background-color:transparent;}.cl-f6483eb1{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:0;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-7a9068f9{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-bd61faf7{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:0;line-height: 1.15;background-color:transparent;}.cl-270f6c08{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:0;padding-right:0;line-height: 1.15;background-color:transparent;}.cl-0f1762d3{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1.15;background-color:transparent;}.cl-71516dcf{width:0.278in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e3426e03{width:0.788in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6d4752d4{width:0.863in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ec32e03d{width:0.715in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-d35498d3{width:0.635in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c1f27f4d{width:0.329in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-72dcc9d5{width:0.278in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-13c12c56{width:0.788in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5280e7a6{width:0.863in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-38df1fac{width:0.715in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-41788bdf{width:0.635in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-259316c1{width:0.329in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9a234ba{width:0.278in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-66e77ccc{width:0.788in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-dfcbe20d{width:0.863in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-2dc291a4{width:0.715in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-2a4e5e6b{width:0.635in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-f3c36127{width:0.329in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-efedbba3{width:0.278in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c074472a{width:0.788in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-92c85c52{width:0.863in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-55e2cbf9{width:0.715in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-47404178{width:0.635in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-0ee38168{width:0.329in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Variable
```
