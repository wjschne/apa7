# APA-formatted correlation table

APA-formatted correlation table

## Usage

``` r
apa_cor(
  data,
  alpha = c(0.05, 0.01, 0.001),
  digits = 2,
  bold_significant = FALSE,
  star_significant = TRUE,
  significance_note = TRUE,
  keep_empty_star_columns = TRUE,
  summary_functions = list(M = mean, SD = stats::sd),
  column_formats = NULL,
  method = "pearson",
  p_adjust = "none",
  partial = FALSE,
  ...,
  output,
  note,
  font_family,
  font_size,
  text_color,
  border_color,
  border_width,
  line_spacing,
  table_width,
  p_value
)
```

## Arguments

- data:

  data.frame or tibble with variables to be correlated, or the output of
  [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html)
  (see Details)

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
  object that formats the summary columns (e.g., *M* and *SD*). If
  `NULL` (default), the column formats set with
  [`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md).

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

- output, note, font_family, font_size, text_color, border_color,
  border_width, line_spacing, table_width:

  deprecated. If any is given, a flextable is returned as in apa7 0.2.0,
  with a warning (except `output = "tibble"`, which returns the table).
  Pass the table to
  [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
  instead, which takes the same styling arguments, with `note = TRUE`
  for the note or a character string for a note of your own.

- p_value:

  deprecated; use `alpha`

## Value

`apa7_tbl` (tibble)

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

`apa_cor()` returns an `apa7_tbl`, which is displayed as an APA-style
table in documents (see
[apa7_tbl](https://wjschne.github.io/apa7/reference/apa7_tbl.md)).
[`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
writes its note: what kind of correlations are shown (unless they are
ordinary Pearson correlations), how the *p*-values were adjusted for
multiple comparisons, and what the stars or bold values mean. With
apaquarto, use the `apa-note` chunk option (e.g.,
`#| apa-note: !expr apa7::apa_note(tbl)`). Otherwise, use
`apa_flextable(tbl, note = TRUE)`. Correlations computed by group,
between two sets of variables, or with Bayesian methods are not yet
supported.

In apa7 0.2.0 and earlier, `apa_cor()` computed the correlations with
[`psych::corTest()`](https://rdrr.io/pkg/psych/man/corr.test.html). Its
`adjust` argument is still accepted as `p_adjust`, with a warning, and
`use = "complete"` removes incomplete rows, with a warning. Until apa7
0.3.0, `apa_cor()` returned a flextable with the note below it, and it
took the arguments that style a flextable. These arguments still work,
with a warning.

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
tbl <- apa_cor(mtcars[, c("mpg", "am", "gear", "carb")])
tbl
#> # A tibble: 4 × 12
#>   apa7listcolumn Variable `*M*` `*SD*` `1`   mpgapa7starcolumn `2`  
#>   <chr>          <chr>    <chr> <chr>  <chr> <chr>             <chr>
#> 1 1.             mpg      20.09 6.03   —     ""                NA   
#> 2 2.             am        0.41 0.50   .60   "^\\*\\*\\*^"     —    
#> 3 3.             gear      3.69 0.74   .48   "^\\*\\*^"        .79  
#> 4 4.             carb      2.81 1.62   −.55  "^\\*\\*^"        .06  
#> # ℹ 5 more variables: amapa7starcolumn <chr>, `3` <chr>,
#> #   gearapa7starcolumn <chr>, `4` <chr>, carbapa7starcolumn <chr>
apa_note(tbl)
#> [1] "^\\*\\* ^*p*&nbsp;≤&nbsp;.01. ^\\*\\*\\* ^*p*&nbsp;≤&nbsp;.001."

# A flextable with the note below it, for documents without apaquarto
apa_flextable(tbl, note = TRUE)


.cl-d8bad469{}.cl-fd7caf76{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-f148d26d{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-733a321e{font-family:'Times New Roman';font-size:7.8pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;vertical-align:4.08pt;}.cl-9c3421d6{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:0;line-height: 1.15;background-color:transparent;}.cl-f6483eb1{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:0;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-7a9068f9{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-bd61faf7{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:0;line-height: 1.15;background-color:transparent;}.cl-270f6c08{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:0;padding-right:0;line-height: 1.15;background-color:transparent;}.cl-a68d07e4{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:0;padding-top:2pt;padding-left:3pt;padding-right:3pt;line-height: 1.15;background-color:transparent;}.cl-71516dcf{width:0.278in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-b01b2211{width:0.778in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7d12c5a3{width:0.87in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ef019cd1{width:0.721in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-d35d298f{width:0.64in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-272baf68{width:0.323in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-72dcc9d5{width:0.278in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fce5a274{width:0.778in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-17677429{width:0.87in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-2fef6460{width:0.721in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-377a8a3d{width:0.64in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ad6b268c{width:0.323in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a9a234ba{width:0.278in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-265bb0fb{width:0.778in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-71ba44e4{width:0.87in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-13fe707e{width:0.721in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4654321f{width:0.64in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-c9cafb64{width:0.323in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-1db935f8{width:0.278in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-9ac4025a{width:0.778in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-188397f1{width:0.87in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-a06a4e95{width:0.721in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-91ab0b6a{width:0.64in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-7a533be8{width:0.323in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Variable
```
