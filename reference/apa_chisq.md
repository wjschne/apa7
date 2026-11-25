# Contingency table with a test of independence

Makes an APA-style contingency table of two categorical variables: a row
for each level of `variable` and a column (or a pair of columns, with
percentages) for each level of `by`, with the counts in each cell. The
variables are tested for independence, and
[`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
writes the note, with the result of the test and its effect size.

## Usage

``` r
apa_chisq(
  data,
  variable = NULL,
  by = NULL,
  percent = c("column", "row", "total", "none"),
  total = TRUE,
  test = c("chisq", "fisher", "none"),
  correct = TRUE,
  effect_size = c("auto", "cramers_v", "phi", "none"),
  percent_digits = 1,
  labels = NULL,
  format_variable_names = TRUE,
  ...
)
```

## Arguments

- data:

  data frame

- variable:

  variable whose levels are the rows of the table, unquoted or quoted
  (e.g., `smoking` or `"smoking"`)

- by:

  variable whose levels are the columns of the table, unquoted or
  quoted. If `data` has exactly two columns, `variable` and `by` can be
  left out: the first column is then `by`, and the second is `variable`
  (as in earlier versions of apa7).

- percent:

  percentages shown next to the counts: `"column"` (default) for
  percentages of each column's total, `"row"` for percentages of each
  row's total, `"total"` for percentages of the whole sample, or
  `"none"` for counts only

- total:

  if `TRUE` (default), a "Total" row and column are added

- test:

  `"chisq"` (default) for the chi-square test of independence,
  `"fisher"` for Fisher's exact test, or `"none"` for no test

- correct:

  if `TRUE` (default), the chi-square test of a 2 × 2 table uses Yates's
  continuity correction

- effect_size:

  `"auto"` (default) for \\\phi\\ in 2 × 2 tables and Cramér's *V*
  otherwise, `"cramers_v"`, `"phi"` (2 × 2 tables only), or `"none"`

- percent_digits:

  number of decimal places of percentages

- labels:

  named character vector of labels for `variable` and `by` (e.g.,
  `c(smoke = "Smoking")`). Variables without a label are named by their
  formatted names.

- format_variable_names:

  format the names of variables that have no label with the `Variable`
  column format

- ...:

  deprecated arguments of earlier versions of `apa_chisq()` (`note`,
  `suppress_warnings`, and the styling arguments of
  [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)).
  If any is given, a flextable is returned, as before, with a warning.

## Value

`apa7_tbl` (tibble)

## Details

Like the other apa7 tables, the result is a tibble that is displayed as
an APA-style table in Quarto and R Markdown documents (see
[apa7_tbl](https://wjschne.github.io/apa7/reference/apa7_tbl.md)). Pass
it to
[`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
to change its appearance, or use `apa_flextable(note = TRUE)` to put the
note below it.

Percentages are of each column's total (`percent = "column"`, the
default), so each group of `by` can be described on its own; of each
row's total (`"row"`); or of the whole sample (`"total"`). With
`total = TRUE` (default), a "Total" row and column give the marginal
counts.

The test is the chi-square test of independence
([`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html)) or
Fisher's exact test
([`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html)). As
in [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html), 2 ×
2 tables use Yates's continuity correction unless `correct = FALSE`.
When an expected count is below 5, the chi-square approximation can be
poor, and the note says so. The effect size is \\\phi\\ for 2 × 2 tables
and Cramér's *V* otherwise (without the small-sample adjustment), from
the effectsize package.

Rows with a missing value of either variable are left out, and the note
gives their number.

## Examples

``` r
d <- data.frame(
  Sex = factor(psych::sat.act$gender, levels = 1:2, labels = c("Men", "Women")),
  Education = factor(psych::sat.act$education)
)
tbl <- apa_chisq(d, Education, by = Sex)
tbl
#> # A tibble: 7 × 7
#>   Education Men_n `Men_%` Women_n `Women_%` Total_n `Total_%`
#>   <chr>     <int> <chr>     <int> <chr>       <int> <chr>    
#> 1 0            27  10.9        30   6.6          57   8.1    
#> 2 1            20   8.1        25   5.5          45   6.4    
#> 3 2            23   9.3        21   4.6          44   6.3    
#> 4 3            80  32.4       195  43.0         275  39.3    
#> 5 4            51  20.6        87  19.2         138  19.7    
#> 6 5            46  18.6        95  21.0         141  20.1    
#> 7 Total       247 100.0       453 100.0         700 100.0    
apa_note(tbl)
#> [1] "Values are counts (*n*) and percentages of each column (each group of Sex). Chi-square test of independence: *&chi;*^2^(5, *N* = 700) = 16.09, *p* = .007, Cramér's *V* = .15."

# Percentages of each row, and no totals
apa_chisq(d, Education, by = Sex, percent = "row", total = FALSE)
#> # A tibble: 6 × 5
#>   Education Men_n `Men_%` Women_n `Women_%`
#>   <chr>     <int> <chr>     <int> <chr>    
#> 1 0            27 47.4         30 52.6     
#> 2 1            20 44.4         25 55.6     
#> 3 2            23 52.3         21 47.7     
#> 4 3            80 29.1        195 70.9     
#> 5 4            51 37.0         87 63.0     
#> 6 5            46 32.6         95 67.4     

# Fisher's exact test instead of the chi-square test
apa_chisq(d[d$Education %in% c("0", "5"), ], Education, by = Sex, test = "fisher")
#> # A tibble: 3 × 7
#>   Education Men_n `Men_%` Women_n `Women_%` Total_n `Total_%`
#>   <chr>     <int> <chr>     <int> <chr>       <int> <chr>    
#> 1 0            27  37.0        30  24.0          57  28.8    
#> 2 5            46  63.0        95  76.0         141  71.2    
#> 3 Total        73 100.0       125 100.0         198 100.0    
```
