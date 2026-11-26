# Reliability and item analysis table

Makes an APA-style table of the reliability of one or more scales: for
each item, its mean, standard deviation, corrected item-total
correlation, and Cronbach's \\\alpha\\ if the item were deleted,
followed for each scale by the scale score's mean and standard
deviation, Cronbach's \\\alpha\\ with its confidence interval,
McDonald's \\\omega\_{\text{total}}\\, and the mean inter-item
correlation.

## Usage

``` r
apa_reliability(
  data,
  scales = NULL,
  reverse = NULL,
  item_range = NULL,
  level = c("items", "scales"),
  omega = TRUE,
  ci = 0.95,
  score = c("mean", "sum"),
  flag = NULL,
  format_variable_names = TRUE,
  digits = 2
)
```

## Arguments

- data:

  data frame of item responses

- scales:

  named list of the items of each scale, given as unquoted or quoted
  names or tidyselect helpers (e.g.,
  `list(Agreeableness = c(A1, A2, A3), Conscientiousness = starts_with("C"))`).
  A named list of character vectors stored in a variable works too. If
  `NULL` (default), all columns of `data` form one scale.

- reverse:

  items to reverse-score before the analysis, selected like the items in
  `scales` (e.g., `c(A1, C4, C5)`). Items are reversed within their
  scale's range of responses (`item_range`), and they are marked "(R)"
  in the table.

- item_range:

  lowest and highest possible responses, used to reverse items. If
  `NULL` (default), the lowest and highest responses observed in each
  scale are used.

- level:

  `"items"` (default) for item rows followed by each scale's statistics,
  or `"scales"` for one row per scale.

- omega:

  if `TRUE` (default), McDonald's \\\omega\_{\text{total}}\\ is shown

- ci:

  confidence level of the interval of \\\alpha\\

- score:

  `"mean"` (default) or `"sum"`: how scale scores are computed

- flag:

  `NULL` (default) or a number: item-total correlations below it, and
  \\\alpha\\-if-deleted values above the scale's \\\alpha\\, are bolded
  (e.g., `flag = .30`), and the note says so. `TRUE` is the same as
  `.30`. This works like `flag` in
  [`apa_collinearity()`](https://wjschne.github.io/apa7/reference/apa_collinearity.md).

- format_variable_names:

  if `TRUE` (default), item and scale names are lightly formatted:
  underscores become spaces, and the first letter is capitalized (item
  codes such as "A1" stay intact). Set to `FALSE` to keep names exactly
  as they are.

- digits:

  number of decimal places (unaffected by `apa7_defaults(accuracy = )`)

## Value

`apa7_tbl` (tibble)

## Details

The corrected item-total correlation (*r*~it~) is the item's correlation
with the sum of the other items in its scale. Items with low values
(often, below .30) discriminate poorly. \\\alpha\\ if deleted is the
scale's \\\alpha\\ without the item; values above the scale's \\\alpha\\
mean that the item lowers reliability.

Cronbach's \\\alpha\\, the item statistics, and the mean inter-item
correlation come from
[`psych::alpha()`](https://rdrr.io/pkg/psych/man/alpha.html). The
confidence interval of \\\alpha\\ is Feldt's. \\\omega\_{\text{total}}\\
is computed from a one-factor model
([`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html), minimum
residual): \\(\sum\lambda)^2 / ((\sum\lambda)^2 + \sum\psi)\\.

Each scale uses the respondents who answered all of its items, and the
note gives their number. Scale scores are the means of the items (or
their sums, with `score = "sum"`).

When all items of a scale are scored 0 and 1 (e.g., incorrect and
correct answers), *M* is the item's difficulty (the proportion of 1s),
*r*~it~ is the corrected point-biserial correlation, and \\\alpha\\
equals KR-20. The note says so.

## References

Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987). Statistical
inference for coefficient alpha. *Applied Psychological Measurement,
11*(1), 93–103.
[doi:10.1177/014662168701100107](https://doi.org/10.1177/014662168701100107)

McDonald, R. P. (1999). *Test theory: A unified treatment*. Lawrence
Erlbaum.
[doi:10.4324/9781410601087](https://doi.org/10.4324/9781410601087)

## Examples

``` r
d <- psych::bfi
apa_reliability(
  d,
  scales = list(
    Agreeableness = paste0("A", 1:5),
    Conscientiousness = paste0("C", 1:5)
  ),
  reverse = c("A1", "C4", "C5"),
  item_range = c(1, 6)
)
#> # A tibble: 20 × 5
#>    Item                  `*M*`            `*SD*` `*r*~it~` `&alpha; if deleted`
#>    <chr>                 <chr>            <chr>  <chr>     <chr>               
#>  1 Agreeableness         ""               ""     ""        ""                  
#>  2  A1 (R)               "4.59"           "1.40" ".31"     ".72"               
#>  3  A2                   "4.80"           "1.18" ".56"     ".62"               
#>  4  A3                   "4.60"           "1.30" ".59"     ".60"               
#>  5  A4                   "4.68"           "1.49" ".39"     ".69"               
#>  6  A5                   "4.55"           "1.26" ".49"     ".64"               
#>  7  Scale mean (*k* = 5) "4.64"           "0.90" ""        ""                  
#>  8  &alpha; [95% CI]     ".70 [.69, .72]" ""     ""        ""                  
#>  9  *&omega;*~total~     ".72"            ""     ""        ""                  
#> 10  Mean inter-item *r*  ".33"            ""     ""        ""                  
#> 11 Conscientiousness     ""               ""     ""        ""                  
#> 12  C1                   "4.51"           "1.24" ".46"     ".70"               
#> 13  C2                   "4.36"           "1.32" ".51"     ".68"               
#> 14  C3                   "4.30"           "1.29" ".47"     ".69"               
#> 15  C4 (R)               "4.45"           "1.37" ".56"     ".66"               
#> 16  C5 (R)               "3.69"           "1.63" ".48"     ".69"               
#> 17  Scale mean (*k* = 5) "4.26"           "0.95" ""        ""                  
#> 18  &alpha; [95% CI]     ".73 [.71, .75]" ""     ""        ""                  
#> 19  *&omega;*~total~     ".73"            ""     ""        ""                  
#> 20  Mean inter-item *r*  ".35"            ""     ""        ""                  

# One row per scale, with items selected by tidyselect helpers
apa_reliability(
  d,
  scales = list(
    Agreeableness = A1:A5,
    Conscientiousness = starts_with("C")
  ),
  reverse = c(A1, C4, C5),
  item_range = c(1, 6),
  level = "scales"
)
#> # A tibble: 2 × 8
#>   Scale      `*k*` `*M*` `*SD*` `&alpha;` `95% CI` `*&omega;*~total~` `Mean *r*`
#>   <chr>      <chr> <chr> <chr>  <chr>     <chr>    <chr>              <chr>     
#> 1 Agreeable… 5     4.64  0.90   .70       [.69, .… .72                .33       
#> 2 Conscient… 5     4.26  0.95   .73       [.71, .… .73                .35       
```
