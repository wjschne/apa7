# Format hypothesis tests in APA style

Runs *t* tests, Wilcoxon tests, correlation tests, or chi-square tests
of independence on the variables in a data frame and makes a table with
one row per outcome variable: descriptive statistics, the test
statistic, its degrees of freedom, the *p*-value, and an effect size
with its confidence interval. Tests come from the stats package (e.g.,
[`stats::t.test()`](https://rdrr.io/r/stats/t.test.html)), and effect
sizes are computed from the data with the effectsize package (e.g.,
[`effectsize::cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.html)).

## Usage

``` r
apa_htest(
  data,
  formula,
  test = c("t", "wilcoxon", "cor", "chisq"),
  effect_size = NULL,
  ci = 0.95,
  var_equal = FALSE,
  mu = 0,
  method = c("pearson", "spearman", "kendall"),
  descriptives = TRUE,
  pair_labels = NULL,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  ...
)
```

## Arguments

- data:

  data frame, or test results already computed: an `htest` object from
  [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html),
  [`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html),
  [`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html), or
  [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html), or a
  list of such objects of the same kind (named lists label the rows).
  See **Test results**.

- formula:

  two-sided formula with outcome variables on the left and a grouping or
  predictor variable on the right. Several outcomes are joined with `+`
  (e.g., `mpg + hp ~ am`), and each gets its own row. For one-sample *t*
  tests and Wilcoxon signed-rank tests, use `1` on the right (e.g.,
  `mpg ~ 1`). For paired tests, put each pair of variables in
  [`Pair()`](https://rdrr.io/r/stats/Pair.html) with `1` on the right
  (e.g., `Pair(pre, post) ~ 1`, or
  `Pair(anx1, anx2) + Pair(dep1, dep2) ~ 1` for several pairs).

- test:

  type of test:

  - `"t"`: *t* test comparing the means of two groups (Welch's test
    unless `var_equal = TRUE`), a paired *t* test, or a one-sample *t*
    test comparing a mean with `mu`

  - `"wilcoxon"`: Wilcoxon rank-sum test comparing two groups, or a
    Wilcoxon signed-rank test of paired variables or of a variable
    compared with `mu`

  - `"cor"`: correlation test of each outcome with the variable on the
    right

  - `"chisq"`: chi-square test of independence of each outcome with the
    variable on the right

- effect_size:

  effect size to display. If `NULL` (default), the effect size is
  Cohen's *d* (`"cohens_d"`) for *t* tests, the rank-biserial
  correlation (`"rank_biserial"`) for Wilcoxon tests, and Cramér's *V*
  (`"cramers_v"`) for chi-square tests. *t* tests can also show Hedges'
  *g* (`"hedges_g"`), and chi-square tests can show \\\phi\\ (`"phi"`,
  for 2 \\\times\\ 2 tables) or Cohen's *w* (`"cohens_w"`). Use `"none"`
  for no effect size. Correlation tests show the correlation itself.

  For paired *t* tests, Cohen's *d* is *d*~av~ (`"d_av"`), which divides
  the mean difference by the average of the two variables' standard
  deviations, so it is comparable to *d* for two independent groups.
  Other choices are *d*~z~ (`"d_z"`, divided by the standard deviation
  of the differences) and *d*~rm~ (`"d_rm"`, *d*~z~ corrected for the
  correlation between the variables), and Hedges' *g* versions with a
  small-sample correction (`"g_av"`, `"g_z"`, `"g_rm"`; `"hedges_g"` is
  `"g_av"`). These come from
  [`effectsize::repeated_measures_d()`](https://easystats.github.io/effectsize/reference/repeated_measures_d.html).

- ci:

  confidence level of the effect size interval (for correlation tests,
  the interval of Pearson's *r*). Intervals are two-sided.

- var_equal:

  if `TRUE`, two-group *t* tests assume equal variances (Student's *t*
  test), and Cohen's *d* uses the pooled standard deviation. If `FALSE`
  (default), Welch's *t* test is used, and Cohen's *d* uses the average
  of the two groups' variances.

- mu:

  value that one-sample tests compare each variable with (for two-group
  tests, the hypothesized difference in location)

- method:

  correlation coefficient: `"pearson"` (default), `"spearman"`, or
  `"kendall"`

- descriptives:

  if `TRUE` (default), the table shows each group's mean and standard
  deviation (*t* tests) or median (Wilcoxon tests), or each variable's
  sample size (correlation and chi-square tests).

- pair_labels:

  for paired tests, labels of the first and second variable of each
  pair, used as the headings above their descriptive statistics (e.g.,
  `c("Pretest", "Posttest")`). If `NULL` (default), the headings are the
  variable names when there is one pair, and "First" and "Second"
  otherwise.

- starred:

  columns to star with significant *p*-values (e.g., `"t"`)

- bolded:

  columns to bold, if significant

- column_formats:

  column_formats object to format columns. If NULL, the default
  column_formats is used.

- ...:

  further arguments passed to the test function (e.g.,
  `alternative = "greater"`, or `correct = FALSE` for
  [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html))

## Value

`apa7_tbl` (tibble)

## Details

Each test uses the rows in which both the outcome and the grouping (or
predictor) variable are observed. For two-group tests, the grouping
variable must have exactly two observed values, and differences are the
first group (in factor level order) minus the second. Paired tests use
the rows in which both variables of the pair are observed, and
differences are the first variable minus the second.

Effect sizes are computed from the data rather than from the test
statistic, so Cohen's *d* for Welch's test is exact rather than
approximated from *t*.

Use [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
to write a table note that says which tests were done, how the effect
size was computed, and what the abbreviations stand for.

## Test results

`htest` objects keep the test statistic, its degrees of freedom, and the
*p*-value, but not the data, so `formula` and the arguments that control
the tests are not used, and effect sizes are computed from what the
objects keep:

- One-sample and paired *t* tests: Cohen's *d* (*d*~z~ for paired tests)
  is exactly \\t/\sqrt{n}\\, with an exact interval from the noncentral
  *t* distribution.

- Two-sample *t* tests: Cohen's *d* is approximated as \\2t/\sqrt{df}\\
  ([`effectsize::t_to_d()`](https://easystats.github.io/effectsize/reference/t_to_r.html)),
  which assumes groups of equal size, and the note says so. For exact
  values, give `apa_htest()` the data instead.

- Correlation tests: the correlation and, for Pearson's *r*, its
  interval and the sample size.

- Chi-square tests of independence: Cramér's *V*, \\\phi\\, or Cohen's
  *w*, computed from the observed table that the object keeps.

- Wilcoxon tests and chi-square goodness-of-fit tests: no effect size,
  because the objects do not keep the sample sizes.

The rows are labeled by the list's names or, if unnamed, by the objects'
data names (e.g., "mpg by am"). The table has no descriptive statistics.

## Examples

``` r
# Welch's t tests with Cohen's d
d <- mtcars
d$am <- factor(d$am, labels = c("Automatic", "Manual"))
apa_htest(d, mpg + hp + qsec ~ am)
#> # A tibble: 3 × 10
#>   Variable `Automatic_*M*` `Automatic_*SD*` `Manual_*M*` `Manual_*SD*` `*t*`
#>   <chr>    <chr>           <chr>            <chr>        <chr>         <chr>
#> 1 Mpg       17.15           3.83             24.39        6.17         −3.77
#> 2 Hp       160.26          53.91            126.85       84.06          1.27
#> 3 Qsec      18.18           1.75             17.36        1.79          1.29
#> # ℹ 4 more variables: `*df*` <chr>, `*p*` <chr>, `Cohen's *d*` <chr>,
#> #   `95% CI` <chr>

# Student's t test with Hedges' g
apa_htest(d, mpg ~ am, var_equal = TRUE, effect_size = "hedges_g")
#> # A tibble: 1 × 9
#>   Variable `Automatic_*M*` `Automatic_*SD*` `Manual_*M*` `Manual_*SD*` `*t*(30)`
#>   <chr>    <chr>           <chr>            <chr>        <chr>         <chr>    
#> 1 Mpg      17.15           3.83             24.39        6.17          −4.11    
#> # ℹ 3 more variables: `*p*` <chr>, `Hedges' *g*` <chr>, `95% CI` <chr>

# One-sample t test
apa_htest(d, mpg ~ 1, mu = 20)
#> # A tibble: 1 × 7
#>   Variable `*M*` `*SD*` `*t*(31)` `*p*` `Cohen's *d*` `95% CI`     
#>   <chr>    <chr> <chr>  <chr>     <chr> <chr>         <chr>        
#> 1 Mpg      20.09 6.03   0.09      .93   0.02          [−0.33, 0.36]

# Paired t test with d_av
d_sleep <- data.frame(drug1 = sleep$extra[1:10], drug2 = sleep$extra[11:20])
apa_htest(d_sleep, Pair(drug1, drug2) ~ 1)
#> # A tibble: 1 × 9
#>   Variable  `Drug 1_*M*` `Drug 1_*SD*` `Drug 2_*M*` `Drug 2_*SD*` `*t*(9)` `*p*`
#>   <chr>     <chr>        <chr>         <chr>        <chr>         <chr>    <chr>
#> 1 Drug 1 v… 0.75         1.79          2.33         2.00          −4.06    .003 
#> # ℹ 2 more variables: `*d*~av~` <chr>, `95% CI` <chr>

# Wilcoxon rank-sum tests with rank-biserial correlations
apa_htest(d, mpg + hp ~ am, test = "wilcoxon", exact = FALSE)
#> # A tibble: 2 × 7
#>   Variable `Automatic_*Mdn*` `Manual_*Mdn*` `*W*` `*p*` `*r*~rb~` `95% CI`    
#>   <chr>    <chr>             <chr>          <chr> <chr> <chr>     <chr>       
#> 1 Mpg       17.30             22.80          42   .002  −.66      [−.84, −.36]
#> 2 Hp       175.00            109.00         176   .05    .43      [ .04,  .70]

# Correlation tests
apa_htest(d, mpg + hp + qsec ~ wt, test = "cor")
#> # A tibble: 3 × 6
#>   Variable `*n*` `*r*` `95% CI`     `*t*(30)` `*p*`
#>   <chr>    <chr> <chr> <chr>        <chr>     <chr>
#> 1 Mpg      32    −.87  [−.93, −.74] −9.56     <.001
#> 2 Hp       32     .66  [ .40,  .82]  4.80     <.001
#> 3 Qsec     32    −.17  [−.49,  .19] −0.97      .34 

# Chi-square tests of independence with Cramer's V
d$cyl <- factor(d$cyl)
d$vs <- factor(d$vs, labels = c("V-shaped", "Straight"))
tbl <- apa_htest(d, cyl + vs ~ am, test = "chisq")
#> Warning: Chi-squared approximation may be incorrect
tbl
#> # A tibble: 2 × 7
#>   Variable `*N*` `*&chi;*^2^` `*df*` `*p*` `Cramér's *V*` `95% CI`  
#>   <chr>    <chr> <chr>        <chr>  <chr> <chr>          <chr>     
#> 1 Cyl      32    8.74         2      .01   .52            [.11, .85]
#> 2 Vs       32    0.35         1      .56   .17            [.00, .51]
apa_note(tbl)
#> [1] "Chi-square tests of independence with Am. 2 × 2 tables use Yates's continuity correction. CI = confidence interval for Cramér's *V*."

# Test results already computed
apa_htest(t.test(mpg ~ am, data = d))
#> # A tibble: 1 × 5
#>   Variable  `*t*(18.33)` `*p*` `Cohen's *d*` `95% CI`      
#>   <chr>     <chr>        <chr> <chr>         <chr>         
#> 1 mpg by am −3.77        .001  −1.76         [−2.82, −0.67]
apa_htest(list(
  Weight = cor.test(d$mpg, d$wt),
  Displacement = cor.test(d$mpg, d$disp)
))
#> # A tibble: 2 × 6
#>   Variable     `*n*` `*r*` `95% CI`     `*t*(30)` `*p*`
#>   <chr>        <chr> <chr> <chr>        <chr>     <chr>
#> 1 Weight       32    −.87  [−.93, −.74] −9.56     <.001
#> 2 Displacement 32    −.85  [−.92, −.71] −8.75     <.001
```
