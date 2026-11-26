# Hypothesis Tests

``` r

library(dplyr)
library(apa7)
apa7_defaults(font_family = "Open Sans", line_spacing = 1.5)
```

The `apa_htest` function runs common hypothesis tests on the variables
in a data frame and makes an APA-style table with one row per test. Each
row has descriptive statistics, the test statistic and its degrees of
freedom, the *p*-value, and an effect size with its confidence interval.
The tests come from the stats package (e.g., `t.test`), and the effect
sizes are computed from the data by the
[effectsize](https://easystats.github.io/effectsize/) package.

| Test | `test` | Formula | Default effect size |
|----|----|----|----|
| Welch’s or Student’s *t* test | `"t"` | `y ~ group` | Cohen’s *d* |
| One-sample *t* test | `"t"` | `y ~ 1` | Cohen’s *d* |
| Paired *t* test | `"t"` | `Pair(pre, post) ~ 1` | *d*_(av) |
| Wilcoxon rank-sum test | `"wilcoxon"` | `y ~ group` | Rank-biserial *r* |
| Wilcoxon signed-rank test | `"wilcoxon"` | `y ~ 1` or `Pair(pre, post) ~ 1` | Rank-biserial *r* |
| Correlation test | `"cor"` | `y ~ x` | The correlation itself |
| Chi-square test of independence | `"chisq"` | `y ~ group` | Cramér’s *V* |

Several outcomes on the left side of the formula, joined with `+`, give
one row each. Test results that were already computed (e.g., by
`t.test`) can also be formatted (see [Test Results Already
Computed](#test-results-already-computed)). In the console, `apa_htest`
returns an ordinary tibble. In Quarto and R Markdown documents, the
tibble is displayed as an APA-style table automatically, and
`apa_flextable` customizes it further.

## Comparing Two Groups

### Welch’s *t* test

The `sat.act` data in the psych package ([Revelle,
2026](#ref-revellePsychProceduresPsychological2026)) has self-reported
ACT and SAT scores from 700 people. Do men and women differ in their
scores? The grouping variable must have exactly two values. Make it a
factor with readable labels, and give the test scores names that read
well in a table.

``` r

d_sat <- psych::sat.act |>
  as_tibble() |>
  mutate(
    Gender = factor(gender, levels = 1:2, labels = c("Men", "Women")),
    ACT,
    `SAT-V` = SATV,
    `SAT-Q` = SATQ
  )
```

``` r

apa_htest(
  d_sat,
  ACT + `SAT-V` + `SAT-Q` ~ Gender
) |> 
  mutate(`*df*` = as.integer(`*df*`)) |> 
  apa_flextable()
```

| Variable | Men    |        |     | Women  |        | t    | df  | p      | Cohen’s d | 95% CI          |
|----------|--------|--------|-----|--------|--------|------|-----|--------|-----------|-----------------|
|          | M      | SD     |     | M      | SD     |      |     |        |           |                 |
| ACT      |  28.79 |   5.06 |     |  28.42 |   4.69 | 0.94 | 473 |  .35   | 0.08      | \[−0.08, 0.23\] |
| SAT-V    | 615.11 | 114.16 |     | 610.66 | 112.31 | 0.50 | 498 |  .62   | 0.04      | \[−0.12, 0.19\] |
| SAT-Q    | 635.87 | 116.02 |     | 596.00 | 113.07 | 4.35 | 492 | \<.001 | 0.35      | \[ 0.19, 0.51\] |

Table 1

Welch’s *t* Tests Comparing Test Scores of Men and Women

By default, the table shows each group’s mean (*M*) and standard
deviation (*SD*) under a spanner with the group’s name. Then come
Welch’s *t* test, its *p*-value, and Cohen’s *d* with its 95% confidence
interval. Welch’s test does not assume equal variances, so its degrees
of freedom are not whole numbers and differ from row to row. They get
their own column (*df*). When all rows have the same degrees of freedom,
as in Student’s *t* test below, they appear in the column header instead
(e.g., *t*(698)).

Differences are the first group (in factor level order) minus the
second, so a positive *d* means that men have the larger mean. To flip
the direction, reorder the factor levels (e.g., with
[`forcats::fct_rev`](https://forcats.tidyverse.org/reference/fct_rev.html)).

### Student’s *t* test and Hedges’ *g*

With `var_equal = TRUE`, the table shows Student’s *t* test, which
assumes equal variances, and Cohen’s *d* uses the pooled standard
deviation. Hedges’ *g* corrects *d* for its small-sample bias.

``` r

apa_htest(
  d_sat,
  ACT + `SAT-V` ~ Gender,
  var_equal = TRUE,
  effect_size = "hedges_g"
)
```

| Variable | Men    |        |     | Women  |        | t(698) | p   | Hedges’ g | 95% CI          |
|----------|--------|--------|-----|--------|--------|--------|-----|-----------|-----------------|
|          | M      | SD     |     | M      | SD     |        |     |           |                 |
| ACT      |  28.79 |   5.06 |     |  28.42 |   4.69 | 0.97   | .34 | 0.08      | \[−0.08, 0.23\] |
| SAT-V    | 615.11 | 114.16 |     | 610.66 | 112.31 | 0.50   | .62 | 0.04      | \[−0.12, 0.19\] |

Table 2

Student’s *t* Tests Comparing ACT and SAT Verbal Scores of Men and Women
With Hedges’ *g*

### One-sided tests and other test options

Other arguments are passed to the test function. For example,
`alternative = "greater"` makes a one-sided test, here of whether men
have higher SAT Quantitative scores than women. Effect size intervals
stay two-sided, and the table note says that the *p*-values are
one-sided.

``` r

tbl_greater <- apa_htest(
  d_sat,
  `SAT-Q` ~ Gender,
  alternative = "greater"
)
tbl_greater
```

| Variable | Men |  |  | Women |  | t(492.94) | p | Cohen’s d | 95% CI |
|----|----|----|----|----|----|----|----|----|----|
|  | M | SD |  | M | SD |  |  |  |  |
| SAT-Q | 635.87 | 116.02 |  | 596.00 | 113.07 | 4.35 | \<.001 | 0.35 | \[0.19, 0.51\] |

Table 3

One-Sided Welch’s *t* Test of SAT Quantitative Scores

``` r

apa_note(tbl_greater)
```

```
[1] "Welch's *t* test compares the groups of Gender without assuming equal variances. The *p*-value is one-sided (alternative: greater). Cohen's *d* uses the average of the two groups' variances. CI = confidence interval for Cohen's *d*."
```

## One-Sample Tests

To compare a mean with a fixed value, put `1` on the right side of the
formula and give the value as `mu`. SAT section scores were scaled to
have a mean of 500. Is the mean SAT Verbal score in `sat.act` different
from 500?

``` r

apa_htest(d_sat, `SAT-V` ~ 1, mu = 500)
```

| Variable | M      | SD     | t(699) | p      | Cohen’s d | 95% CI         |
|----------|--------|--------|--------|--------|-----------|----------------|
| SAT-V    | 612.23 | 112.90 | 26.30  | \<.001 | 0.99      | \[0.90, 1.08\] |

Table 4

One-Sample *t* Test of SAT Verbal Scores Against 500

## Paired Tests

### One pair

In the `sleep` data, 10 patients each took two sleep drugs. Paired data
must be in wide format, with one column per measurement. Put the two
variables of each pair in [`Pair()`](https://rdrr.io/r/stats/Pair.html),
with `1` on the right side of the formula, as in `t.test`:

``` r

d_sleep <- sleep |>
  tidyr::pivot_wider(names_from = group, values_from = extra, names_prefix = "Drug ")
d_sleep
```

```
# A tibble: 10 × 3
   ID    `Drug 1` `Drug 2`
   <fct>    <dbl>    <dbl>
 1 1          0.7      1.9
 2 2         -1.6      0.8
 3 3         -0.2      1.1
 4 4         -1.2      0.1
 5 5         -0.1     -0.1
 6 6          3.4      4.4
 7 7          3.7      5.5
 8 8          0.8      1.6
 9 9          0        4.6
10 10         2        3.4
```

``` r

tbl_sleep <- apa_htest(d_sleep, Pair(`Drug 1`, `Drug 2`) ~ 1)
```

The table note extracted with `apa_note` says how *d* was standardized:

```` markdown
```{r}
#| label: tbl-paired
#| tbl-cap: "Paired-Samples *t* test"
#| apa-note: !expr 'apa7::apa_note(tbl_sleep)'
tbl_sleep
```
````

| Variable          | Drug 1 |      |     | Drug 2 |      | t(9)  | p    | dav   | 95% CI           |
|-------------------|--------|------|-----|--------|------|-------|------|-------|------------------|
|                   | M      | SD   |     | M      | SD   |       |      |       |                  |
| Drug 1 vs. Drug 2 | 0.75   | 1.79 |     | 2.33   | 2.00 | −4.06 | .003 | −0.83 | \[−1.23, −0.43\] |

Table 5

Paired-Samples *t* test

*Note*. Paired *t* test compares Drug 1 with Drug 2. *d*_(av) divides
the mean difference by the average of the two variables’ standard
deviations ([Lakens, 2013](#ref-lakensCalculatingReportingEffect2013)).
CI = confidence interval for *d*_(av).

With one pair, the variable names head their descriptive statistics, and
differences are the first variable minus the second.

### Effect sizes for paired data

Paired data have several versions of Cohen’s *d*, which differ in the
standard deviation that the mean difference is divided by ([Lakens,
2013](#ref-lakensCalculatingReportingEffect2013)):

| `effect_size` | Effect size | Mean difference divided by |
|----|----|----|
| `"d_av"` (default) | *d*_(av) | The average of the two variables’ *SD*s |
| `"d_z"` | *d*_(z) | The *SD* of the differences |
| `"d_rm"` | *d*_(rm) | The *SD* of the differences, corrected for the correlation between the variables |
| `"g_av"`, `"g_z"`, `"g_rm"` | *g*_(av), *g*_(z), *g*_(rm) | As above, with Hedges’ small-sample correction |

The default, *d*_(av), is on the same scale as *d* for two independent
groups, so it can be compared with between-subjects studies. *d*_(z) is
often much larger, because the *SD* of the differences is small when the
two variables are highly correlated. It describes how consistent the
change is rather than how large it is relative to individual
differences. `"cohens_d"` is the same as `"d_av"`, and `"hedges_g"` is
the same as `"g_av"`. All of them come from
[`effectsize::repeated_measures_d`](https://easystats.github.io/effectsize/reference/repeated_measures_d.html).

``` r

apa_htest(d_sleep, Pair(`Drug 1`, `Drug 2`) ~ 1, effect_size = "d_z")
```

| Variable          | Drug 1 |      |     | Drug 2 |      | t(9)  | p    | dz    | 95% CI           |
|-------------------|--------|------|-----|--------|------|-------|------|-------|------------------|
|                   | M      | SD   |     | M      | SD   |       |      |       |                  |
| Drug 1 vs. Drug 2 | 0.75   | 1.79 |     | 2.33   | 2.00 | −4.06 | .003 | −1.28 | \[−2.12, −0.45\] |

Table 6

Paired-Samples *t* Test With *d*_(z)

### Several pairs

Join several pairs with `+` to get one row per pair. Their descriptive
statistics are headed “First” and “Second” unless you set `pair_labels`.
In this made-up study, anxiety and depression were measured before and
after an intervention:

``` r

set.seed(1)
d_prepost <- tibble(
  `Anxiety Pre` = rnorm(40, 50, 10),
  `Anxiety Post` = `Anxiety Pre` - rnorm(40, 4, 6),
  `Depression Pre` = rnorm(40, 50, 10),
  `Depression Post` = `Depression Pre` - rnorm(40, 1, 6)
)

apa_htest(
  d_prepost,
  Pair(`Anxiety Pre`, `Anxiety Post`) + Pair(`Depression Pre`, `Depression Post`) ~ 1,
  pair_labels = c("Pretest", "Posttest")
) |> 
  mutate(Variable = apa7::hanging_indent(Variable, width = 20)) |> 
  apa_flextable(no_format_columns = Variable) |> 
  flextable::width(width = c(1.58,.5,.5, .02,.5,.5, rep(.6, 3), 1.1)) 
```

[TABLE]

Table 7

Paired-Samples *t* Tests of Anxiety and Depression Before and After
Treatment

Each pair uses the rows in which both of its variables are observed.

## Wilcoxon Tests

With `test = "wilcoxon"`, the table shows each group’s median (*Mdn*),
the Wilcoxon test statistic (*W* for the rank-sum test, *V* for the
signed-rank test), and the rank-biserial correlation (*r*_(rb)). Like
other correlations, *r*_(rb) ranges from −1 to 1 and has no leading
zero.

``` r

apa_htest(
  d_sat,
  `SAT-V` + `SAT-Q` ~ Gender,
  test = "wilcoxon",
  exact = FALSE
) |> 
  rename(`*Mdn*_Men` = `Men_*Mdn*`,
         `*Mdn*_Women` = `Women_*Mdn*`)
```

| Variable | Mdn |       | W      | p      | rrb | 95% CI        |
|----------|-----|-------|--------|--------|-----|---------------|
|          | Men | Women |        |        |     |               |
| SAT-V    | 630 | 620   | 57,518 |  .54   | .03 | \[−.06, .12\] |
| SAT-Q    | 660 | 600   | 65,921 | \<.001 | .22 | \[ .13, .30\] |

Table 8

Wilcoxon Rank-Sum Tests Comparing SAT Scores of Men and Women

With ties in the data, `wilcox.test` cannot compute exact *p*-values and
warns about it. `exact = FALSE` asks for the normal approximation
without the warning.

Signed-rank tests use the same formulas as one-sample and paired *t*
tests:

``` r

apa_htest(
  d_sleep,
  Pair(`Drug 1`, `Drug 2`) ~ 1,
  test = "wilcoxon",
  exact = FALSE
) |> 
  rename(`*Mdn*_Drug 1` = `Drug 1_*Mdn*`,
         `*Mdn*_Drug 2` = `Drug 2_*Mdn*`)
```

| Variable          | Mdn    |        | V   | p    | rrb   | 95% CI           |
|-------------------|--------|--------|-----|------|-------|------------------|
|                   | Drug 1 | Drug 2 |     |      |       |                  |
| Drug 1 vs. Drug 2 | 0.35   | 1.75   | 0   | .009 | −1.00 | \[−1.00, −1.00\] |

Table 9

Wilcoxon Signed-Rank Test of Extra Sleep Under Two Drugs

Every patient slept more with Drug 2 (or the same with both), so
*r*_(rb) is −1.00.

## Correlation Tests

With `test = "cor"`, each variable on the left is correlated with the
variable on the right. The table shows the sample size, the correlation
with its confidence interval, and the *t* test of the correlation. The
`epi.bfi` data in the psychTools package ([Revelle,
2019](#ref-revellePsychToolsToolsAccompany2019)) has personality,
depression, and anxiety scores from 231 undergraduates. How strongly do
depression, anxiety, and extraversion correlate with neuroticism?

``` r

d_personality <- psychTools::epi.bfi |>
  as_tibble() |>
  select(
    Neuroticism = bfneur,
    Extraversion = bfext,
    Depression = bdi,
    Anxiety = traitanx
  )

apa_htest(
  d_personality,
  Depression + Anxiety + Extraversion ~ Neuroticism,
  test = "cor"
)
```

| Variable     | n   | r   | 95% CI        | t(229) | p      |
|--------------|-----|-----|---------------|--------|--------|
| Depression   | 231 | .47 | \[ .36, .56\] |  7.97  | \<.001 |
| Anxiety      | 231 | .59 | \[ .50, .67\] | 11.14  | \<.001 |
| Extraversion | 231 | .04 | \[−.09, .17\] |  0.56  |  .57   |

Table 10

Pearson Correlations of Depression, Anxiety, and Extraversion With
Neuroticism

`method = "spearman"` or `method = "kendall"` gives rank correlations,
which have no confidence interval:

``` r

apa_htest(
  d_personality,
  Depression + Anxiety ~ Neuroticism,
  test = "cor",
  method = "spearman",
  exact = FALSE
)
```

| Variable   | n   | ρ   | p      |
|------------|-----|-----|--------|
| Depression | 231 | .47 | \<.001 |
| Anxiety    | 231 | .62 | \<.001 |

Table 11

Spearman Correlations of Depression and Anxiety With Neuroticism

To display all the correlations among a set of variables, use `apa_cor`
(see the [correlations
article](https://wjschne.github.io/apa7/articles/correlations.md)).

## Chi-Square Tests of Independence

With `test = "chisq"`, each variable on the left is cross-tabulated with
the variable on the right and tested for independence. The default
effect size is Cramér’s *V*. For 2 × 2 tables, `effect_size = "phi"`
gives *φ*.

The `survey` data in the MASS package ([Venables & Ripley,
2002](#ref-ripleyModernApplied2002)) has the answers of 237 statistics
students to questions about themselves. Do women and men differ in how
often they exercise or in which hand they write with?

``` r

d_survey <- MASS::survey |>
  as_tibble() |>
  transmute(
    Sex,
    Exercise = factor(Exer, levels = c("None", "Some", "Freq"),
                      labels = c("None", "Some", "Frequent")),
    `Writing Hand` = W.Hnd
  )

tbl_chisq <- apa_htest(d_survey, Exercise + `Writing Hand` ~ Sex, test = "chisq")
```

```` markdown
```{r}
#| label: tbl-chisq
#| tbl-cap: Chi-Square Tests of Exercise and Writing Hand by Sex
#| apa-note: !expr 'apa7::apa_note(tbl_chisq)'
tbl_chisq
```
````

| Variable     | N   | χ2   | df  | p   | Cramér’s V | 95% CI       |
|--------------|-----|------|-----|-----|------------|--------------|
| Exercise     | 236 | 5.72 | 2   | .06 | .16        | \[.00, .27\] |
| Writing Hand | 235 | 0.24 | 1   | .63 | .05        | \[.00, .18\] |

Table 12

Chi-Square Tests of Exercise and Writing Hand by Sex

*Note*. Chi-square tests of independence with Sex. 2 × 2 tables use
Yates’s continuity correction. CI=confidence interval for Cramér’s *V*.

As in `chisq.test`, 2 × 2 tables (here, Writing Hand × Sex) use Yates’s
continuity correction, and the note says so. Use `correct = FALSE` to
turn it off. If some expected counts are small, `chisq.test` warns that
the chi-square approximation may be incorrect. To display the cell
counts of one contingency table, use `apa_chisq` (see the [contingency
tables article](https://wjschne.github.io/apa7/articles/chisquare.md)).

## Customizing Hypothesis Test Tables

### Notes

`apa_htest` records how the tests were done, and `apa_note` turns that
record into note text. The note names the test, explains the effect size
and the abbreviations, and, when used, explains stars and bold values.
The text is in markdown, and it does not start with “*Note*.” because
apaquarto adds that.

``` r

tbl_sat <- apa_htest(
  d_sat,
  `SAT-V` + `SAT-Q` ~ Gender,
  starred = "t"
)
apa_note(tbl_sat)
```

```
[1] "Welch's *t* tests compare the groups of Gender without assuming equal variances. Cohen's *d* uses the average of the two groups' variances. CI = confidence interval for Cohen's *d*. ^\\* ^*p* ≤ .05. ^\\*\\* ^*p* ≤ .01. ^\\*\\*\\* ^*p* ≤ .001."
```

With [apaquarto](https://wjschne.github.io/apaquarto/), use `apa_note`
in the `apa-note` chunk option with `!expr`. Chunk options are evaluated
before the chunk runs, so make the table in an earlier chunk (as
`tbl_sat` was made above) and display it in the chunk with the note:

```` markdown
```{r}
#| label: tbl-sat-note
#| apa7-width: landscape
#| column: page-right
#| tbl-cap: Welch's *t* Tests Comparing SAT Scores of Men and Women
#| apa-note: !expr apa7::apa_note(tbl_sat)
tbl_sat
```
````

| Variable | Men |  |  | Women |  | t | df | p | Cohen’s d | 95% CI |
|----|----|----|----|----|----|----|----|----|----|----|
|  | M | SD |  | M | SD |  |  |  |  |  |
| SAT-V | 615.11 | 114.16 |  | 610.66 | 112.31 | 0.50     | 498.58 |  .62  | 0.04 | \[−0.12, 0.19\] |
| SAT-Q | 635.87 | 116.02 |  | 596.00 | 113.07 | 4.35\*\*\* | 492.94 | \<.001 | 0.35 | \[ 0.19, 0.51\] |

Table 13

Welch’s *t* Tests Comparing SAT Scores of Men and Women

*Note*. Welch’s *t* tests compare the groups of Gender without assuming
equal variances. Cohen’s *d* uses the average of the two groups’
variances. CI = confidence interval for Cohen’s *d*. ^(\* )*p* ≤ .05.
^(\*\* )*p* ≤ .01. ^(\*\*\* )*p* ≤ .001.

Without apaquarto, `apa_flextable(tbl, note = TRUE)` puts the same note
below the table. Do not combine it with the `apa-note` chunk option, or
the note will appear twice.

``` r

apa_flextable(tbl_sat, note = TRUE)
```

| Variable | Men |  |  | Women |  | t | df | p | Cohen’s d | 95% CI |
|----|----|----|----|----|----|----|----|----|----|----|
|  | M | SD |  | M | SD |  |  |  |  |  |
| SAT-V | 615.11 | 114.16 |  | 610.66 | 112.31 | 0.50     | 498.58 |  .62  | 0.04 | \[−0.12, 0.19\] |
| SAT-Q | 635.87 | 116.02 |  | 596.00 | 113.07 | 4.35\*\*\* | 492.94 | \<.001 | 0.35 | \[ 0.19, 0.51\] |
| Note. Welch’s t tests compare the groups of Gender without assuming equal variances. Cohen’s d uses the average of the two groups’ variances. CI = confidence interval for Cohen’s d. \* p ≤ .05. \*\* p ≤ .01. \*\*\* p ≤ .001. |  |  |  |  |  |  |  |  |  |  |

Table 14

Welch’s *t* Tests Comparing SAT Scores of Men and Women With a Generated
Note

The generated note is a starting point. With `collapse = NULL`,
`apa_note` returns one sentence per element, so you can drop, reword, or
add sentences.

### Stars and bold values

`starred` puts significance stars on the columns you name, and `bolded`
bolds significant values:

``` r

apa_htest(
  d_sat,
  `SAT-V` + `SAT-Q` ~ Gender,
  bolded = "p"
)
```

| Variable | Men |  |  | Women |  | t | df | p | Cohen’s d | 95% CI |
|----|----|----|----|----|----|----|----|----|----|----|
|  | M | SD |  | M | SD |  |  |  |  |  |
| SAT-V | 615.11 | 114.16 |  | 610.66 | 112.31 | 0.50 | 498.58 |  .62  | 0.04 | \[−0.12, 0.19\] |
| SAT-Q | 635.87 | 116.02 |  | 596.00 | 113.07 | 4.35 | 492.94 | \<.001 | 0.35 | \[ 0.19, 0.51\] |

Table 15

Welch’s *t* Tests With Significant *p*-Values in Bold

### Fewer columns

`descriptives = FALSE` drops the descriptive statistics, and
`effect_size = "none"` drops the effect size:

``` r

apa_htest(
  d_sat,
  `SAT-V` + `SAT-Q` ~ Gender,
  descriptives = FALSE,
  effect_size = "none"
)
```

| Variable | t    | df     | p      |
|----------|------|--------|--------|
| SAT-V    | 0.50 | 498.58 |  .62   |
| SAT-Q    | 4.35 | 492.94 | \<.001 |

Table 16

Welch’s *t* Tests Without Descriptive Statistics or Effect Sizes

### Labels

Variable names are converted to title case (e.g., `bdi` becomes Bdi).
For better labels, rename the variables before calling `apa_htest`, as
was done throughout this article. Because the output is a tibble, you
can also change the labels afterward.

### Missing data

Each test uses the rows in which the outcome and the grouping (or
predictor) variable are observed, so rows can have different sample
sizes. In `sat.act`, for example, 13 people did not report an SAT
Quantitative score, so the SAT Quantitative tests above use fewer people
than the ACT and SAT Verbal tests. Paired tests use the rows in which
both variables of the pair are observed.

## Test Results Already Computed

Functions like `t.test` return `htest` objects, which have the test
statistic and *p*-value but not the data. `apa_htest` also accepts these
objects, or a list of objects of the same kind, with one row per test.
The names of the list label the rows:

``` r

neuroticism_tests <- list(
  Depression = cor.test(d_personality$Neuroticism, d_personality$Depression),
  Anxiety = cor.test(d_personality$Neuroticism, d_personality$Anxiety),
  Extraversion = cor.test(d_personality$Neuroticism, d_personality$Extraversion)
)
apa_htest(neuroticism_tests) |>
  apa_flextable(note = TRUE)
```

| Variable | n | r | 95% CI | t(229) | p |
|----|----|----|----|----|----|
| Depression | 231 | .47 | \[ .36, .56\] |  7.97 | \<.001 |
| Anxiety | 231 | .59 | \[ .50, .67\] | 11.14 | \<.001 |
| Extraversion | 231 | .04 | \[−.09, .17\] |  0.56 |  .57  |
| Note. Results are from Pearson correlation tests. CI = confidence interval for r. |  |  |  |  |  |

Table 17

Correlations of Neuroticism With Depression, Anxiety, and Extraversion

Effect sizes, however, are computed from what the objects keep, and that
is not always enough:

| Test | Effect size from the object |
|----|----|
| One-sample *t* test | Cohen’s *d* = *t*/√*n*, exactly |
| Paired *t* test | *d*_(z) = *t*/√*n*, exactly (not *d*_(av), which needs the data) |
| Welch’s or Student’s *t* test | Cohen’s *d* ≈ 2*t*/√*df*, which assumes groups of equal size |
| Correlation test | The correlation itself |
| Chi-square test of independence | Cramér’s *V* (or *φ*, or Cohen’s *w*), exactly, from the observed table |
| Wilcoxon tests | None, because the group sizes are not kept |

For two groups, the approximation can be noticeably off when the groups
differ in size or variance, as men (247) and women (453) do in
`sat.act`. The note says that it is an approximation:

``` r

t.test(`SAT-Q` ~ Gender, data = d_sat) |>
  apa_htest() |>
  apa_flextable(note = TRUE)
```

| Variable | t(492.94) | p | Cohen’s d | 95% CI |
|----|----|----|----|----|
| SAT-Q by Gender | 4.35 | \<.001 | 0.39 | \[0.21, 0.57\] |
| Note. The result is from Welch’s t test. Cohen’s d is approximated from t and its degrees of freedom (2t/√df), which assumes groups of equal size. CI = confidence interval for Cohen’s d. |  |  |  |  |

Table 18

Welch’s *t* Test of SAT-Q Scores by Gender

When the data are at hand, giving them to `apa_htest` is better: the
effect sizes are then computed from the data rather than approximated
from the test statistic, and the table has descriptive statistics.

## References

Lakens, D. (2013). Calculating and reporting effect sizes to facilitate
cumulative science: A practical primer for t-tests and ANOVAs.
*Frontiers in Psychology*, *4*, Article 863.
<https://doi.org/10.3389/fpsyg.2013.00863>

Revelle, W. (2019). *psychTools: Tools to accompany the psych package
for psychological research* (Version 2.6.4) \[Computer software\].
Comprehensive R Archive Network.
<https://doi.org/10.32614/CRAN.package.psychTools>

Revelle, W. (2026). *psych: Procedures for psychological, psychometric,
and personality research* (Version 2.6.9) \[Computer software\].
Northwestern University. <https://CRAN.R-project.org/package=psych>

Venables, W. N., & Ripley, B. D. (2002). *Modern applied statistics with
S* (4th ed.). Springer. <https://www.stats.ox.ac.uk/pub/MASS4/>
