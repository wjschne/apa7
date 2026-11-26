# Correlations

``` r

library(apa7)
library(dplyr)
library(flextable)
library(ftExtra)
library(tidyr)
library(stringr)
library(correlation)
apa7_defaults(font_family = "Open Sans", line_spacing = 1.5)
```

Correlation matrices look simple enough…until you try to make one by
hand in MS Word. The `apa_cor` function can remove most of the tedium by
formatting most of what can automated, and any finishing touches can be
added with a styling function.

The examples use the `epi.bfi` data set from the psychTools package
([Revelle, 2019](#ref-revellePsychToolsToolsAccompany2019)), which has
personality, depression, and anxiety scores for 231 participants in
studies at Northwestern University’s Personality, Motivation, and
Cognition Laboratory. Openness, conscientiousness, extraversion,
agreeableness, and neuroticism are Big Five scale scores, and depression
is the Beck Depression Inventory score.

``` r

d_personality <- psychTools::epi.bfi |>
  select(
    Openness = bfopen,
    Conscientiousness = bfcon,
    Extraversion = bfext,
    Agreeablness = bfagree,
    Neuroticism = bfneur,
    Depression = bdi
  )
```

By default, the output of `apa_cor`
[flextable](https://davidgohel.github.io/flextable/), with significant
correlations starred.

``` r

d_personality |> 
  apa_cor()
```

| Variable |  | M | SD | 1 |  | 2 |  | 3 |  | 4 |  | 5 |  | 6 |  |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 1.  | Openness | 123.43 | 20.51 | — |  |  |  |  |  |  |  |  |  |  |  |
| 2.  | Conscientiousness | 113.25 | 21.88 | .31 | \*\*\* | — |  |  |  |  |  |  |  |  |  |
| 3.  | Extraversion | 102.18 | 26.45 | .46 | \*\*\* | .27 | \*\*\* | — |  |  |  |  |  |  |  |
| 4.  | Agreeablness | 125.00 | 18.14 | .39 | \*\*\* | .45 | \*\*\* | .48 | \*\*\* | — |  |  |  |  |  |
| 5.  | Neuroticism |  87.97 | 23.34 | .29 | \*\*\* | .04 |  | .04 |  | −.04 |  | — |  |  |  |
| 6.  | Depression |   6.78 |  5.78 | −.08 |  | −.18 | \*\* | −.14 | \* | −.14 | \* | .47 | \*\*\* | — |  |
| \* p ≤ .05. \*\* p ≤ .01. \*\*\* p ≤ .001 |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Table 1

Correlations Among Personality and Depression

Significant correlations can be bolded instead.

``` r

d_personality |>
  apa_cor(bold_significant = TRUE)
```

| Variable |  | M | SD | 1 | 2 | 3 | 4 | 5 | 6 |
|----|----|----|----|----|----|----|----|----|----|
| 1.  | Openness | 123.43 | 20.51 |  —     |  |  |  |  |  |
| 2.  | Conscientiousness | 113.25 | 21.88 |  .31 |  —     |  |  |  |  |
| 3.  | Extraversion | 102.18 | 26.45 |  .46 |  .27 |  —     |  |  |  |
| 4.  | Agreeablness | 125.00 | 18.14 |  .39 |  .45 |  .48 |  —     |  |  |
| 5.  | Neuroticism |  87.97 | 23.34 |  .29 |  .04 |  .04 | −.04 | —     |  |
| 6.  | Depression |   6.78 |  5.78 | −.08 | −.18 | −.14 | −.14 | .47 | — |
| Note. Correlations significant at p ≤ .05 are bolded. |  |  |  |  |  |  |  |  |  |

Table 2

Correlations With Significant Correlations in Bold

## Summary functions

You can include any named list of summary functions:

``` r

# Named list of descriptive functions
my_summaries <- list(Mdn = median, "*IQR*" = IQR)

apa_cor(d_personality, 
        summary_functions = my_summaries, 
        star_significant = FALSE)
```

| Variable |                   | Mdn | IQR   | 1      | 2      | 3      | 4      | 5     | 6   |
|----------|-------------------|-----|-------|--------|--------|--------|--------|-------|-----|
| 1.       | Openness          | 125 | 26.50 |  —     |        |        |        |       |     |
| 2.       | Conscientiousness | 114 | 29.50 |  .31   |  —     |        |        |       |     |
| 3.       | Extraversion      | 104 | 30.50 |  .46   |  .27   |  —     |        |       |     |
| 4.       | Agreeablness      | 126 | 24.50 |  .39   |  .45   |  .48   |  —     |       |     |
| 5.       | Neuroticism       |  90 | 34.00 |  .29   |  .04   |  .04   | −.04   | —     |     |
| 6.       | Depression        |   6 |  6.00 | −.08   | −.18   | −.14   | −.14   | .47   | —   |

Table 3

Correlations Among Personality and Depression With Medians and
Interquartile Ranges

You can also use a plain vector of descriptive functions from these
options:
`"IQR", "Interquartile Range", "Kurtosis", "MAD", "Median Absolute Deviation", "M", "Mean", "Med", "Median", "n", "N", "Quantile", "Range", "SD", "Skewness", "Var", "Variance"`

``` r

my_summaries <- c("n", "M", "SD", "Skewness", "Kurtosis")

apa_cor(d_personality, summary_functions = my_summaries)
```

| Variable |  | n | M | SD | Skewness | Kurtosis | 1 |  | 2 |  | 3 |  | 4 |  | 5 |  | 6 |  |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 1.  | Openness | 231 | 123.43 | 20.51 | −0.16 | −0.16 | — |  |  |  |  |  |  |  |  |  |  |  |
| 2.  | Conscientiousness | 231 | 113.25 | 21.88 | −0.02 |  0.23 | .31 | \*\*\* | — |  |  |  |  |  |  |  |  |  |
| 3.  | Extraversion | 231 | 102.18 | 26.45 | −0.41 |  0.51 | .46 | \*\*\* | .27 | \*\*\* | — |  |  |  |  |  |  |  |
| 4.  | Agreeablness | 231 | 125.00 | 18.14 | −0.21 | −0.27 | .39 | \*\*\* | .45 | \*\*\* | .48 | \*\*\* | — |  |  |  |  |  |
| 5.  | Neuroticism | 231 |  87.97 | 23.34 |  0.07 | −0.55 | .29 | \*\*\* | .04 |  | .04 |  | −.04 |  | — |  |  |  |
| 6.  | Depression | 231 |   6.78 |  5.78 |  1.29 |  1.50 | −.08 |  | −.18 | \*\* | −.14 | \* | −.14 | \* | .47 | \*\*\* | — |  |
| \* p ≤ .05. \*\* p ≤ .01. \*\*\* p ≤ .001 |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Table 4

Correlations Among Personality and Depression With Additional Summary
Statistics

## Styling options

Your styling options include:

- `note` A custom markdown formatted note. Overrides the automatic note.
  Default: `NULL`
- `alpha` *p*-value thresholds for one, two, and three stars (with
  `bold_significant = TRUE`, only the first is used). Default:
  `c(.05, .01, .001)`
- `digits` Number of decimal places. Default: `2`
- `bold_significant` Whether to bold significant correlations instead of
  starring them. Default: `FALSE`
- `star_significant` Whether to star significant correlations. Default:
  `TRUE`
- `significance_note` Whether the note explains the stars (or bolding).
  Overridden by a custom note. Also, the automatic note will not appear
  if no correlations are significant. Default: `TRUE`
- `output` Options are `flextable` or `tibble`. Default: `flextable`
- `font_family` Font family (typeface). Default: `Times New Roman`
- `font_size` Font size (in points). Default: `12`
- `text_color` Text color. Default: `black`
- `border_color` Table border color. Default: `black`
- `border_width` Table border width. Default: `0.5`
- `line_spacing` Text line spacing. Default: `2`

For example,

``` r

d_personality |>
  apa_cor(
    alpha = 0.001,
    digits = 2,
    bold_significant = TRUE,
    significance_note = TRUE,
    summary_functions = my_summaries,
    font_family = "Arial",
    font_size = 10,
    text_color = "darkred",
    border_color = "royalblue4",
    border_width = 1,
    line_spacing = 2
  )
```

| Variable |  | n | M | SD | Skewness | Kurtosis | 1 | 2 | 3 | 4 | 5 | 6 |
|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 1.  | Openness | 231 | 123.43 | 20.51 | −0.16 | −0.16 |  —     |  |  |  |  |  |
| 2.  | Conscientiousness | 231 | 113.25 | 21.88 | −0.02 |  0.23 |  .31 |  —     |  |  |  |  |
| 3.  | Extraversion | 231 | 102.18 | 26.45 | −0.41 |  0.51 |  .46 |  .27 |  —     |  |  |  |
| 4.  | Agreeablness | 231 | 125.00 | 18.14 | −0.21 | −0.27 |  .39 |  .45 |  .48 |  —     |  |  |
| 5.  | Neuroticism | 231 |  87.97 | 23.34 |  0.07 | −0.55 |  .29 |  .04 |  .04 | −.04 | —     |  |
| 6.  | Depression | 231 |   6.78 |  5.78 |  1.29 |  1.50 | −.08 | −.18 | −.14 | −.14 | .47 | — |
| Note. Correlations significant at p ≤ .001 are bolded. |  |  |  |  |  |  |  |  |  |  |  |  |

Table 5

Correlations With Alternative Styling

To remove summary statistics entirely, set `summary_functions` to `NA`
or `NULL`.

``` r

apa_cor(d_personality, summary_functions = NA)
```

| Variable |  | 1 |  | 2 |  | 3 |  | 4 |  | 5 |  | 6 |  |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 1.  | Openness | — |  |  |  |  |  |  |  |  |  |  |  |
| 2.  | Conscientiousness | .31 | \*\*\* | — |  |  |  |  |  |  |  |  |  |
| 3.  | Extraversion | .46 | \*\*\* | .27 | \*\*\* | — |  |  |  |  |  |  |  |
| 4.  | Agreeablness | .39 | \*\*\* | .45 | \*\*\* | .48 | \*\*\* | — |  |  |  |  |  |
| 5.  | Neuroticism | .29 | \*\*\* | .04 |  | .04 |  | −.04 |  | — |  |  |  |
| 6.  | Depression | −.08 |  | −.18 | \*\* | −.14 | \* | −.14 | \* | .47 | \*\*\* | — |  |
| \* p ≤ .05. \*\* p ≤ .01. \*\*\* p ≤ .001 |  |  |  |  |  |  |  |  |  |  |  |  |  |

Table 6

Correlations Without Summary Statistics

## Customization

The styling options for `apa_cor` are minimal, but flextable allows for
all kinds of customization. Just place whatever code from flextable you
want after `apa_cor`. Here I set the width to 50% and align the table to
the left of the page. I place a footnote in a specific cell in the table
body.

``` r

apa_cor(d_personality, table_width = .5) |>
  flextable::footnote(
    i = 2,
    j = "Variable",
    value = flextable::as_paragraph("This is a footnote."),
    part = "body"
  )
```

| Variable |  | M | SD | 1 |  | 2 |  | 3 |  | 4 |  | 5 |  | 6 |  |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 1.  | Openness | 123.43 | 20.51 | — |  |  |  |  |  |  |  |  |  |  |  |
| 2.  | Conscientiousness1 | 113.25 | 21.88 | .31 | \*\*\* | — |  |  |  |  |  |  |  |  |  |
| 3.  | Extraversion | 102.18 | 26.45 | .46 | \*\*\* | .27 | \*\*\* | — |  |  |  |  |  |  |  |
| 4.  | Agreeablness | 125.00 | 18.14 | .39 | \*\*\* | .45 | \*\*\* | .48 | \*\*\* | — |  |  |  |  |  |
| 5.  | Neuroticism |  87.97 | 23.34 | .29 | \*\*\* | .04 |  | .04 |  | −.04 |  | — |  |  |  |
| 6.  | Depression |   6.78 |  5.78 | −.08 |  | −.18 | \*\* | −.14 | \* | −.14 | \* | .47 | \*\*\* | — |  |
| \* p ≤ .05. \*\* p ≤ .01. \*\*\* p ≤ .001 |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
| 1This is a footnote. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Table 7

Correlations Styled With flextable Functions

## Output to tibble

If you need to process the underlying data before converting to
flextable (or use another table-creating function from another package),
you can output a tibble and then process the table however you want.
Here we add a row title.

``` r

apa_cor(d_personality, output = "tibble") |>
  mutate(apa7listcolumn = str_trim(apa7listcolumn)) |>
  unite(col = Variable, 
        apa7listcolumn, 
        Variable, 
        sep = "&thinsp;") |>
  add_row(Variable = "Row title", .before = 1) |>
  apa_flextable(no_format_columns = "Variable") |>
  padding(i = 2:7,
          j = 1,
          padding.left = 15)
```

| Variable | M | SD | 1 |  | 2 |  | 3 |  | 4 |  | 5 |  | 6 |  |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| Row title |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
| 1. Openness | 123.43 | 20.51 | — |  |  |  |  |  |  |  |  |  |  |  |
| 2. Conscientiousness | 113.25 | 21.88 | .31 | \*\*\* | — |  |  |  |  |  |  |  |  |  |
| 3. Extraversion | 102.18 | 26.45 | .46 | \*\*\* | .27 | \*\*\* | — |  |  |  |  |  |  |  |
| 4. Agreeablness | 125.00 | 18.14 | .39 | \*\*\* | .45 | \*\*\* | .48 | \*\*\* | — |  |  |  |  |  |
| 5. Neuroticism |  87.97 | 23.34 | .29 | \*\*\* | .04 |  | .04 |  | −.04 |  | — |  |  |  |
| 6. Depression |   6.78 |  5.78 | −.08 |  | −.18 | \*\* | −.14 | \* | −.14 | \* | .47 | \*\*\* | — |  |

Table 8

Correlations With a Row Title

## Other Kinds of Correlations

`apa_cor` computes the correlations with
[`correlation::correlation`](https://easystats.github.io/correlation/reference/correlation.html)
from the [correlation](https://easystats.github.io/correlation/)
package: by default, Pearson correlations with unadjusted *p*-values.
Depression scores are skewed, as the skewness in the summary statistics
above shows, so rank correlations are a reasonable alternative. The
`method` argument chooses the kind of correlation. For example, Spearman
rank correlations:

``` r

apa_cor(d_personality, method = "spearman")
```

| Variable |  | M | SD | 1 |  | 2 |  | 3 |  | 4 |  | 5 |  | 6 |  |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 1.  | Openness | 123.43 | 20.51 | — |  |  |  |  |  |  |  |  |  |  |  |
| 2.  | Conscientiousness | 113.25 | 21.88 | .32 | \*\*\* | — |  |  |  |  |  |  |  |  |  |
| 3.  | Extraversion | 102.18 | 26.45 | .43 | \*\*\* | .25 | \*\*\* | — |  |  |  |  |  |  |  |
| 4.  | Agreeablness | 125.00 | 18.14 | .39 | \*\*\* | .43 | \*\*\* | .50 | \*\*\* | — |  |  |  |  |  |
| 5.  | Neuroticism |  87.97 | 23.34 | .28 | \*\*\* | .04 |  | .05 |  | −.04 |  | — |  |  |  |
| 6.  | Depression |   6.78 |  5.78 | −.10 |  | −.16 | \* | −.13 | \* | −.11 |  | .47 | \*\*\* | — |  |
| Note. Values are Spearman correlations. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
| \* p ≤ .05. \*\*\* p ≤ .001 |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Table 9

Spearman Rank Correlations Among Personality and Depression

The note says what kind of correlations are shown. Other options for
`method` include:

| Option        | Correlation                     |
|:--------------|:--------------------------------|
| `pearson`     | Pearson’s Correlation (default) |
| `spearman`    | Spearman’s Rank Correlation     |
| `kendall`     | Kendall’s Rank Correlation      |
| `biserial`    | Biserial Correlation            |
| `polychoric`  | Polychoric Correlation          |
| `tetrachoric` | Tetrachoric Correlation         |
| `biweight`    | Biweight midcorrelation         |
| `distance`    | Distance Correlation            |
| `percentage`  | Percentage Bend Correlation     |
| `blomqvist`   | Blomqvist’s coefficient         |
| `hoeffding`   | Hoeffding’s D                   |
| `gamma`       | Gamma Correlation               |
| `gaussian`    | for Gaussian Rank correlation   |
| `shepherd`    | for Shepherd’s Pi correlation   |

To adjust the *p*-values (and so the stars) for multiple comparisons,
use `p_adjust` (e.g., `p_adjust = "holm"`), and the note says how they
were adjusted. Partial correlations, each controlling for the other
variables in the table, need only `partial = TRUE`:

``` r

apa_cor(d_personality, partial = TRUE)
```

| Variable |  | M | SD | 1 |  | 2 |  | 3 |  | 4 |  | 5 |  | 6 |  |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 1.  | Openness | 123.43 | 20.51 | — |  |  |  |  |  |  |  |  |  |  |  |
| 2.  | Conscientiousness | 113.25 | 21.88 | .10 |  | — |  |  |  |  |  |  |  |  |  |
| 3.  | Extraversion | 102.18 | 26.45 | .31 | \*\*\* | .01 |  | — |  |  |  |  |  |  |  |
| 4.  | Agreeablness | 125.00 | 18.14 | .20 | \*\* | .35 | \*\*\* | .33 | \*\*\* | — |  |  |  |  |  |
| 5.  | Neuroticism |  87.97 | 23.34 | .37 | \*\*\* | .10 |  | −.02 |  | −.15 | \* | — |  |  |  |
| 6.  | Depression |   6.78 |  5.78 | −.17 | \* | −.16 | \* | −.06 |  | .05 |  | .51 | \*\*\* | — |  |
| Note. Values are partial Pearson correlations, each controlling for all other variables in the table. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
| \* p ≤ .05. \*\* p ≤ .01. \*\*\* p ≤ .001 |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Table 10

Partial Correlations Among Personality and Depression

Partial correlations can differ considerably from ordinary correlations.
With zero-order correlations, openness and extraversion correlate at
.27, but once other personality variables are held constant, the partial
correlation between them is close to zero.

Other arguments of
[`correlation::correlation`](https://easystats.github.io/correlation/reference/correlation.html)
(e.g., `robust = TRUE`, `winsorize = .2`, or `multilevel = TRUE`) can be
given to `apa_cor` as well. Variable names with spaces work for every
kind of correlation, because `apa_cor` gives the variables internal
names while it computes the correlations. (Called directly,
[`correlation::correlation`](https://easystats.github.io/correlation/reference/correlation.html)
does not accept such names for partial or multilevel correlations.)

The output of
[`correlation::correlation`](https://easystats.github.io/correlation/reference/correlation.html)
can also be given to `apa_cor`:

``` r

correlation(d_personality, method = "spearman") |>
  apa_cor()
```

| Variable |  | M | SD | 1 |  | 2 |  | 3 |  | 4 |  | 5 |  | 6 |  |
|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 1.  | Openness | 123.43 | 20.51 | — |  |  |  |  |  |  |  |  |  |  |  |
| 2.  | Conscientiousness | 113.25 | 21.88 | .32 | \*\*\* | — |  |  |  |  |  |  |  |  |  |
| 3.  | Extraversion | 102.18 | 26.45 | .43 | \*\*\* | .25 | \*\*\* | — |  |  |  |  |  |  |  |
| 4.  | Agreeablness | 125.00 | 18.14 | .39 | \*\*\* | .43 | \*\*\* | .50 | \*\*\* | — |  |  |  |  |  |
| 5.  | Neuroticism |  87.97 | 23.34 | .28 | \*\*\* | .04 |  | .05 |  | −.04 |  | — |  |  |  |
| 6.  | Depression |   6.78 |  5.78 | −.10 |  | −.16 |  | −.13 |  | −.11 |  | .47 | \*\*\* | — |  |
| Note. Values are Spearman correlations. p-values are adjusted for multiple comparisons with Holm’s method ([Holm,](#ref-holmSimpleSequentiallyRejective1979)[ ](#ref-holmSimpleSequentiallyRejective1979)[1979](#ref-holmSimpleSequentiallyRejective1979)). |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
| \*\*\* p ≤ .001 |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Table 11

Spearman Rank Correlations Computed by the correlation Package

Here,
[`correlation::correlation`](https://easystats.github.io/correlation/reference/correlation.html)
adjusted the *p*-values with Holm’s method, its default, so the stars
are for the adjusted *p*-values, and the note says so. The means and
standard deviations come from the data that
[`correlation::correlation`](https://easystats.github.io/correlation/reference/correlation.html)
keeps with its results. Correlations computed by group, between two sets
of variables, or with Bayesian methods are not yet supported.

## References

Holm, S. (1979). A simple sequentially rejective multiple test
procedure. *Scandinavian Journal of Statistics*, *6*(2), 65–70.
<https://www.jstor.org/stable/4615733>

Revelle, W. (2019). *psychTools: Tools to accompany the psych package
for psychological research* (Version 2.6.4) \[Computer software\].
Comprehensive R Archive Network.
<https://doi.org/10.32614/CRAN.package.psychTools>
