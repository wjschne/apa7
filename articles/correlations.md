# Correlations

``` r

library(apa7)
library(dplyr)
library(flextable)
library(ftExtra)
library(tidyr)
library(stringr)
library(correlation)
```

Some tables, like correlation matrices, can be standardized and
automated. Others are uniquely tied to a specific purpose, and no
function could anticipate their structure. The apa7 package has a few
functions to automate what can be automated and a styling function to
make the final styling of a complex table a little easier.

By default, the output of `apa_cor`
[flextable](https://davidgohel.github.io/flextable/), with significant
correlations starred.

``` r

trees |> 
  apa_cor()
```

| Variable                        |        | M     | SD    | 1   |        | 2   |        | 3   |     |
|---------------------------------|--------|-------|-------|-----|--------|-----|--------|-----|-----|
| 1.                              | Girth  | 13.25 |  3.14 | —   |        |     |        |     |     |
| 2.                              | Height | 76.00 |  6.37 | .52 | \*\*   | —   |        |     |     |
| 3.                              | Volume | 30.17 | 16.44 | .97 | \*\*\* | .60 | \*\*\* | —   |     |
| \*\* p \< .01. \*\*\* p \< .001 |        |       |       |     |        |     |        |     |     |

Table 1: Correlation Table

Significant correlations can be bolded instead.

``` r

trees |>
  apa_cor(bold_significant = TRUE)
```

| Variable |  | M | SD | 1 | 2 | 3 |
|----|----|----|----|----|----|----|
| 1.  | Girth | 13.25 |  3.14 | — |  |  |
| 2.  | Height | 76.00 |  6.37 | .52 | — |  |
| 3.  | Volume | 30.17 | 16.44 | .97 | .60 | — |
| Note. Correlations significant at p \< .05 are bolded. |  |  |  |  |  |  |

Table 2: Bolding Significant Correlations

## Summary functions

You can include any named list of summary functions:

``` r

# Named list of descriptive functions
my_summaries <- list(Median = median, "Interquartile Range" = IQR)

apa_cor(trees, summary_functions = my_summaries)
```

| Variable |  | Median | Interquartile Range | 1 |  | 2 |  | 3 |  |
|----|----|----|----|----|----|----|----|----|----|
| 1.  | Girth | 12.90 | 4.20 | — |  |  |  |  |  |
| 2.  | Height | 76.00 | 8.00 | .52 | \*\* | — |  |  |  |
| 3.  | Volume | 24.20 | 17.90 | .97 | \*\*\* | .60 | \*\*\* | — |  |
| \*\* p \< .01. \*\*\* p \< .001 |  |  |  |  |  |  |  |  |  |

You can also use a plain vector of descriptive functions from these
options:
`"IQR", "Interquartile Range", "Kurtosis", "MAD", "Median Absolute Deviation", "M", "Mean", "Med", "Median", "n", "N", "Quantile", "Range", "SD", "Skewness", "Var", "Variance"`

``` r

my_summaries <- c("n", "M", "SD", "Skewness", "Kurtosis")

apa_cor(trees, summary_functions = my_summaries)
```

| Variable |  | n | M | SD | Skewness | Kurtosis | 1 |  | 2 |  | 3 |  |
|----|----|----|----|----|----|----|----|----|----|----|----|----|
| 1.  | Girth | 31 | 13.25 |  3.14 | 0.50 | −0.71 | — |  |  |  |  |  |
| 2.  | Height | 31 | 76.00 |  6.37 | −0.36 | −0.72 | .52 | \*\* | — |  |  |  |
| 3.  | Volume | 31 | 30.17 | 16.44 | 1.01 | 0.25 | .97 | \*\*\* | .60 | \*\*\* | — |  |
| \*\* p \< .01. \*\*\* p \< .001 |  |  |  |  |  |  |  |  |  |  |  |  |

## Styling options

Your styling options include:

- `note` A custom markdown formatted note. Overrides automatic note
  about bolded correlations being significant. Default: `NULL`
- `p_value` Significance value. Default: `0.05`
- `digits` Number of digits for rounding. Default: `2`
- `bold_significant` Whether to bold significant correlations. Default:
  `FALSE`
- `star_significant` Whether to star significant correlations. Default:
  `TRUE`
- `significance_note` Whether note about bolding correlations appears.
  Overridden by a custom note. Also, the automatic note will not appear
  if no correlations are significant. Default: `TRUE`
- `output` Options are `flextable` or `tibble`. Default: `flextable`
- `family` Font family (typeface). Default: `Times New Roman`
- `font_size` Font size (in points). Default: `12`
- `text_color` Text color. Default: `black`
- `border_color` Table border color. Default: `black`
- `border_width` Table border width. Default: `0.5`
- `line_spacing` Text line spacing. Default: `2`

For example,

``` r

trees |>
  apa_cor(
    p_value = 0.001,
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

| Variable |  | n | M | SD | Skewness | Kurtosis | 1 | 2 | 3 |
|----|----|----|----|----|----|----|----|----|----|
| 1.  | Girth | 31 | 13.25 |  3.14 | 0.50 | −0.71 | — |  |  |
| 2.  | Height | 31 | 76.00 |  6.37 | −0.36 | −0.72 | .52 | — |  |
| 3.  | Volume | 31 | 30.17 | 16.44 | 1.01 | 0.25 | .97 | .60 | — |
| Note. Correlations significant at p \< .001 are bolded. |  |  |  |  |  |  |  |  |  |

Table 3: Alternative Styling

To remove summary statistics entirely, set `summary_functions` to `NA`
or `NULL`.

``` r

apa_cor(trees, summary_functions = NA, bold_significant = FALSE)
```

| Variable                        |        | 1   |        | 2   |        | 3   |     |
|---------------------------------|--------|-----|--------|-----|--------|-----|-----|
| 1.                              | Girth  | —   |        |     |        |     |     |
| 2.                              | Height | .52 | \*\*   | —   |        |     |     |
| 3.                              | Volume | .97 | \*\*\* | .60 | \*\*\* | —   |     |
| \*\* p \< .01. \*\*\* p \< .001 |        |     |        |     |        |     |     |

Table 4: Removing Summary Functions

## Customization

The styling options for `apa_cor` are minimal, but flextable allows for
all kinds of customization. Just place whatever code from flextable you
want after `apa_cor`. Here I set the width to 50% and align the table to
the left of the page. I place a footnote in a specific cell in the table
body.

``` r

apa_cor(trees) |>
  flextable::footnote(
    i = 2,
    j = "Variable",
    value = flextable::as_paragraph("This is a footnote."),
    part = "body"
  ) |>
  flextable::set_table_properties(
    width = .5,
    align = "left"
  )
```

| Variable                        |         | M     | SD    | 1   |        | 2   |        | 3   |     |
|---------------------------------|---------|-------|-------|-----|--------|-----|--------|-----|-----|
| 1.                              | Girth   | 13.25 |  3.14 | —   |        |     |        |     |     |
| 2.                              | Height1 | 76.00 |  6.37 | .52 | \*\*   | —   |        |     |     |
| 3.                              | Volume  | 30.17 | 16.44 | .97 | \*\*\* | .60 | \*\*\* | —   |     |
| \*\* p \< .01. \*\*\* p \< .001 |         |       |       |     |        |     |        |     |     |
| 1This is a footnote.            |         |       |       |     |        |     |        |     |     |

Table 5: Styling a Table with flextable Functions

## Output to tibble

If you need to process the underlying data before converting to
flextable (or use another table-creating function from another package),
you can output a tibble and then process the table however you want.
Here we add a row title.

``` r

apa_cor(trees, output = "tibble") |>
  mutate(apa7listcolumn = str_trim(apa7listcolumn)) |>
  unite(col = Variable, 
        apa7listcolumn, 
        Variable, 
        sep = "&thinsp;") |>
  add_row(Variable = "Row title", .before = 1) |>
  apa_flextable(no_format_columns = "Variable") |>
  padding(i = 2:4,
          j = 1,
          padding.left = 15) 
```

| Variable  | M     | SD    | 1   |        | 2   |        | 3   |     |
|-----------|-------|-------|-----|--------|-----|--------|-----|-----|
| Row title |       |       |     |        |     |        |     |     |
| 1. Girth  | 13.25 |  3.14 | —   |        |     |        |     |     |
| 2. Height | 76.00 |  6.37 | .52 | \*\*   | —   |        |     |     |
| 3. Volume | 30.17 | 16.44 | .97 | \*\*\* | .60 | \*\*\* | —   |     |

Table 6: Customizing a Table with flextable Functions.

## Correlations from the correlation package

`apa_cor` computes Pearson correlations with
[`psych::corTest`](https://rdrr.io/pkg/psych/man/corr.test.html). For
other kinds of correlations, compute them with
[`correlation::correlation`](https://easystats.github.io/correlation/reference/correlation.html)
from the [correlation](https://easystats.github.io/correlation/)
package, and give the result to `apa_cor`. For example, Spearman rank
correlations:

``` r

correlation(trees, method = "spearman") |>
  apa_cor()
```

| Variable |  | M | SD | 1 |  | 2 |  | 3 |  |
|----|----|----|----|----|----|----|----|----|----|
| 1.  | Girth | 13.25 |  3.14 | — |  |  |  |  |  |
| 2.  | Height | 76.00 |  6.37 | .44 | \* | — |  |  |  |
| 3.  | Volume | 30.17 | 16.44 | .95 | \*\*\* | .58 | \*\* | — |  |
| Note. Values are Spearman correlations. p-values are adjusted for multiple comparisons with Holm’s method (Holm, 1979). |  |  |  |  |  |  |  |  |  |
| \* p \< .05. \*\* p \< .01. \*\*\* p \< .001 |  |  |  |  |  |  |  |  |  |

Table 7: Spearman rank correlations from the correlation package

The note says what kind of correlations are shown. Other options from
the
[`correlation::correlation`](https://easystats.github.io/correlation/reference/correlation.html)
function include:

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

By default,
[`correlation::correlation`](https://easystats.github.io/correlation/reference/correlation.html)
adjusts the *p*-values for multiple comparisons with Holm’s method, so
the stars are for the adjusted *p*-values, and the note says so (use
`p_adjust = "none"` for unadjusted *p*-values). Partial correlations,
each controlling for the other variables in the table, work the same
way:

``` r

correlation(trees, partial = TRUE, p_adjust = "none") |>
  apa_cor()
```

| Variable |  | M | SD | 1 |  | 2 |  | 3 |  |
|----|----|----|----|----|----|----|----|----|----|
| 1.  | Girth | 13.25 |  3.14 | — |  |  |  |  |  |
| 2.  | Height | 76.00 |  6.37 | −.29 |  | — |  |  |  |
| 3.  | Volume | 30.17 | 16.44 | .96 | \*\*\* | .44 | \* | — |  |
| Note. Values are partial Pearson correlations, each controlling for all other variables in the table. |  |  |  |  |  |  |  |  |  |
| \* p \< .05. \*\*\* p \< .001 |  |  |  |  |  |  |  |  |  |

Table 8: Partial correlations from the correlation package

The means and standard deviations come from the data that
[`correlation::correlation`](https://easystats.github.io/correlation/reference/correlation.html)
keeps with its results. Correlations computed by group, between two sets
of variables, or with Bayesian methods are not yet supported.
