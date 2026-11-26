# Building Tables by Hand

``` r

library(apa7)
library(flextable)
library(ftExtra)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
set_flextable_defaults(
  theme_fun = theme_apa,
  font.family = "Open Sans",
  text.align = "center",
  table_align = "left"
)
apa7_defaults(font_family = "Open Sans", line_spacing = 1.5)
```

## Summarizing by Hand

Functions that automate your descriptive statistics tables for you
(e.g., the amazing [gtsummary
package](https://www.danieldsjoberg.com/gtsummary/index.html))
inevitably entail some level of compromise. It is unreasonable for a
single function to anticipate the diversity of needs out there.
Sometimes you need a table to be a particular way, and you need to do
the heaving lifting yourself. After all that, the `apa_flextable`
function will get a flextable table close to APA style. You may need to
do some additional styling with flextable as well.

The flextable package helper functions for automating summary tables.
The `tabulator`, `summarizor`, and `proc_freq` functions are
particularly powerful ways of summarizing and describing data. However,
I am going to compute my summaries with standard base R and tidyverse
functions. Here I create a table of means, standard deviations, and
sample sizes for several variables across groups. The particulars are
not so important here, just that apa7 and flextable can do most of the
rest after that.

The `bfi` data set in the psychTools package ([Revelle,
2019](#ref-revellePsychToolsToolsAccompany2019)) has the answers of
2,800 people to 25 personality items, along with their education.
[`psych::scoreFast`](https://rdrr.io/pkg/psych/man/score.items.html)
averages the items of each Big Five scale (reversing the reverse-keyed
items), and the summaries are computed by education:

``` r

bfi_scales <- psych::scoreFast(psychTools::bfi.keys, psychTools::bfi)

d_bfi <- psychTools::bfi |>
  transmute(
    Education = factor(
      education,
      levels = 1:5,
      labels = c("Some high school", 
                 "High school graduate", 
                 "Some college",
                 "College graduate", 
                 "Graduate degree")
    ),
    Conscientiousness = bfi_scales[, "conscientious-A"],
    Neuroticism = bfi_scales[, "neuroticism-A"],
    Openness = bfi_scales[, "openness-A"]
  ) |>
  filter(!is.na(Education))

d_summary <- d_bfi |>
  arrange(Education) |>
  pivot_longer(where(is.numeric), names_to = "Variable") |>
  summarise(
    M = mean(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    n = n(),
    .by = c(Variable, Education)) |>
  pivot_longer(c(M, SD)) |>
  unite(Variable, Variable, name) |>
  pivot_wider(names_from = Variable)
```

The `apa_flextable` function handles the formatting automatically.
[Table 1](#tbl-apaflextable) decimal aligns all the numbers, though this
is apparent in column `n` only.

``` r

apa_flextable(d_summary)
```

| Education | n | Conscientiousness |  |  | Neuroticism |  |  | Openness |  |
|----|----|----|----|----|----|----|----|----|----|
|  |  | M | SD |  | M | SD |  | M | SD |
| Some high school |   224 | 4.12 | 0.94 |  | 3.26 | 1.21 |  | 4.55 | 0.84 |
| High school graduate |   292 | 4.23 | 0.99 |  | 3.23 | 1.28 |  | 4.61 | 0.82 |
| Some college | 1,249 | 4.39 | 0.91 |  | 3.13 | 1.20 |  | 4.51 | 0.78 |
| College graduate |   394 | 4.22 | 0.94 |  | 3.06 | 1.18 |  | 4.69 | 0.81 |
| Graduate degree |   418 | 4.28 | 0.98 |  | 3.06 | 1.11 |  | 4.83 | 0.79 |

Table 1

Conscientiousness, Neuroticism, and Openness by Education Using
`apa_flextable`

Functions such as `apa_parameters` and `apa_descriptives` make finished
tables from model objects and data. When no function makes the table you
need, apa7 can still do most of the work of turning a data frame of
results into an APA-style table. This article shows how, starting with
`apa_flextable` and ending with full control over every step.

## From a Data Frame to an APA Table

Suppose we have results we want to put in a table:

``` r

d <- tibble(
  Model = paste("Model", c(1, 1, 2, 2, 2)),
  Predictor = c(
    "Constant", "Socioeconomic status",
    "Constant", "Socioeconomic status", "Age"
  ),
  b = c(-4.5, 1.23, -5.1, 1.45, -.23),
  beta = c(NA, .24, NA, .31, .031),
  t = c(-18.457, 2.345, -22.457, 2.114, .854),
  df = c(85, 85, 84, 84, 84),
  p = c(.0001, .0245, .0001, .0341, .544)
)

d
#> # A tibble: 5 × 7
#>   Model   Predictor                b   beta       t    df      p
#>   <chr>   <chr>                <dbl>  <dbl>   <dbl> <dbl>  <dbl>
#> 1 Model 1 Constant             -4.5  NA     -18.5      85 0.0001
#> 2 Model 1 Socioeconomic status  1.23  0.24    2.35     85 0.0245
#> 3 Model 2 Constant             -5.1  NA     -22.5      84 0.0001
#> 4 Model 2 Socioeconomic status  1.45  0.31    2.11     84 0.0341
#> 5 Model 2 Age                  -0.23  0.031   0.854    84 0.544
```

With the flextable package’s APA theme (set in the setup chunk with
`set_flextable_defaults`), `flextable` gets us close:

``` r

flextable(d)
```

| Model   | Predictor            | b     | beta | t      | df    | p    |
|---------|----------------------|-------|------|--------|-------|------|
| Model 1 | Constant             | -4.50 |      | -18.46 | 85.00 | 0.00 |
| Model 1 | Socioeconomic status | 1.23  | 0.24 | 2.35   | 85.00 | 0.02 |
| Model 2 | Constant             | -5.10 |      | -22.46 | 84.00 | 0.00 |
| Model 2 | Socioeconomic status | 1.45  | 0.31 | 2.11   | 84.00 | 0.03 |
| Model 2 | Age                  | -0.23 | 0.03 | 0.85   | 84.00 | 0.54 |

Table 2

Regression Results as a Default flextable

Getting from “close” to “exactly right” requires a series of polishing
steps:

- Make `Model` into row titles (Model 1, Model 2).
- Left-align and widen the `Predictor` column.
- Use true minus signs (−) instead of hyphens.
- Align numbers on their decimals.
- Format *p*-values, and drop the leading zeros of `beta` and `p`.
- Italicize statistical symbols in the headers (e.g., `b` → *b*).

The flextable package has functions for each of these, but applying them
by hand is tedious. As a general-purpose package, flextable should not
be expected to know the rules of a particular style guide. The
`apa_flextable` function applies them all:

``` r

apa_flextable(d, row_title_column = Model)
```

| Predictor            | b     | β   | t      | df  | p      |
|----------------------|-------|-----|--------|-----|--------|
| Model 1              |       |     |        |     |        |
| Constant             | −4.50 |     | −18.46 | 85  | \<.001 |
| Socioeconomic status |  1.23 | .24 |   2.35 | 85  |  .02   |
| Model 2              |       |     |        |     |        |
| Constant             | −5.10 |     | −22.46 | 84  | \<.001 |
| Socioeconomic status |  1.45 | .31 |   2.11 | 84  |  .03   |
| Age                  | −0.23 | .03 |   0.85 | 84  |  .54   |

Table 3

Regression Results With Models as Row Titles

## Rows and Groups

Row titles can be aligned to the left, center, or right:

``` r

apa_flextable(d, row_title_column = Model, row_title_align = "center")
```

| Predictor            | b     | β   | t      | df  | p      |
|----------------------|-------|-----|--------|-----|--------|
| Model 1              |       |     |        |     |        |
| Constant             | −4.50 |     | −18.46 | 85  | \<.001 |
| Socioeconomic status |  1.23 | .24 |   2.35 | 85  |  .02   |
| Model 2              |       |     |        |     |        |
| Constant             | −5.10 |     | −22.46 | 84  | \<.001 |
| Socioeconomic status |  1.45 | .31 |   2.11 | 84  |  .03   |
| Age                  | −0.23 | .03 |   0.85 | 84  |  .54   |

Table 4

Regression Results With Centered Row Titles

Without row titles, groups can be shown with a vertically merged column
instead:

``` r

d |>
  mutate(Model = str_remove(Model, "Model ")) |>
  apa_flextable() |>
  merge_v() |>
  align(j = "Predictor", part = "all") |>
  align(j = "Model", align = "center") |>
  valign(j = "Model", valign = "middle") |>
  surround(i = 2, border.bottom = flextable::fp_border_default()) |>
  width(width = c(.8, 1.75, rep(.8, 5)))
```

| Model | Predictor            | b     | β   | t      | df  | p      |
|-------|----------------------|-------|-----|--------|-----|--------|
| 1     | Constant             | −4.50 |     | −18.46 | 85  | \<.001 |
|       | Socioeconomic status |  1.23 | .24 |   2.35 |     |  .02   |
| 2     | Constant             | −5.10 |     | −22.46 | 84  | \<.001 |
|       | Socioeconomic status |  1.45 | .31 |   2.11 |     |  .03   |
|       | Age                  | −0.23 | .03 |   0.85 |     |  .54   |

Table 5

Regression Results With Merged Model Cells

## Columns and Headers

### Column spanners and deckered heads

The `apa_flextable` function treats underscores in column names as
header levels: in `Openness_M`, “Openness” is a column spanner (a label
over several columns) and “M” is a deckered head (the label of one
column under it). Here are the means and standard deviations of the
personality scales from above, by education, without the sample sizes:

``` r

d_personality <- d_summary |>
  select(-n)

d_personality
#> # A tibble: 5 × 7
#>   Education            Conscientiousness_M Conscientiousness_SD Neuroticism_M
#>   <fct>                              <dbl>                <dbl>         <dbl>
#> 1 Some high school                    4.12                0.941          3.26
#> 2 High school graduate                4.23                0.985          3.23
#> 3 Some college                        4.39                0.908          3.13
#> 4 College graduate                    4.22                0.941          3.06
#> 5 Graduate degree                     4.28                0.980          3.06
#> # ℹ 3 more variables: Neuroticism_SD <dbl>, Openness_M <dbl>, Openness_SD <dbl>
```

``` r

apa_flextable(d_personality)
```

| Education | Conscientiousness |  |  | Neuroticism |  |  | Openness |  |
|----|----|----|----|----|----|----|----|----|
|  | M | SD |  | M | SD |  | M | SD |
| Some high school | 4.12 | 0.94 |  | 3.26 | 1.21 |  | 4.55 | 0.84 |
| High school graduate | 4.23 | 0.99 |  | 3.23 | 1.28 |  | 4.61 | 0.82 |
| Some college | 4.39 | 0.91 |  | 3.13 | 1.20 |  | 4.51 | 0.78 |
| College graduate | 4.22 | 0.94 |  | 3.06 | 1.18 |  | 4.69 | 0.81 |
| Graduate degree | 4.28 | 0.98 |  | 3.06 | 1.11 |  | 4.83 | 0.79 |

Table 6

Descriptive Statistics of Personality Scales by Education

`separate_headers = FALSE` turns this off. `column_spanner_label` adds
the same spanner to several columns, selected by name or with tidyselect
functions such as `starts_with`. The selected columns are moved next to
the first one (`relocate = FALSE` keeps them in place):

``` r

d |>
  column_spanner_label("Significance test", c(t, df, p)) |>
  column_spanner_label("Coefficients", starts_with("b")) |>
  apa_flextable(row_title_column = Model)
```

| Predictor            | Coefficients |     |     | Significance test |     |        |
|----------------------|--------------|-----|-----|-------------------|-----|--------|
|                      | b            | β   |     | t                 | df  | p      |
| Model 1              |              |     |     |                   |     |        |
| Constant             | −4.50        |     |     | −18.46            | 85  | \<.001 |
| Socioeconomic status |  1.23        | .24 |     |   2.35            | 85  |  .02   |
| Model 2              |              |     |     |                   |     |        |
| Constant             | −5.10        |     |     | −22.46            | 84  | \<.001 |
| Socioeconomic status |  1.45        | .31 |     |   2.11            | 84  |  .03   |
| Age                  | −0.23        | .03 |     |   0.85            | 84  |  .54   |

Table 7

Regression Results With Column Spanners

### Tables of cell statistics

`pivot_wider_name_first` is like
[`tidyr::pivot_wider`](https://tidyr.tidyverse.org/reference/pivot_wider.html),
but it puts the names first (e.g., `High_M` rather than `M_High`), so
the new columns get a spanner for each name. This makes tables of cell
statistics in a factorial design easy to build. In the `Moore` data set
in the carData package ([Moore & Krupat,
1971](#ref-mooreRelationshipSourceStatus1971)), participants made
judgments with a partner of low or high status, and the number of times
they conformed to their partner’s judgments was recorded. Their
authoritarianism was classified as low, medium, or high. Here are the
mean, standard deviation, and size of each cell, with a row for each
partner status and a spanner for each level of authoritarianism:

``` r

d_cells <- carData::Moore |>
  transmute(
    `Partner Status` = factor(partner.status,
                              levels = c("low", "high"),
                              labels = c("Low", "High")),
    Authoritarianism = factor(fcategory,
                              levels = c("low", "medium", "high"),
                              labels = c("Low", "Medium", "High")),
    Conformity = conformity
  ) |>
  summarise(
    M = mean(Conformity),
    SD = sd(Conformity),
    n = n(),
    .by = c(`Partner Status`, Authoritarianism)
  ) |>
  arrange(`Partner Status`, Authoritarianism)

d_cells
#>   Partner Status Authoritarianism        M       SD  n
#> 1            Low              Low  8.90000 2.643651 10
#> 2            Low           Medium  7.25000 3.947573  4
#> 3            Low             High 12.62500 7.347254  8
#> 4           High              Low 17.40000 4.505552  5
#> 5           High           Medium 14.27273 3.951985 11
#> 6           High             High 11.85714 3.933979  7
```

``` r

d_cells |>
  pivot_wider_name_first(
    names_from = Authoritarianism,
    values_from = c(M, SD, n)
  ) |>
  apa_flextable()
```

| Partner Status | Low   |      |     |     | Medium |      |     |     | High  |      |     |
|----------------|-------|------|-----|-----|--------|------|-----|-----|-------|------|-----|
|                | M     | SD   | n   |     | M      | SD   | n   |     | M     | SD   | n   |
| Low            |  8.90 | 2.64 | 10  |     |  7.25  | 3.95 |  4  |     | 12.62 | 7.35 | 8   |
| High           | 17.40 | 4.51 |  5  |     | 14.27  | 3.95 | 11  |     | 11.86 | 3.93 | 7   |

Table 8

Conformity by Partner Status and Authoritarianism

The spanners follow the order in which the levels first appear in the
data, so sort the data first. There is another example in [A Complete
Custom Table](#a-complete-custom-table) below. To count the cells of a
contingency table instead, use `apa_chisq` (see the [contingency tables
article](https://wjschne.github.io/apa7/articles/chisquare.md)). The
flextable package can also place statistics into cells:
[`tabulator`](https://davidgohel.github.io/flextable/reference/tabulator.html)
arranges precomputed statistics by row and column variables, and
[`proc_freq`](https://davidgohel.github.io/flextable/reference/proc_freq.html)
makes frequency tables with row, column, and total percentages. Their
output can be styled with `apa_style`.

### Break columns

By default, a narrow blank column separates groups of columns under
different spanners. `add_break_columns` inserts break columns yourself,
after any column selected by name or with a tidyselect function:

``` r

d_personality |>
  add_break_columns(ends_with("SD"), omit_last = TRUE) |>
  apa_flextable()
```

| Education | Conscientiousness |  |  | Neuroticism |  |  | Openness |  |
|----|----|----|----|----|----|----|----|----|
|  | M | SD |  | M | SD |  | M | SD |
| Some high school | 4.12 | 0.94 |  | 3.26 | 1.21 |  | 4.55 | 0.84 |
| High school graduate | 4.23 | 0.99 |  | 3.23 | 1.28 |  | 4.61 | 0.82 |
| Some college | 4.39 | 0.91 |  | 3.13 | 1.20 |  | 4.51 | 0.78 |
| College graduate | 4.22 | 0.94 |  | 3.06 | 1.18 |  | 4.69 | 0.81 |
| Graduate degree | 4.28 | 0.98 |  | 3.06 | 1.11 |  | 4.83 | 0.79 |

Table 9

Descriptive Statistics of Personality Scales With Break Columns

`apa_flextable` treats any column whose name begins with
`apa7breakcolumn` as a break column.

## Formatting Values

### Decimal alignment

`align_chr` rounds numbers (to .01 by default), replaces hyphens with
true minus signs, and pads numbers with figure spaces on both sides of
the decimal so that all values in a column have the same width:

``` r

tibble(x = align_chr(c(2.431, -0.4, -10, 101))) |>
  apa_flextable(table_width = .2) |>
  align(align = "center")
```

| x      |
|--------|
|   2.43 |
|  −0.40 |
| −10.00 |
| 101.00 |

Table 10

Numbers Aligned on the Decimal Point

Trailing zeros can be dropped, and leading zeros can be trimmed (for
statistics such as correlations that cannot exceed 1):

``` r

tibble(x = align_chr(c(2.431, -0.4, -10, 101),
                     drop0trailing = TRUE,
                     trim_leading_zeros = TRUE)) |>
  apa_flextable(table_width = .2) |>
  align(align = "center")
```

| x      |
|--------|
|   2.43 |
|   −.4  |
| −10    |
| 101    |

Table 11

Numbers Aligned Without Trailing or Leading Zeros

### Stars

`add_star_column` adds significance stars in a column of their own, so
the numbers stay aligned. The column to the left of the stars is
right-aligned, and the stars are left-aligned:

``` r

d_star <- tibble(
  Predictor = c("Constant", "Socioeconomic status"),
  b = c(.45, .55),
  p = c(.02, .0002)
) |>
  add_star_column(b, p = p)

apa_flextable(d_star)
```

| Predictor            | b    |        | p      |
|----------------------|------|--------|--------|
| Constant             | 0.45 | \*     |  .02   |
| Socioeconomic status | 0.55 | \*\*\* | \<.001 |

Table 12

Coefficients With a Star Column

If stars are already appended to numbers, `separate_star_column` moves
them into a column of their own:

``` r

tibble(
  Predictor = c("Constant", "Socioeconomic status"),
  b = c("1.10***", "2.32*"),
  beta = c(NA, .34)
) |>
  separate_star_column(b) |>
  apa_flextable()
```

| Predictor            | b    |        | β   |
|----------------------|------|--------|-----|
| Constant             | 1.10 | \*\*\* |     |
| Socioeconomic status | 2.32 | \*     | .34 |

Table 13

Coefficients With Stars in Their Own Column

`p2stars` converts *p*-values to stars, and `apa_p_star_note` writes the
note that explains them.

### Hanging indents and numbered lists

flextable has no hanging indent, so `hanging_indent` wraps text and
indents the continuation lines with figure spaces:

``` r

d_quote <- tibble(
  Quote = c(
    "Believe those who are seeking the truth. Doubt those who find it.",
    "Resentment is like drinking poison and waiting for the other person to die.",
    "Advice is what we ask for when we already know the answer but wish we didn’t.",
    "Do not ask whether a statement is true until you know what it means.",
    "Tact is the art of making a point without making an enemy.",
    "Short cuts make long delays.",
    "There is a stubbornness about me that never can bear to be frightened at the will of others. My courage always rises at every attempt to intimidate me.",
    "There is a crack in everything, that’s how the light gets in."
  ),
  Attribution = c(
    "André Gide",
    "Carrie Fisher",
    "Erica Jong",
    "Errett Bishop",
    "Howard W. Newton",
    "J. R. R. Tolkien",
    "Jane Austen",
    "Leonard Cohen"
  )
) |>
  arrange(nchar(Quote))

d_quote |>
  mutate(Quote = hanging_indent(Quote, width = 55, indent = 5)) |>
  apa_flextable() |>
  align(j = "Attribution", part = "all") |>
  width(width = c(4.5, 2))
```

[TABLE]

Table 14

Quotations With Hanging Indents

`add_list_column` numbers the rows. Set `type` to `"A"`, `"a"`, `"I"`,
or `"i"` for letters or Roman numerals:

``` r

d_quote |>
  add_list_column(Quote, type = "A", sep = ") ") |>
  apa_flextable() |>
  align(j = "Attribution", part = "all") |>
  width(width = c(.3, 4.5, 1.7))
```

|  | Quote | Attribution |
|----|----|----|
| A) | Short cuts make long delays. | J. R. R. Tolkien |
| B) | Tact is the art of making a point without making an enemy. | Howard W. Newton |
| C) | There is a crack in everything, that’s how the light gets in. | Leonard Cohen |
| D) | Believe those who are seeking the truth. Doubt those who find it. | André Gide |
| E) | Do not ask whether a statement is true until you know what it means. | Errett Bishop |
| F) | Resentment is like drinking poison and waiting for the other person to die. | Carrie Fisher |
| G) | Advice is what we ask for when we already know the answer but wish we didn’t. | Erica Jong |
| H) | There is a stubbornness about me that never can bear to be frightened at the will of others. My courage always rises at every attempt to intimidate me. | Jane Austen |

Table 15

Quotations as a Lettered List

## Automatic Column Formatting

`apa_flextable` formats the columns and headers it recognizes (e.g.,
`b`, `beta`, `t`, `df`, and `p`). To turn this off:

``` r

apa_flextable(d, row_title_column = Model, auto_format_columns = FALSE)
```

| Predictor            | b     | beta | t      | df  | p    |
|----------------------|-------|------|--------|-----|------|
| Model 1              |       |      |        |     |      |
| Constant             | -4.50 |      | -18.46 | 85  | 0.00 |
| Socioeconomic status | 1.23  | 0.24 | 2.35   | 85  | 0.02 |
| Model 2              |       |      |        |     |      |
| Constant             | -5.10 |      | -22.46 | 84  | 0.00 |
| Socioeconomic status | 1.45  | 0.31 | 2.11   | 84  | 0.03 |
| Age                  | -0.23 | 0.03 | 0.85   | 84  | 0.54 |

Table 16

Regression Results Without Automatic Column Formatting

Each recognized column has a `column_format`: its header (in markdown),
its LaTeX header, a function that formats its values, and an optional
definition for table notes. To change how a column is shown, make a new
format. Here, any column named `Predictor` is relabeled “Variable” and
shown in capitals:

``` r

cf_predictor <- column_format(
  name = "Predictor",
  header = "Variable",
  latex = "Variable",
  formatter = stringr::str_to_upper
)
```

`column_formats` makes the full set of default formats, which can be
changed. Here, numbers are rounded to .001, the new `Predictor` format
is added, and `beta` is no longer formatted:

``` r

my_formats <- column_formats(accuracy = .001)
my_formats$Predictor <- cf_predictor
my_formats$beta <- NULL

apa_flextable(d, row_title_column = Model, column_formats = my_formats)
```

| Variable             | b      | beta | t       | df  | p      |
|----------------------|--------|------|---------|-----|--------|
| Model 1              |        |      |         |     |        |
| CONSTANT             | −4.500 |      | −18.457 | 85  | \<.001 |
| SOCIOECONOMIC STATUS |  1.230 | 0.24 |   2.345 | 85  |  .025  |
| Model 2              |        |      |         |     |        |
| CONSTANT             | −5.100 |      | −22.457 | 84  | \<.001 |
| SOCIOECONOMIC STATUS |  1.450 | 0.31 |   2.114 | 84  |  .034  |
| AGE                  | −0.230 | 0.03 |   0.854 | 84  |  .544  |

Table 17

Regression Results With Custom Column Formats

To use the formats in every table, set them with
`apa7_defaults(column_formats = my_formats)`. These are the default
formats:

``` r

column_formats()@get_tibble |>
  select(-formatter) |>
  arrange(name, .locale = "en") |>
  apa_flextable(markdown_body = FALSE)
```

| name | header | latex |
|----|----|----|
| AIC | \*AIC\* | \$AIC\$ |
| AIC_wt | \*AIC\* weight | \$AIC\$ weight |
| AICc | \*AICc\* | \$AICc\$ |
| AICc_wt | \*AICc\* weight | \$AICc\$ weight |
| alpha | &alpha; | \$\alpha\$ |
| b | \*b\* | \$b\$ |
| B | \*B\* | \$B\$ |
| beta | &beta; | \$\beta\$ |
| BIC | \*BIC\* | \$BIC\$ |
| BIC_wt | \*BIC\* weight | \$BIC\$ weight |
| BICc | \*BICc\* | \$BICc\$ |
| CFI | CFI | CFI |
| Chi2 | \*&chi;\*^2^ | \$\chi^2\$ |
| Chi2 value | \*&chi;\*^2^ | \$\chi^2\$ |
| Chi2_df | \*df\* | \$df\$ |
| chisq | \*&chi;\*^2^ | \$\chi^2\$ |
| Chisq | \*&chi;\*^2^ | \$\chi^2\$ |
| CI | {round(ci \* 100)}% CI | {round(ci \* 100)}\\ CI |
| CI_high | UL | UL |
| CI_low | LL | LL |
| CI_percent | {p}% CI | {p}\\ CI |
| Coefficient | \*B\* | \$B\$ |
| cohens_d | Cohen's \*d\* | Cohen's \$d\$ |
| Cohens_d | Cohen's \*d\* | Cohen's \$d\$ |
| Cohens_w | Cohen's \*w\* | Cohen's \$w\$ |
| Complexity | Complexity | Complexity |
| Cramers_v | Cramér's \*V\* | Cramér's \$V\$ |
| cronbach | Cronbach's &alpha; | Cronbach's \$\alpha\$ |
| d_av | \*d\*~av~ | \$d\_{\text{av}}\$ |
| d_rm | \*d\*~rm~ | \$d\_{\text{rm}}\$ |
| d_z | \*d\*~z~ | \$d\_{\text{z}}\$ |
| deltaAIC | &Delta;\*AIC\* | \$\Delta AIC\$ |
| deltaBIC | &Delta;\*BIC\* | \$\Delta BIC\$ |
| deltachi2 | &Delta;&chi;^2^ | \$\Delta \chi^2\$ |
| deltaR2 | &Delta;\*R\*^2^ | \$\Delta R^2\$ |
| deltaR2_conditional | &Delta;\*R\*^2^~conditional~ | \$\Delta R^2\_{\text{conditional}}\$ |
| df | \*df\* | \$df\$ |
| df_diff | &Delta;\*df\* | \$\Delta df\$ |
| df_error | \*df\* | \$df\$ |
| edf | \*edf\* | \$edf\$ |
| Epsilon2 | \*&epsilon;\*^2^ | \$\epsilon^2\$ |
| Epsilon2_partial | \*&epsilon;\*^2^~p~ | \$\epsilon^2_p\$ |
| eta2 | \*&eta;\*^2^ | \$\eta^2\$ |
| Eta2 | \*&eta;\*^2^ | \$\eta^2\$ |
| Eta2_generalized | \*&eta;\*^2^~G~ | \$\eta^2_G\$ |
| Eta2_partial | \*&eta;\*^2^~p~ | \$\eta^2_p\$ |
| Exp_CI | {round(ci \* 100)}% CI for Exp(\*B\*) | {round(ci \* 100)}\\ CI for Exp(\$B\$) |
| Exp_Coefficient | Exp(\*B\*) | Exp(\$B\$) |
| F | \*F\* | \$F\$ |
| g_av | \*g\*~av~ | \$g\_{\text{av}}\$ |
| g_rm | \*g\*~rm~ | \$g\_{\text{rm}}\$ |
| g_z | \*g\*~z~ | \$g\_{\text{z}}\$ |
| h2 | \*h\*^2^ | \$h^2\$ |
| Hedges_g | Hedges' \*g\* | Hedges' \$g\$ |
| ICC | ICC | ICC |
| m | \*m\* | \$m\$ |
| M | \*M\* | \$M\$ |
| Mdn | \*Mdn\* | \$Mdn\$ |
| Mean | \*Mean\* | \$Mean\$ |
| Mean_Square | \*MS\* | \$MS\$ |
| n | \*n\* | \$n\$ |
| N | \*N\* | \$N\$ |
| NFI | NFI | NFI |
| omega | &omega; | \$\omega\$ |
| Omega2 | \*&omega;\*^2^ | \$\omega^2\$ |
| Omega2_partial | \*&omega;\*^2^~p~ | \$\omega^2_p\$ |
| p | \*p\* | \$p\$ |
| p_Chi2 | \*p\* | \$p\$ |
| Parameter | Variable | Variable |
| phi | &phi; | \$\phi\$ |
| Phi | &Phi; | \$\Phi\$ |
| r | \*r\* | \$r\$ |
| r_rank_biserial | \*r\*~rb~ | \$r\_{\text{rb}}\$ |
| R2 | \*R\*^2^ | \$R^2\$ |
| R2_adjusted | adj\*R\*^2^ | \$\text{adj}R^2\$ |
| R2_conditional | \*R\*^2^~conditional~ | \$R^2\_{\text{conditional}}\$ |
| R2_marginal | \*R\*^2^~marginal~ | \$R^2\_{\text{marginal}}\$ |
| rho | &rho; | \$\rho\$ |
| RMSE | \*RMSE\* | \$RMSE\$ |
| RMSEA | RMSEA | RMSEA |
| s | \*s\* | \$s\$ |
| SD | \*SD\* | \$SD\$ |
| SE | \*SE\* | \$SE\$ |
| SE_B | \*SE_B\* | \$SE~B\$ |
| Sigma | &sigma;~\*e\*~ | \$\sigma\_{e}\$ |
| SRMR | SRMR | SRMR |
| Std_Coefficient | &beta; | \$\beta\$ |
| Sum_Squares | \*SS\* | \$SS\$ |
| t | \*t\* | \$t\$ |
| t_df | \*t\*({df}) | \$t\$({df}) |
| tau | &tau; | \$\tau\$ |
| TLI | TLI | TLI |
| u2 | \*u\*^2^ | \$u^2\$ |
| V | \*V\* | \$V\$ |
| Variable | Variable | Variable |
| W | \*W\* | \$W\$ |
| z | \*z\* | \$z\$ |

Table 18

Default Column Formats in apa7

## Changing a Finished Table

`apa_flextable` returns a flextable, so any flextable function can
change it afterward. For example, to bold one cell:

``` r

apa_flextable(d, row_title_column = Model) |>
  bold(i = 3, j = 3)
```

| Predictor            | b     | β   | t      | df  | p      |
|----------------------|-------|-----|--------|-----|--------|
| Model 1              |       |     |        |     |        |
| Constant             | −4.50 |     | −18.46 | 85  | \<.001 |
| Socioeconomic status |  1.23 | .24 |   2.35 | 85  |  .02   |
| Model 2              |       |     |        |     |        |
| Constant             | −5.10 |     | −22.46 | 84  | \<.001 |
| Socioeconomic status |  1.45 | .31 |   2.11 | 84  |  .03   |
| Age                  | −0.23 | .03 |   0.85 | 84  |  .54   |

Table 19

Regression Results With One Cell in Bold

These flextable functions solve common formatting problems. Each can be
applied to selected rows (including rows chosen by a condition on the
data) and columns:

| Function | Purpose |
|----|----|
| Text |  |
| [bold](https://davidgohel.github.io/flextable/reference/bold.html) | Bold text |
| [color](https://davidgohel.github.io/flextable/reference/color.html) | Color text |
| [font](https://davidgohel.github.io/flextable/reference/font.html) | Font family |
| [fontsize](https://davidgohel.github.io/flextable/reference/fontsize.html) | Font size |
| [highlight](https://davidgohel.github.io/flextable/reference/highlight.html) | Highlight color |
| [italic](https://davidgohel.github.io/flextable/reference/italic.html) | Italicize text |
| Cell |  |
| [align](https://davidgohel.github.io/flextable/reference/align.html) | Horizontal alignment |
| [bg](https://davidgohel.github.io/flextable/reference/bg.html) | Background color |
| [line_spacing](https://davidgohel.github.io/flextable/reference/line_spacing.html) | Line spacing |
| [padding](https://davidgohel.github.io/flextable/reference/padding.html) | Cell padding |
| [rotate](https://davidgohel.github.io/flextable/reference/rotate.html) | Rotate text |
| [surround](https://davidgohel.github.io/flextable/reference/surround.html) | Cell borders |
| [valign](https://davidgohel.github.io/flextable/reference/valign.html) | Vertical alignment |
| [width](https://davidgohel.github.io/flextable/reference/width.html) | Column width |

Table 20

Common flextable Formatting Functions

## A Complete Custom Table

Here the unstandardized and standardized coefficients of the two models
are side by side, with stars appended to the unstandardized coefficients
and a note below the table:

``` r

d |>
  # Decimal-align b and append stars
  mutate(b = paste0(align_chr(b), p2stars(p))) |>
  select(-c(t, df, p)) |>
  # One column for each statistic in each model
  pivot_wider_name_first(names_from = Model, values_from = c(b, beta)) |>
  apa_flextable() |>
  add_footer_lines(
    values = as_paragraph_md(
      c(
        paste(
          "*Note*. *b* = unstandardized regression coefficient.",
          "&beta; = standardized regression coefficient."
        ),
        apa_p_star_note()
      )
    )
  ) |>
  align(part = "footer", align = "left") 
```

| Predictor | Model 1 |  |  | Model 2 |  |
|----|----|----|----|----|----|
|  | b | β |  | b | β |
| Constant | −4.50\*\*\* |  |  | −5.10\*\*\* |  |
| Socioeconomic status |  1.23\*     | .24 |  |  1.45\*     | .31 |
| Age |  |  |  | −0.23        | .03 |
| Note. b = unstandardized regression coefficient. β = standardized regression coefficient. |  |  |  |  |  |
| \* p ≤ .05. \*\* p ≤ .01. \*\*\* p ≤ .001 |  |  |  |  |  |

Table 21

Coefficients of Two Models Side by Side

## Inside `apa_flextable`

The `apa_flextable` function does more than might be apparent at first
glance. To understand what it does under the hood, I have outlined the
major steps in changing a data frame into a flextable in APA format.
With a few minor details omitted, it does roughly this:

1.  Adds a break column between adjacent column spanners, if
    `add_breaks_between_spanners = TRUE` and `separate_headers = TRUE`.
    A break column is a narrow blank column without borders, so the
    rules under adjacent spanners do not touch.
2.  If `auto_format_columns = TRUE`, turns columns of whole numbers
    (e.g., degrees of freedom) into integers so that they have no
    decimals. Then formats the columns with `apa_format_columns`.
    Columns named in `no_format_columns`, or in a `"no_format_columns"`
    attribute of the data (set by functions such as `apa_loadings`), are
    left as they are.
3.  Makes the flextable. With `row_title_column`, it first restructures
    the data with
    [`flextable::as_grouped_data`](https://davidgohel.github.io/flextable/reference/as_grouped_data.html),
    and each row title gets a rule above it (`row_title_border`).
4.  Separates the headers into spanners and deckered heads, if
    `separate_headers = TRUE`.
5.  Left-aligns the first column and centers the columns of numbers.
6.  Applies `apa_style` to style the table and convert markdown.
7.  Aligns the row titles (`row_title_align`) and indents the rows under
    them.
8.  Sets the column widths with `pretty_widths`.
9.  Lines up numbers padded by `align_chr` on their decimal points.
10. Draws a rule above the rows named in a `"rule_above"` attribute of
    the data (set by functions such as `apa_loadings` and `apa_chisq`).
11. Adds the note, if `note` is given. It is added last so that a long
    note does not widen the columns.

Most of these steps can also be applied one at a time. Here they are
with column spanners added for illustration (steps 9 through 11 are not
included, because they use functions inside apa7):

``` r

d |>
  # Column spanners
  rename_with(.cols = c(b, beta), \(x) paste0("Coefficients_", x)) |>
  rename_with(.cols = c(t, df, p), \(x) paste0("Significance Test_", x)) |>
  # Step 1: Space between column spanners
  add_break_columns(ends_with("beta")) |>
  # Step 2: Whole numbers as integers, then format the columns
  mutate(across(where(rlang::is_integerish), as.integer)) |>
  apa_format_columns() |>
  # Step 3: Row titles and the flextable
  flextable::as_grouped_data("Model") |>
  mutate(row_title = Model, .before = 1) |>
  fill(Model) %>%
  flextable(col_keys = colnames(select(., -Model, -row_title))) |>
  mk_par(i = ~ !is.na(row_title), value = as_paragraph(row_title)) |>
  merge_h(i = ~ !is.na(row_title)) |>
  surround(
    i = ~ !is.na(row_title),
    border.top = list(color = "gray20", style = "solid", width = 1)
  ) |>
  # Step 4: Spanners and deckered heads
  flextable::separate_header() |>
  # Step 5: Alignment
  align(j = 1, part = "all") |>
  # Step 6: Style the table and convert markdown
  apa_style() |>
  # Step 7: Row titles centered, other rows left-aligned
  align(j = 1, i = ~ is.na(row_title)) |>
  align(i = ~ !is.na(row_title), align = "center") |>
  # Step 8: Column widths
  pretty_widths()
```

| Predictor            | Coefficients |     |     | Significance Test |     |        |
|----------------------|--------------|-----|-----|-------------------|-----|--------|
|                      | b            | β   |     | t                 | df  | p      |
| Model 1              |              |     |     |                   |     |        |
| Constant             | −4.50        |     |     | −18.46            | 85  | \<.001 |
| Socioeconomic status |  1.23        | .24 |     |   2.35            | 85  |  .02   |
| Model 2              |              |     |     |                   |     |        |
| Constant             | −5.10        |     |     | −22.46            | 84  | \<.001 |
| Socioeconomic status |  1.45        | .31 |     |   2.11            | 84  |  .03   |
| Age                  | −0.23        | .03 |     |   0.85            | 84  |  .54   |

Table 22

Regression Results Grouped by Model With Column Spanners

## Speed

By default, `apa_flextable` converts markdown in every cell (with
[`ftExtra::colformat_md`](https://ftextra.atusy.net/reference/colformat_md.html)).
This makes formatting easy and consistent, but it is a little slow, and
a document with many tables can take a while to render.
`markdown_body = FALSE` skips the conversion in the body of a table
whose cells have no markdown, and caching the chunks of finished tables
(with the `cache` chunk option) avoids remaking them each time the
document is rendered.

## References

Moore, J. C., & Krupat, E. (1971). Relationship between source status,
authoritarianism, and conformity in a social influence setting.
*Sociometry*, *34*(1), 122–134. <https://doi.org/10.2307/2786355>

Revelle, W. (2019). *psychTools: Tools to accompany the psych package
for psychological research* (Version 2.6.4) \[Computer software\].
Comprehensive R Archive Network.
<https://doi.org/10.32614/CRAN.package.psychTools>
