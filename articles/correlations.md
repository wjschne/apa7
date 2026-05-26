# Correlations

``` r

library(apa7)
library(dplyr)
library(flextable)
library(ftExtra)
library(tidyr)
library(stringr)
```

Some tables, like correlation matrices, can be standardized and
automated. Others are uniquely tied to a specific purpose, and no
function could anticipate their structure. The apa7 package has a few
functions to automate what can be automated and a styling function to
make the final styling of a complex table a little easier.

By default, the output of `apa_cor`
[flextable](https://davidgohel.github.io/flextable/), with significant
correlations bolded.

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

Table 1. Correlation table using flextable via `apa_cor`

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

Table 2. Bolding significant correlations

## Styling options

Your styling options include:

- `note` A custom markdown formatted note. Overrides automatic note
  about bolded correlations being significant. Default: `NULL`
- `p_value` Significance value. Default: `.05`
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

``` r

# or a vector descriptives
my_summaries <- c("n", "M", "SD", "Skewness", "Kurtosis")

trees |>
  apa_cor(
    p_value = .001,
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
