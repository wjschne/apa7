# Contingency Tables

``` r

library(apa7)
library(dplyr)
library(tidyr)
library(flextable)
library(ftExtra)
```

## Data

``` r

d <- mtcars |>
  select(Gears = gear, Transmission = am) |>
  mutate(Transmission = factor(Transmission, labels = c("Automatic", "Manual")))
```

## The `apa_chisq` contingency table function

The default output of the `apa_chisq` function is a contingency table
with a chi-square test of independence in the table note. The output is
a flextable, which can be modified with flextable commands.

``` r

apa_chisq(d)
```

| Transmission | 3 |  | 4 |  | 5 |  |
|----|----|----|----|----|----|----|
|  | n | % | n | % | n | % |
| Automatic | 15 | 100.0% | 4 | 33.3% | 0 | 0.0% |
| Manual |  0 | 0.0% | 8 | 66.7% | 5 | 100.0% |
| Note. χ2 (2) = 20.94, p \< .001, Adj. Cramer’s V = .78 |  |  |  |  |  |  |

## Styling Options

The table can be styled in many ways:

``` r

apa_chisq(
  d,
  font_size = 16,
  line_spacing = 3,
  text_color = "darkred",
  border_width = 2,
  border_color = "gray",
  font_family = "Arial"
)
```

| Transmission | 3 |  | 4 |  | 5 |  |
|----|----|----|----|----|----|----|
|  | n | % | n | % | n | % |
| Automatic | 15 | 100.0% | 4 | 33.3% | 0 | 0.0% |
| Manual |  0 | 0.0% | 8 | 66.7% | 5 | 100.0% |
| Note. χ2 (2) = 20.94, p \< .001, Adj. Cramer’s V = .78 |  |  |  |  |  |  |

``` r

# No note
apa_chisq(d, note = NA)
```

| Transmission | 3   |        | 4   |       | 5   |        |
|--------------|-----|--------|-----|-------|-----|--------|
|              | n   | %      | n   | %     | n   | %      |
| Automatic    | 15  | 100.0% | 4   | 33.3% | 0   | 0.0%   |
| Manual       |  0  | 0.0%   | 8   | 66.7% | 5   | 100.0% |

``` r

# Custom note
apa_chisq(
  d,
  note = "This is a *custom* note written in **markdown** $x > \\omega$."
)
```

| Transmission | 3 |  | 4 |  | 5 |  |
|----|----|----|----|----|----|----|
|  | n | % | n | % | n | % |
| Automatic | 15 | 100.0% | 4 | 33.3% | 0 | 0.0% |
| Manual |  0 | 0.0% | 8 | 66.7% | 5 | 100.0% |
| Note. This is a custom note written in markdown x \> ω. |  |  |  |  |  |  |

## What if I want something completely different?

There are a lot of options out there. The flextable package has the
`proc_freq` function, which has the ability to include row, column, and
total percentages in the table.

``` r

mtcars %>%
  proc_freq(row = "gear", col = "vs") %>%
  theme_apa()
```

| gear |  | vs |  |  |
|----|----|----|----|----|
|  |  | 0 | 1 | Total |
| 3 | Count | 12 (37.5%) | 3 (9.4%) | 15 (46.9%) |
|  | Mar. pct (1) | 66.7% ; 80.0% | 21.4% ; 20.0% |  |
| 4 | Count | 2 (6.2%) | 10 (31.2%) | 12 (37.5%) |
|  | Mar. pct | 11.1% ; 16.7% | 71.4% ; 83.3% |  |
| 5 | Count | 4 (12.5%) | 1 (3.1%) | 5 (15.6%) |
|  | Mar. pct | 22.2% ; 80.0% | 7.1% ; 20.0% |  |
| Total | Count | 18 (56.2%) | 14 (43.8%) | 32 (100.0%) |
|  (1) Columns and rows percentages |  |  |  |  |

These can be turned off selectively:

``` r

mtcars %>%
  proc_freq(
    row = "gear",
    col = "vs",
    include.row_percent = FALSE,
    include.column_percent = FALSE,
    include.table_percent = FALSE
  ) %>%
  apa_style(table_width = .5)
```

| gear  | vs  |     |       |
|-------|-----|-----|-------|
|       | 0   | 1   | Total |
| 3     | 12  | 3   | 15    |
| 4     | 2   | 10  | 12    |
| 5     | 4   | 1   | 5     |
| Total | 18  | 14  | 32    |

The flextable package’s `tabulator` function has considerable power in
creating a wide variety of descriptive tables. In general, the
`tabulator` function requires that you calculate the statistics first,
and then you specify where they should go.

Here I calculate the means, standard deviations, and sample sizes within
each cell of a contingency table.

``` r

d_tension <- warpbreaks |>
  summarise(
    m = mean(breaks, na.rm = TRUE),
    sd = sd(breaks, na.rm = TRUE),
    n = n(),
    .by = c(wool, tension)
  ) %>%
  rename(Wool = wool) |>
  mutate(
    tension = factor(
      tension,
      labels = paste(c("Low", "Medium", "High"), "Tension")
    )) 
  
d_tension |> 
  flextable::tabulator(
    rows = c("Wool"),
    columns = "tension",
    M = as_paragraph(m),
    SD = as_paragraph(sd),
    N = as_paragraph(n)
  ) |> 
  flextable::as_flextable() |>
  italic(i = 2, part = "header") |>
  align(j = 1, align = "center") |> 
  theme_apa()
```

| Wool |     | Low Tension |      |     |     | Medium Tension |     |     |     | High Tension |      |     |
|------|-----|-------------|------|-----|-----|----------------|-----|-----|-----|--------------|------|-----|
|      |     | M           | SD   | N   |     | M              | SD  | N   |     | M            | SD   | N   |
| A    |     | 44.6        | 18.1 | 9   |     | 24.0           | 8.7 | 9   |     | 24.6         | 10.3 | 9   |
| B    |     | 28.2        | 9.9  | 9   |     | 28.8           | 9.4 | 9   |     | 18.8         | 4.9  | 9   |

## Processing Data Yourself

What if you want it to look a little different? Unfortunately, you might
have to do some of the heavy lifting yourself. The
`pivot_wider_name_first` function is a wrapper around
[`tidyr::pivot_wider`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
that prints and sorts column names as “name_variable” instead of
“variable_name”. I wanted a shortcut because I frequently needed to make
a shortcut for setting `names_glue = "{tension}_{.value}"` and
`names_vary = "slowest"` when `names_from = "tension"`.

A subtle difference in this version of the table is that the columns are
decimal aligned, which is evident in the standard deviation column.

Also, whereas previously the “stub header” (Wool column) was middle
aligned, it is now top aligned, which is consistent with comparable
examples in the *APA Style Manual*. I am not sure if this is a real rule
in APA style, and I actually prefer middle alignment for situations like
this.

``` r

d_tension |> 
  rename(M = m, SD = sd) |> 
  pivot_wider_name_first(
    values_from = c(M, SD, n),
    names_from = tension
  ) |>
  apa_flextable(column_formats = column_formats(accuracy = .1)) |>
  align(j = 1, align = "center") 
```

| Wool | Low Tension |      |     |     | Medium Tension |     |     |     | High Tension |      |     |
|------|-------------|------|-----|-----|----------------|-----|-----|-----|--------------|------|-----|
|      | M           | SD   | n   |     | M              | SD  | n   |     | M            | SD   | n   |
| A    | 44.6        | 18.1 | 9   |     | 24.0           | 8.7 | 9   |     | 24.6         | 10.3 | 9   |
| B    | 28.2        |  9.9 | 9   |     | 28.8           | 9.4 | 9   |     | 18.8         |  4.9 | 9   |
