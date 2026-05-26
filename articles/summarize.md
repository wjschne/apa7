# Summarize Data

``` r

library(apa7)
library(ftExtra)
library(flextable)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)
```

The flextable and gt packages have many, many options and helper
functions. The `tabulator`, `summarizor`, and `proc_freq` functions from
the flextable package are particularly powerful ways of summarizing and
describing data. Functions that automate your descriptive statistics
tables for you (e.g., the amazing [gtsummary
package](https://www.danieldsjoberg.com/gtsummary/index.html))
inevitably entail some level of compromise. It is unreasonable for a
single function to anticipate the diversity of needs out there.
Sometimes you need a table to be a particular way, and you need to do
the heaving lifting yourself. After all that, the `apa_style` function
will get a flextable or gt table close to APA style. You may need to do
some additional styling with flextable or gt as well.

Here I create a table of means, standard deviations, and sample sizes
for several variables across groups. The particulars are not so
important here, just that flextable and gt can do most of the rest after
that.

## Data Setup

``` r

d <- diamonds %>% 
  select(cut, price, carat, depth, table) %>% 
  arrange(cut) %>% 
  rename_with(str_to_title) %>% 
  pivot_longer(where(is.numeric), names_to = "Variable") %>% 
  summarise(
    M = mean(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    n = n(),
    .by = c(Variable, Cut)) %>% 
  pivot_longer(c(M, SD)) %>% 
  unite(Variable, Variable, name) %>%
  pivot_wider(names_from = Variable) 
```

## Flextable

The flextable package has a `theme_apa` function that can get things
very close to full APA style. With a little extra care, the formatting
in [Table 1](#tbl-flextable) is pretty much perfect.

``` r

flextable::flextable(d) |>
  theme_apa() |>
  flextable::separate_header() |>
  colformat_double(j = 3:4,
                   prefix = "$",
                   digits = 2) |>
  italic(j = -1, i = 2, part = "header") |>
  italic(j = 2, i = 1, part = "header") |>
  align(align = "center", part = "header") |>
  align(j = 1) |>
  surround(
    i = 2,
    part = "header",
    border.top = flextable::fp_border_default(
      color = "black",
      width = 1)
  ) 
```

| Cut       | n      | Price      |            | Carat |      | Depth |      | Table |      |
|-----------|--------|------------|------------|-------|------|-------|------|-------|------|
|           |        | M          | SD         | M     | SD   | M     | SD   | M     | SD   |
| Fair      | 1,610  | \$4,358.76 | \$3,560.39 | 1.05  | 0.52 | 64.04 | 3.64 | 59.05 | 3.95 |
| Good      | 4,906  | \$3,928.86 | \$3,681.59 | 0.85  | 0.45 | 62.37 | 2.17 | 58.69 | 2.85 |
| Very Good | 12,082 | \$3,981.76 | \$3,935.86 | 0.81  | 0.46 | 61.82 | 1.38 | 57.96 | 2.12 |
| Premium   | 13,791 | \$4,584.26 | \$4,349.20 | 0.89  | 0.52 | 61.26 | 1.16 | 58.75 | 1.48 |
| Ideal     | 21,551 | \$3,457.54 | \$3,808.40 | 0.70  | 0.43 | 61.71 | 0.72 | 55.95 | 1.25 |

Table 1: Price, Carat, Depth, Table Using `flextable`

### The `apa_flextable` function

The `apa_flextable` function handles most of the formatting
automatically except for the dollar signs, which flextable can add
later. [Table 2](#tbl-apaflextable) also decimal aligns all the numbers,
though this is apparent in column `n` only.

``` r

d |> 
  apa_flextable() |> 
  colformat_char(j = 3:4, prefix = "$") 
```

| Cut       | n      | Price      |            |     | Carat |      |     | Depth |      |     | Table |      |
|-----------|--------|------------|------------|-----|-------|------|-----|-------|------|-----|-------|------|
|           |        | M          | SD         |     | M     | SD   |     | M     | SD   |     | M     | SD   |
| Fair      |  1,610 | \$4,358.76 | \$3,560.39 |     | 1.05  | 0.52 |     | 64.04 | 3.64 |     | 59.05 | 3.95 |
| Good      |  4,906 | \$3,928.86 | \$3,681.59 |     | 0.85  | 0.45 |     | 62.37 | 2.17 |     | 58.69 | 2.85 |
| Very Good | 12,082 | \$3,981.76 | \$3,935.86 |     | 0.81  | 0.46 |     | 61.82 | 1.38 |     | 57.96 | 2.12 |
| Premium   | 13,791 | \$4,584.26 | \$4,349.20 |     | 0.89  | 0.52 |     | 61.26 | 1.16 |     | 58.75 | 1.48 |
| Ideal     | 21,551 | \$3,457.54 | \$3,808.40 |     | 0.70  | 0.43 |     | 61.71 | 0.72 |     | 55.95 | 1.25 |

Table 2: Price, Carat, Depth, Table Using `apa_flextable`
