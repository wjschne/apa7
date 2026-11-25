# Descriptive Statistics Tables

``` r

library(apa7)
library(flextable)
library(ftExtra)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
apa7_defaults(font_family = "Open Sans", line_spacing = 1.5)
```

## Automatic Summaries with the `apa_descriptives` Function

The first table of most empirical papers describes the sample.
`apa_descriptives` makes this table: for each continuous variable, its
mean and standard deviation, *M* (*SD*), and for each categorical
variable, the number and percentage of respondents at each level, *n*
(%). Numeric variables are treated as continuous, and factors, character
vectors, and logical vectors as categorical. Each categorical variable
has a heading row, with its levels indented below it.

### Data Set Summaries

The `survey` data set in the MASS package ([Ripley & Venables,
2009](#ref-ripleyMASSSupportFunctions2009)) has the answers of 237
statistics students to a short questionnaire. Without groups, there is
one column for the whole sample:

``` r

d_survey <- MASS::survey |>
  mutate(
    Exer = fct_recode(
      Exer, 
      Frequently = "Freq") |> 
      fct_relevel("Frequently", after = Inf),
    Smoke = fct_recode(
      Smoke, 
      Occasionally = "Occas", 
      Regularly = "Regul") |> 
      fct_relevel("Heavy", after = Inf)
  ) |> 
  select(Age, 
         `Height (cm)` = Height, 
         `Pulse (beats per minute)` = Pulse, 
         Smoking = Smoke, 
         Exercise = Exer,
         Sex)


apa_descriptives(d_survey, format_variable_names = FALSE) |>
  apa_flextable(note = TRUE)
```

| Variable | Total (N = 237) |
|----|----|
| Age, M (SD) | 20.37 (6.47) |
| Height (cm), M (SD) | 172.38 (9.85) |
| Pulse (beats per minute), M (SD) | 74.15 (11.69) |
| Smoking, n (%) |  |
|  Never | 189 (80.1) |
|  Occasionally | 19 (8.1) |
|  Regularly | 17 (7.2) |
|  Heavy | 11 (4.7) |
| Exercise, n (%) |  |
|  None | 24 (10.1) |
|  Some | 98 (41.4) |
|  Frequently | 115 (48.5) |
| Sex, n (%) |  |
|  Female | 118 (50.0) |
|  Male | 118 (50.0) |
| Note. Values are M (SD) for continuous variables and n (%) for categorical variables. Percentages are of nonmissing responses. Missing values: 28 for Height (cm), 45 for Pulse (beats per minute), 1 for Smoking, and 1 for Sex. |  |

Variables are named by their formatted names unless `labels` gives
better ones. Percentages are of the non-missing responses, and the note
lists the number of missing values of each variable that has any.

### Groups

With `by`, each group gets a column, with its size in the header,
followed by a column for the whole sample (`total = FALSE` leaves it
out). Rows whose group is missing are left out, and the note says how
many.

``` r

apa_descriptives(
  d_survey,
  format_variable_names = FALSE,
  by = Sex
) |>
  apa_flextable(note = TRUE)
```

| Variable | Female (n = 118) | Male (n = 118) | Total (N = 236) |
|----|----|----|----|
| Age, M (SD) | 20.41 (6.91) | 20.33 (6.07) | 20.37 (6.49) |
| Height (cm), M (SD) | 165.69 (6.15) | 178.83 (8.38) | 172.38 (9.87) |
| Pulse (beats per minute), M (SD) | 75.13 (11.41) | 73.20 (12.00) | 74.16 (11.72) |
| Smoking, n (%) |  |  |  |
|  Never | 99 (83.9) | 89 (76.1) | 188 (80.0) |
|  Occasionally | 9 (7.6) | 10 (8.5) | 19 (8.1) |
|  Regularly | 5 (4.2) | 12 (10.3) | 17 (7.2) |
|  Heavy | 5 (4.2) | 6 (5.1) | 11 (4.7) |
| Exercise, n (%) |  |  |  |
|  None | 11 (9.3) | 13 (11.0) | 24 (10.2) |
|  Some | 58 (49.2) | 40 (33.9) | 98 (41.5) |
|  Frequently | 49 (41.5) | 65 (55.1) | 114 (48.3) |
| Note. Values are M (SD) for continuous variables and n (%) for categorical variables. Percentages are of nonmissing responses. Missing values: 28 for Height (cm), 45 for Pulse (beats per minute), and 1 for Smoking. Respondents with a missing value of Sex (n = 1) are not included. |  |  |  |

Table 1

Characteristics of Students by Sex

### Skewed Variables and Group Differences

Age is strongly skewed in this sample (most students are about 20, a few
much older), so its median and interquartile range, *Mdn* (*IQR*),
describe it better. `continuous` sets the description of particular
variables.

With `test = TRUE`, a *p* column tests the group differences: Welch’s
*t* test (two groups) or Welch’s one-way ANOVA (more groups) for
variables described by their mean, the Kruskal–Wallis test for variables
described by their median, and the chi-square test of independence for
categorical variables. When a categorical variable’s table has an
expected count below 5, where the chi-square approximation is poor,
Fisher’s exact test is used for that variable instead, and the note
names it (`categorical_test = "chisq"` or `"fisher"` uses one test for
all categorical variables). Tests are not shown by default, because many
journals discourage testing baseline differences in randomized trials,
where any difference is due to chance.

``` r

apa_descriptives(
  d_survey,
  by = Sex,
  continuous = c(Age = "median_iqr"),
  test = TRUE, 
  format_variable_names = FALSE
) |>
  apa_flextable(note = TRUE)
```

| Variable | Female (n = 118) | Male (n = 118) | Total (N = 236) | p |
|----|----|----|----|----|
| Age, Mdn (IQR) | 18.42 (2.48) | 18.87 (2.37) | 18.58 (2.52) |  .04  |
| Height (cm), M (SD) | 165.69 (6.15) | 178.83 (8.38) | 172.38 (9.87) | \<.001 |
| Pulse (beats per minute), M (SD) | 75.13 (11.41) | 73.20 (12.00) | 74.16 (11.72) |  .26  |
| Smoking, n (%) |  |  |  |  .31  |
|  Never | 99 (83.9) | 89 (76.1) | 188 (80.0) |  |
|  Occasionally | 9 (7.6) | 10 (8.5) | 19 (8.1) |  |
|  Regularly | 5 (4.2) | 12 (10.3) | 17 (7.2) |  |
|  Heavy | 5 (4.2) | 6 (5.1) | 11 (4.7) |  |
| Exercise, n (%) |  |  |  |  .06  |
|  None | 11 (9.3) | 13 (11.0) | 24 (10.2) |  |
|  Some | 58 (49.2) | 40 (33.9) | 98 (41.5) |  |
|  Frequently | 49 (41.5) | 65 (55.1) | 114 (48.3) |  |
| Note. Values are M (SD) or Mdn (IQR) for continuous variables and n (%) for categorical variables. Percentages are of nonmissing responses. Missing values: 28 for Height (cm), 45 for Pulse (beats per minute), and 1 for Smoking. Respondents with a missing value of Sex (n = 1) are not included. p-values are from Welch’s t tests (variables described by M (SD)), Kruskal–Wallis tests (variables described by Mdn (IQR)), and chi-square tests of independence (categorical variables) comparing the groups of Sex. Mdn = median; IQR = interquartile range. |  |  |  |  |

Table 2

Characteristics of Students by Sex With Tests of Group Differences

### Missing Values and Ranges

By default, the note lists the missing values. With `missing = "row"`,
each variable with missing values gets a “Missing” row with the number
of missing values in each column instead. With `range = TRUE`, each
continuous variable gets a “Range” row with its minimum and maximum:

``` r

apa_descriptives(
  d_survey,
  by = Sex,
  missing = "row",
  format_variable_names = FALSE
) |>
  apa_flextable(note = TRUE)
```

| Variable | Female (n = 118) | Male (n = 118) | Total (N = 236) |
|----|----|----|----|
| Age, M (SD) | 20.41 (6.91) | 20.33 (6.07) | 20.37 (6.49) |
| Height (cm), M (SD) | 165.69 (6.15) | 178.83 (8.38) | 172.38 (9.87) |
|  Missing | 16 | 12 | 28 |
| Pulse (beats per minute), M (SD) | 75.13 (11.41) | 73.20 (12.00) | 74.16 (11.72) |
|  Missing | 23 | 22 | 45 |
| Smoking, n (%) |  |  |  |
|  Never | 99 (83.9) | 89 (76.1) | 188 (80.0) |
|  Occasionally | 9 (7.6) | 10 (8.5) | 19 (8.1) |
|  Regularly | 5 (4.2) | 12 (10.3) | 17 (7.2) |
|  Heavy | 5 (4.2) | 6 (5.1) | 11 (4.7) |
|  Missing | 0 | 1 | 1 |
| Exercise, n (%) |  |  |  |
|  None | 11 (9.3) | 13 (11.0) | 24 (10.2) |
|  Some | 58 (49.2) | 40 (33.9) | 98 (41.5) |
|  Frequently | 49 (41.5) | 65 (55.1) | 114 (48.3) |
| Note. Values are M (SD) for continuous variables and n (%) for categorical variables. Percentages are of nonmissing responses. Missing rows give the number of missing values. Respondents with a missing value of Sex (n = 1) are not included. |  |  |  |

Table 3

Characteristics of Students by Sex With Ranges and Missing Values

### Options

- `categorical` treats numeric codes (e.g., 1 = yes, 0 = no) as
  categories.
- `digits` and `percent_digits` set the decimal places of the statistics
  and the percentages.
- `format_variable_names = FALSE` keeps unlabeled variable names exactly
  as they are.

For tables with other layouts, see [Building tables by
hand](https://wjschne.github.io/apa7/articles/summarize.md), which shows
how to make any data frame into an APA-style table.

## References

Ripley, B., & Venables, B. (2009). *MASS: Support functions and datasets
for Venables and Ripley’s MASS* (pp. 7.3–66). Comprehensive R Archive
Network. <https://doi.org/10.32614/CRAN.package.MASS>
