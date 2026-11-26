# Descriptive statistics table ("Table 1")

Makes an APA-style table describing a sample: for each continuous
variable, its mean and standard deviation (or median and interquartile
range), and for each categorical variable, the number and percentage of
respondents at each level. With `by`, there is a column for each group
and one for the whole sample, and group differences can be tested.

## Usage

``` r
apa_descriptives(
  data,
  variables = NULL,
  by = NULL,
  total = TRUE,
  continuous = "mean_sd",
  categorical = NULL,
  labels = NULL,
  test = c("none", "auto", "chisq", "fisher"),
  missing = c("note", "row"),
  range = FALSE,
  digits = 2,
  percent_digits = 1,
  format_variable_names = TRUE
)
```

## Arguments

- data:

  data frame

- variables:

  variables to describe, in order: unquoted or quoted names (e.g.,
  `c(age, income)` or `c("age", "income")`) or tidyselect helpers (e.g.,
  `starts_with("item")` or `where(is.numeric)`). If `NULL` (default),
  all columns. The `by` variable is never described.

- by:

  grouping variable, unquoted or quoted (e.g., `group` or `"group"`). If
  given, each group gets a column.

- total:

  if `TRUE` (default), a column for the whole sample is added when there
  are groups

- continuous:

  how continuous variables are described: `"mean_sd"` (default) for *M*
  (*SD*), or `"median_iqr"` for *Mdn* (*IQR*). A named vector sets it
  for particular variables (e.g., `c(income = "median_iqr")`), and other
  variables use `"mean_sd"`.

- categorical:

  numeric variables to treat as categorical, selected like `variables`

- labels:

  named character vector of variable labels (e.g.,
  `c(age = "Age (years)")`). Variables without a label are named by
  their formatted names.

- test:

  tests of the differences between groups (requires `by`): `"none"`
  (default) for no tests, or a column of *p*-values with categorical
  variables tested by Fisher's exact test when an expected count is
  below 5 and the chi-square test otherwise (`"auto"`), by the
  chi-square test (`"chisq"`), or by Fisher's exact test (`"fisher"`).
  Continuous variables are tested the same way in each case (see
  Details). `TRUE` is the same as `"auto"`, and `FALSE` the same as
  `"none"`.

- missing:

  where missing values are reported: `"note"` (default) in the note, or
  `"row"` in a "Missing" row below each variable that has missing values

- range:

  if `TRUE`, a "Range" row with the minimum and maximum is added below
  each continuous variable

- digits:

  number of decimal places of means, standard deviations, medians,
  interquartile ranges, and ranges (unaffected by
  `apa7_defaults(accuracy = )`)

- percent_digits:

  number of decimal places of percentages

- format_variable_names:

  format variable names that have no label with the `Variable` column
  format

## Value

`apa7_tbl` (tibble)

## Details

Numeric variables are continuous, and factors, character vectors, and
logical vectors are categorical (use `categorical` to treat numeric
codes as categories). Each categorical variable is introduced by a
heading row, with its levels indented below it, in factor level order
(character vectors are sorted).

Percentages are of the nonmissing responses. By default,
[`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
lists the number of missing values of each variable that has any; with
`missing = "row"`, each such variable gets a "Missing" row with the
number of missing values in each column instead. Rows whose `by` value
is missing are left out.

With `range = TRUE`, each continuous variable gets a "Range" row with
its minimum and maximum.

With `test = "auto"` (or `"chisq"` or `"fisher"`), a *p* column tests
group differences: Welch's *t* test (two groups) or Welch's one-way
ANOVA
([`stats::oneway.test()`](https://rdrr.io/r/stats/oneway.test.html),
more groups) for variables described by their mean, the Kruskal-Wallis
test for variables described by their median, and the chi-square test of
independence (without continuity correction) for categorical variables.
When an expected count of a categorical variable's table is below 5, the
chi-square approximation can be poor, so with `test = "auto"` Fisher's
exact test is used for that variable instead, and the note names the
variables tested this way. `test = "chisq"` and `test = "fisher"` use
one test for every categorical variable. Many journals discourage
testing baseline differences in randomized trials, so tests are not
shown by default.

## Examples

``` r
d <- mtcars
d$am <- factor(d$am, labels = c("Automatic", "Manual"))
d$cyl <- factor(d$cyl)
apa_descriptives(d, variables = c("mpg", "hp", "cyl"), by = "am")
#> # A tibble: 6 × 4
#>   Variable        `Automatic (*n* = 19)` `Manual (*n* = 13)` `Total (*N* = 32)`
#>   <chr>           <chr>                  <chr>               <chr>             
#> 1 Mpg, *M* (*SD*) "17.15 (3.83)"         "24.39 (6.17)"      "20.09 (6.03)"    
#> 2 Hp, *M* (*SD*)  "160.26 (53.91)"       "126.85 (84.06)"    "146.69 (68.56)"  
#> 3 Cyl, *n* (%)    ""                     ""                  ""                
#> 4  4              "3 (15.8)"             "8 (61.5)"          "11 (34.4)"       
#> 5  6              "4 (21.1)"             "3 (23.1)"          "7 (21.9)"        
#> 6  8              "12 (63.2)"            "2 (15.4)"          "14 (43.8)"       

# Ranges and rows of missing values
d$hp[c(3, 8)] <- NA
apa_descriptives(d, variables = c("mpg", "hp", "cyl"), by = "am",
                 range = TRUE, missing = "row")
#> # A tibble: 9 × 4
#>   Variable        `Automatic (*n* = 19)` `Manual (*n* = 13)` `Total (*N* = 32)`
#>   <chr>           <chr>                  <chr>               <chr>             
#> 1 Mpg, *M* (*SD*) "17.15 (3.83)"         "24.39 (6.17)"      "20.09 (6.03)"    
#> 2  Range          "10.40–24.40"          "15.00–33.90"       "10.40–33.90"     
#> 3 Hp, *M* (*SD*)  "165.72 (49.77)"       "129.67 (87.16)"    "151.30 (68.24)"  
#> 4  Range          "95.00–245.00"         "52.00–335.00"      "52.00–335.00"    
#> 5  Missing        "1"                    "1"                 "2"               
#> 6 Cyl, *n* (%)    ""                     ""                  ""                
#> 7  4              "3 (15.8)"             "8 (61.5)"          "11 (34.4)"       
#> 8  6              "4 (21.1)"             "3 (23.1)"          "7 (21.9)"        
#> 9  8              "12 (63.2)"            "2 (15.4)"          "14 (43.8)"       

# Unquoted names and tidyselect helpers work too
apa_descriptives(d, variables = c(mpg, starts_with("c")), by = am)
#> # A tibble: 6 × 4
#>   Variable         `Automatic (*n* = 19)` `Manual (*n* = 13)` `Total (*N* = 32)`
#>   <chr>            <chr>                  <chr>               <chr>             
#> 1 Mpg, *M* (*SD*)  "17.15 (3.83)"         "24.39 (6.17)"      "20.09 (6.03)"    
#> 2 Cyl, *n* (%)     ""                     ""                  ""                
#> 3  4               "3 (15.8)"             "8 (61.5)"          "11 (34.4)"       
#> 4  6               "4 (21.1)"             "3 (23.1)"          "7 (21.9)"        
#> 5  8               "12 (63.2)"            "2 (15.4)"          "14 (43.8)"       
#> 6 Carb, *M* (*SD*) "2.74 (1.15)"          "2.92 (2.18)"       "2.81 (1.62)"     

# Medians for skewed variables, and tests of group differences
apa_descriptives(
  d,
  variables = c(mpg, hp, cyl),
  by = am,
  continuous = c(hp = "median_iqr"),
  test = "auto"
)
#> # A tibble: 6 × 5
#>   Variable   `Automatic (*n* = 19)` `Manual (*n* = 13)` `Total (*N* = 32)` `*p*`
#>   <chr>      <chr>                  <chr>               <chr>              <chr>
#> 1 Mpg, *M* … "17.15 (3.83)"         "24.39 (6.17)"      "20.09 (6.03)"     ".00…
#> 2 Hp, *Mdn*… "175.00 (75.75)"       "109.50 (62.50)"    "136.50 (74.00)"   ".04…
#> 3 Cyl, *n* … ""                     ""                  ""                 ".00…
#> 4  4         "3 (15.8)"             "8 (61.5)"          "11 (34.4)"        ""   
#> 5  6         "4 (21.1)"             "3 (23.1)"          "7 (21.9)"         ""   
#> 6  8         "12 (63.2)"            "2 (15.4)"          "14 (43.8)"        ""   
```
