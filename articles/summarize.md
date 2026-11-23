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
  font.family = "Times New Roman",
  text.align = "center",
  table_align = "left"
)
```

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

Getting from “close” to “exactly right” takes a series of polishing
steps:

- Make `Model` into row titles (Model 1, Model 2).
- Left-align and widen the `Predictor` column.
- Use true minus signs (−) instead of hyphens.
- Align numbers on their decimals.
- Format *p*-values, and drop the leading zeros of `beta` and `p`.
- Italicize statistical symbols in the headers (e.g., `b` → *b*).

flextable has functions for each of these, but applying them by hand is
tedious. As a general-purpose package, flextable should not be expected
to know the rules of a particular style guide. `apa_flextable` applies
them all:

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

## Columns and Headers

### Column spanners and deckered heads

`apa_flextable` treats underscores in column names as header levels: in
`Carat_M`, “Carat” is a column spanner (a label over several columns)
and “M” is a deckered head (the label of one column under it). Here are
the means and standard deviations of several variables in the `diamonds`
data set, by cut:

``` r

d_diamonds <- ggplot2::diamonds |>
  select(cut, carat, depth, table) |>
  arrange(cut) |>
  rename_with(str_to_title) |>
  pivot_longer(where(is.numeric), names_to = "Variable") |>
  summarise(
    M = mean(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    .by = c(Variable, Cut)
  ) |>
  pivot_longer(c(M, SD)) |>
  unite(Variable, Variable, name) |>
  pivot_wider(names_from = Variable)

d_diamonds
#> # A tibble: 5 × 7
#>   Cut       Carat_M Carat_SD Depth_M Depth_SD Table_M Table_SD
#>   <ord>       <dbl>    <dbl>   <dbl>    <dbl>   <dbl>    <dbl>
#> 1 Fair        1.05     0.516    64.0    3.64     59.1     3.95
#> 2 Good        0.849    0.454    62.4    2.17     58.7     2.85
#> 3 Very Good   0.806    0.459    61.8    1.38     58.0     2.12
#> 4 Premium     0.892    0.515    61.3    1.16     58.7     1.48
#> 5 Ideal       0.703    0.433    61.7    0.719    56.0     1.25

apa_flextable(d_diamonds)
```

| Cut       | Carat |      |     | Depth |      |     | Table |      |
|-----------|-------|------|-----|-------|------|-----|-------|------|
|           | M     | SD   |     | M     | SD   |     | M     | SD   |
| Fair      | 1.05  | 0.52 |     | 64.04 | 3.64 |     | 59.05 | 3.95 |
| Good      | 0.85  | 0.45 |     | 62.37 | 2.17 |     | 58.69 | 2.85 |
| Very Good | 0.81  | 0.46 |     | 61.82 | 1.38 |     | 57.96 | 2.12 |
| Premium   | 0.89  | 0.52 |     | 61.26 | 1.16 |     | 58.75 | 1.48 |
| Ideal     | 0.70  | 0.43 |     | 61.71 | 0.72 |     | 55.95 | 1.25 |

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

`pivot_wider_name_first` is like
[`tidyr::pivot_wider`](https://tidyr.tidyverse.org/reference/pivot_wider.html),
but it puts the names first (e.g., `Model 1_b` rather than `b_Model 1`),
so the new columns get spanners for each model. There is an example in
[A Complete Custom Table](#a-complete-custom-table) below.

### Break columns

By default, a narrow blank column separates groups of columns under
different spanners. `add_break_columns` inserts break columns yourself,
after any column selected by name or with a tidyselect function:

``` r

d_diamonds |>
  add_break_columns(ends_with("SD"), omit_last = TRUE) |>
  apa_flextable()
```

| Cut       | Carat |      |     | Depth |      |     | Table |      |
|-----------|-------|------|-----|-------|------|-----|-------|------|
|           | M     | SD   |     | M     | SD   |     | M     | SD   |
| Fair      | 1.05  | 0.52 |     | 64.04 | 3.64 |     | 59.05 | 3.95 |
| Good      | 0.85  | 0.45 |     | 62.37 | 2.17 |     | 58.69 | 2.85 |
| Very Good | 0.81  | 0.46 |     | 61.82 | 1.38 |     | 57.96 | 2.12 |
| Premium   | 0.89  | 0.52 |     | 61.26 | 1.16 |     | 58.75 | 1.48 |
| Ideal     | 0.70  | 0.43 |     | 61.71 | 0.72 |     | 55.95 | 1.25 |

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

`add_list_column` numbers the rows. Set `type` to `"A"`, `"a"`, `"I"`,
or `"i"` for letters or Roman numerals:

``` r

d_quote |>
  add_list_column(Quote, type = "A", sep = ") ") |>
  apa_flextable() |>
  align(j = "Attribution", part = "all") |>
  width(width = c(.3, 4.2, 2))
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
  align(part = "footer", align = "left") |>
  width(width = c(2.05, 1.1, 1.1, .05, 1.1, 1.1))
```

| Predictor | Model 1 |  |  | Model 2 |  |
|----|----|----|----|----|----|
|  | b | β |  | b | β |
| Constant | −4.50\*\*\* |  |  | −5.10\*\*\* |  |
| Socioeconomic status |  1.23\*   | .24 |  |  1.45\*   | .31 |
| Age |  |  |  | −0.23    | .03 |
| Note. b = unstandardized regression coefficient. β = standardized regression coefficient. |  |  |  |  |  |
| \* p \< .05. \*\* p \< .01. \*\*\* p \< .001 |  |  |  |  |  |

## Inside `apa_flextable`

`apa_flextable` does roughly this:

1.  Adds space between adjacent column spanners.
2.  Restructures the data with
    [`flextable::as_grouped_data`](https://davidgohel.github.io/flextable/reference/as_grouped_data.html)
    to make row titles, if `row_title_column` is given.
3.  Formats the columns with `apa_format_columns`, if
    `auto_format_columns = TRUE`.
4.  Makes the flextable.
5.  Separates headers into spanners and deckered heads, if
    `separate_headers = TRUE`.
6.  Draws borders between row groups.
7.  Applies `apa_style` to style the table and convert markdown.
8.  Sets column widths with `pretty_widths`.

These steps can also be applied one at a time. Here they are with column
spanners added for illustration:

``` r

d |>
  # Column spanners
  rename_with(.cols = c(b, beta), \(x) paste0("Coefficients_", x)) |>
  rename_with(.cols = c(t, df, p), \(x) paste0("Significance Test_", x)) |>
  # Step 1: Space between column spanners
  add_break_columns(ends_with("beta")) |>
  # Step 2: Row titles
  flextable::as_grouped_data("Model") |>
  mutate(row_title = Model, .before = 1) |>
  fill(Model) |>
  # Step 3: Format the columns
  apa_format_columns() %>%
  # Step 4: Make the flextable
  flextable(col_keys = colnames(select(., -Model, -row_title))) |>
  mk_par(i = ~ !is.na(row_title), value = as_paragraph(row_title)) |>
  merge_h(i = ~ !is.na(row_title)) |>
  # Step 5: Spanners and deckered heads
  flextable::separate_header() |>
  # Step 6: Borders between row groups
  surround(
    i = ~ !is.na(row_title),
    border.top = list(color = "gray20", style = "solid", width = 1)
  ) |>
  # Step 7: Style the table and convert markdown
  apa_style() |>
  align(j = 1, i = ~ is.na(row_title)) |>
  align(i = ~ !is.na(row_title), align = "center") |>
  # Step 8: Column widths
  pretty_widths()
```

| Predictor            | Coefficients |     |     | Significance Test |       |        |
|----------------------|--------------|-----|-----|-------------------|-------|--------|
|                      | b            | β   |     | t                 | df    | p      |
| Model 1              |              |     |     |                   |       |        |
| Constant             | −4.50        |     |     | −18.46            | 85.00 | \<.001 |
| Socioeconomic status |  1.23        | .24 |     |   2.35            | 85.00 |  .02   |
| Model 2              |              |     |     |                   |       |        |
| Constant             | −5.10        |     |     | −22.46            | 84.00 | \<.001 |
| Socioeconomic status |  1.45        | .31 |     |   2.11            | 84.00 |  .03   |
| Age                  | −0.23        | .03 |     |   0.85            | 84.00 |  .54   |

## Speed

By default, `apa_flextable` converts markdown in every cell (with
[`ftExtra::colformat_md`](https://ftextra.atusy.net/reference/colformat_md.html)).
This makes formatting easy and consistent, but it is a little slow, and
a document with many tables can take a while to render.
`markdown_body = FALSE` skips the conversion in the body of a table
whose cells have no markdown, and caching the chunks of finished tables
(with the `cache` chunk option) avoids remaking them each time the
document is rendered.
