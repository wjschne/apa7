# Analysis of Variance Tables

``` r

library(dplyr)
library(apa7)
library(afex)
library(car)
```

The `apa_anova` function makes APA-style analysis of variance (ANOVA)
source tables. Like `apa_parameters`, it relies on the
[easystats](https://easystats.github.io/easystats/) packages to do the
computing:
[`parameters::model_parameters`](https://easystats.github.io/parameters/reference/model_parameters.html)
gets the sums of squares, *F* tests, and *p*-values, and the effectsize
package computes effect sizes and their confidence intervals. The
workflow is the same as for regression tables:

1.  Fit a model with `aov`, `lm`, or one of the `afex` functions
    (`aov_car`, `aov_ez`, or `aov_4`).
2.  `apa_anova` converts the fit object into a tibble.
3.  In Quarto and R Markdown documents, the tibble is displayed as an
    APA-style table automatically. Pass it to `apa_flextable` to
    customize the table or save it.

`apa_anova` accepts:

| Object | Example |
|----|----|
| Between-subjects models | `aov(y ~ a * b, data)`, `lm(y ~ a * b, data)` |
| Repeated-measures models with error strata | `aov(y ~ a * b + Error(id / b), data)` |
| `afex` models | `afex::aov_ez("id", "y", data, between = "a", within = "b")` |
| ANOVA tables | `anova(fit)`, `car::Anova(fit, type = 2)` |

## Between-Subjects Designs

### A one-way design

In the `ToothGrowth` data set, guinea pigs were given one of three doses
of vitamin C. Does tooth length differ by dose?

``` r

d_tooth <- ToothGrowth |>
  mutate(dose = factor(dose)) |> 
  rename(Dose = dose,
         Supplement = supp,
         Length = len)

fit_dose <- aov(Length ~ Dose, data = d_tooth)
apa_anova(fit_dose)
```

| Source | SS       | df  | MS       | F     | p      | η2  |
|--------|----------|-----|----------|-------|--------|-----|
| Dose   | 2,426.43 |  2  | 1,213.22 | 67.42 | \<.001 | .70 |
| Error  | 1,025.78 | 57  |    18.00 |       |        |     |

Table 1: A One-Way ANOVA Table

The table has a row for each effect and a row for the error term (the
residuals). Its columns are the sums of squares (*SS*), degrees of
freedom (*df*), mean squares (*MS*), the *F* test with its *p*-value,
and eta squared (*η*²). The default effect size is partial *η*², but in
a one-way design, partial and non-partial *η*² are the same, so the
column is labeled *η*².

In the console, `apa_anova` prints as an ordinary tibble, so you can
inspect it or post-process it with dplyr. Here `as_tibble` is used to
show that tibble in this document.

``` r

apa_anova(fit_dose) |>
  tibble::as_tibble()
```

```
# A tibble: 2 × 7
  Source `*SS*`   `*df*` `*MS*`   `*F*`   `*p*`   `*&eta;*^2^`
  <chr>  <chr>    <chr>  <chr>    <chr>   <chr>   <chr>
1 Dose   2,426.43  2     1,213.22 "67.42" "<.001" ".70"
2 Error  1,025.78 57        18.00 ""      ""      ""          
```

### A factorial design

The guinea pigs also received vitamin C either as orange juice or as
ascorbic acid (`supp`). The factorial model tests both main effects and
their interaction. Interaction terms are labeled with a multiplication
sign (e.g., Supp × Dose).

``` r

fit_tooth <- aov(Length ~ Supplement * Dose, data = d_tooth)
apa_anova(fit_tooth)
```

| Source            | SS       | df  | MS       | F     | p      | η2p |
|-------------------|----------|-----|----------|-------|--------|-----|
| Supplement        |   205.35 |  1  |   205.35 | 15.57 | \<.001 | .22 |
| Dose              | 2,426.43 |  2  | 1,213.22 | 92.00 | \<.001 | .77 |
| Supplement × Dose |   108.32 |  2  |    54.16 |  4.11 |  .02   | .13 |
| Error             |   712.11 | 54  |    13.19 |       |        |     |

### Choosing columns

The `anova_parameters` argument selects the columns and their order.
`"Effect_Size"` stands for whichever effect size is selected (see
below), and `"CI"` adds its confidence interval. Columns are labeled by
`apa_anova` automatically.

``` r

apa_anova(
  fit_tooth,
  anova_parameters = c("Parameter", "df", "F", "p", "Effect_Size")
)
```

| Source            | df  | F     | p      | η2p |
|-------------------|-----|-------|--------|-----|
| Supplement        |  1  | 15.57 | \<.001 | .22 |
| Dose              |  2  | 92.00 | \<.001 | .77 |
| Supplement × Dose |  2  |  4.11 |  .02   | .13 |
| Error             | 54  |       |        |     |

To mark significant effects, `starred` adds significance stars to a
column, and `bolded` bolds the values of a column when the effect is
significant. Columns are named as in `anova_parameters` (e.g.,
`"Parameter"` for the Source column).

``` r

apa_anova(fit_tooth, starred = "F")
```

| Source            | SS       | df  | MS       | F           | p      | η2p |
|-------------------|----------|-----|----------|-------------|--------|-----|
| Supplement        |   205.35 |  1  |   205.35 | 15.57\*\*\* | \<.001 | .22 |
| Dose              | 2,426.43 |  2  | 1,213.22 | 92.00\*\*\* | \<.001 | .77 |
| Supplement × Dose |   108.32 |  2  |    54.16 |  4.11\*     |  .02   | .13 |
| Error             |   712.11 | 54  |    13.19 |             |        |     |

## Effect Sizes

### Choosing an effect size

By default, `apa_anova` displays partial eta squared (*η*²_(p)), the
most commonly reported effect size in psychology. Partial *η*² is the
proportion of variance explained by an effect after the variance
explained by the other effects is set aside. Other effect sizes are
available with the `effect_size`, `partial`, and `generalized`
arguments:

| Arguments                                 | Effect size                 |
|-------------------------------------------|-----------------------------|
| `effect_size = "eta"` (default)           | partial *η*² (*η*²_(p))     |
| `effect_size = "eta", partial = FALSE`    | *η*²                        |
| `effect_size = "eta", generalized = TRUE` | generalized *η*² (*η*²_(G)) |
| `effect_size = "omega"`                   | *ω*²_(p)                    |
| `effect_size = "omega", partial = FALSE`  | partial *ω*² (*ω*²_(p))     |
| `effect_size = "epsilon"`                 | partial *ε*² (*ε*²_(p))     |
| `effect_size = NULL`                      | no effect size              |

*η*² overestimates the population effect size in small samples. *ω*² and
*ε*² are less biased, so they are good choices for small samples.

``` r

apa_anova(fit_tooth, effect_size = "omega")
```

| Source            | SS       | df  | MS       | F     | p      | ω2p |
|-------------------|----------|-----|----------|-------|--------|-----|
| Supplement        |   205.35 |  1  |   205.35 | 15.57 | \<.001 | .20 |
| Dose              | 2,426.43 |  2  | 1,213.22 | 92.00 | \<.001 | .75 |
| Supplement × Dose |   108.32 |  2  |    54.16 |  4.11 |  .02   | .09 |
| Error             |   712.11 | 54  |    13.19 |       |        |     |

Generalized *η*² (Olejnik & Algina, 2003) is comparable across designs
with different numbers of between- and within-subjects factors, which
makes it useful for meta-analysis and for comparing studies. It is the
default effect size in afex.

### Confidence intervals

APA style recommends reporting confidence intervals for effect sizes.
Add `"CI"` to `anova_parameters` to show them.

``` r

apa_anova(
  fit_tooth,
  anova_parameters = c(
    "Parameter", "df", "F", "p", "Effect_Size", "CI"
  ),
  ci = 0.90
)
```

| Source            | df  | F     | p      | η2p | 90% CI       |
|-------------------|-----|-------|--------|-----|--------------|
| Supplement        |  1  | 15.57 | \<.001 | .22 | \[.08, .37\] |
| Dose              |  2  | 92.00 | \<.001 | .77 | \[.68, .83\] |
| Supplement × Dose |  2  |  4.11 |  .02   | .13 | \[.01, .27\] |
| Error             | 54  |       |        |     |              |

Intervals are two-sided. Because *F* tests are one-sided, a 90% interval
is a natural companion to an *F* test with *α* = .05: the interval
excludes 0 if and only if the *F* test is significant (Steiger, 2004).
Its lower bound is 0 when the test is not significant. Set `ci = 0.95`
for 95% intervals.

Intervals are not shown for generalized *η*², because the intervals
computed by effectsize for it can exclude the point estimate. If you
request them, `apa_anova` gives a warning and leaves them out.

## Unbalanced Designs and Types of Sums of Squares

When cell sizes are unequal, the effects in a factorial design are
correlated, and the sums of squares depend on how the variance they
share is divided. In the `Moore` data set from the carData package,
participants were paired with a partner of high or low status, and the
number of times they conformed to their partner’s judgments was
recorded. Participants’ authoritarianism was classified as low, medium,
or high. The cell sizes are unequal:

``` r

d_moore <- carData::Moore |>
  mutate(
    fcategory = factor(fcategory, levels = c("low", "medium", "high")),
    partner.status = factor(partner.status, levels = c("low", "high"))
  ) |>
  rename(
    Authoritarianism = fcategory,
    `Authoritarianism Score` = fscore,
    `Partner Status` = partner.status
  )

table(d_moore$Authoritarianism, d_moore$`Partner Status`)
```

```

         low high
  low     10    5
  medium   4   11
  high     8    7
```

By default, `apa_anova` uses Type 1 (sequential) sums of squares, as
`anova` and `summary.aov` do. Each effect is adjusted only for the
effects entered before it, so the results depend on the order of the
terms in the formula.

``` r

fit_moore <- lm(conformity ~ Authoritarianism * `Partner Status`, data = d_moore)
apa_anova(fit_moore)
```

| Source                            | SS     | df  | MS     | F     | p    | η2p |
|-----------------------------------|--------|-----|--------|-------|------|-----|
| Authoritarianism                  |   3.73 |  2  |   1.87 |  0.09 | .92  | .00 |
| Partner Status                    | 212.21 |  1  | 212.21 | 10.12 | .003 | .21 |
| Authoritarianism × Partner Status | 175.49 |  2  |  87.74 |  4.18 | .02  | .18 |
| Error                             | 817.76 | 39  |  20.97 |       |      |     |

Set `type = 2` or `type = 3` for Type 2 or Type 3 sums of squares, which
are computed with
[`car::Anova`](https://rdrr.io/pkg/car/man/Anova.html). Type 2 tests
adjust each main effect for the other main effects but not for the
interaction, which is appropriate when the interaction is negligible.

``` r

apa_anova(fit_moore, type = 2)
```

| Source                            | SS     | df  | MS     | F     | p    | η2p |
|-----------------------------------|--------|-----|--------|-------|------|-----|
| Authoritarianism                  |  11.61 |  2  |   5.81 |  0.28 | .76  | .01 |
| Partner Status                    | 212.21 |  1  | 212.21 | 10.12 | .003 | .21 |
| Authoritarianism × Partner Status | 175.49 |  2  |  87.74 |  4.18 | .02  | .18 |
| Error                             | 817.76 | 39  |  20.97 |       |      |     |

Type 3 tests adjust each effect for all other effects, including
interactions. Type 3 tests are meaningful only when factors are coded
with contrasts that sum to zero (e.g., `contr.sum`), not with R’s
default treatment contrasts, so set the contrasts before fitting the
model:

``` r

fit_moore_sum <- lm(
  conformity ~ Authoritarianism * `Partner Status`,
  data = d_moore,
  contrasts = list(Authoritarianism = contr.sum, `Partner Status` = contr.sum)
)
apa_anova(fit_moore_sum, type = 3)
```

| Source                            | SS     | df  | MS     | F     | p    | η2p |
|-----------------------------------|--------|-----|--------|-------|------|-----|
| Authoritarianism                  |  36.02 |  2  |  18.01 |  0.86 | .43  | .04 |
| Partner Status                    | 239.56 |  1  | 239.56 | 11.42 | .002 | .23 |
| Authoritarianism × Partner Status | 175.49 |  2  |  87.74 |  4.18 | .02  | .18 |
| Error                             | 817.76 | 39  |  20.97 |       |      |     |

A table from [`car::Anova`](https://rdrr.io/pkg/car/man/Anova.html) or
`anova` can also be passed to `apa_anova` directly. It already has a
type of sums of squares, so the `type` argument is not needed.

``` r

car::Anova(fit_moore, type = 2) |>
  apa_anova()
```

| Source                            | SS     | df  | MS     | F     | p    | η2p |
|-----------------------------------|--------|-----|--------|-------|------|-----|
| Authoritarianism                  |  11.61 |  2  |   5.81 |  0.28 | .76  | .01 |
| Partner Status                    | 212.21 |  1  | 212.21 | 10.12 | .003 | .21 |
| Authoritarianism × Partner Status | 175.49 |  2  |  87.74 |  4.18 | .02  | .18 |
| Error                             | 817.76 | 39  |  20.97 |       |      |     |

## Analysis of Covariance

An analysis of covariance (ANCOVA) is an ANOVA with one or more
continuous covariates. Fit it with `lm` or `aov`. Here, partner status
is tested while controlling for participants’ authoritarianism scores.
Type 2 sums of squares adjust each effect for the other, regardless of
the order of the terms.

``` r

fit_ancova <- lm(conformity ~ `Authoritarianism Score` + `Partner Status`, data = d_moore)
apa_anova(fit_ancova, type = 2)
```

| Source                 | SS       | df  | MS     | F    | p    | η2p |
|------------------------|----------|-----|--------|------|------|-----|
| Authoritarianism Score |     0.63 |  1  |   0.63 | 0.03 | .87  | .00 |
| Partner Status         |   204.83 |  1  | 204.83 | 8.57 | .005 | .17 |
| Error                  | 1,004.24 | 42  |  23.91 |      |      |     |

## Repeated-Measures Designs

### Error strata with `aov`

In the `obk.long` data set from the afex package (O’Brien & Kaiser,
1985), 16 participants in three treatment groups were measured before
treatment, after treatment, and at follow-up. Each phase had five
measurements, which we average here.

``` r

d_obk <- obk.long |>
  mutate(phase = factor(phase, levels = c("pre", "post", "fup"))) |>
  summarise(value = mean(value), .by = c(id, treatment, phase)) |>
  rename(Treatment = treatment, Phase = phase)
```

In base R, a mixed design with a between-subjects factor (treatment) and
a within-subjects factor (phase) is fit with `aov` and an `Error` term.
Each error stratum has its own error term. `apa_anova` introduces each
stratum with a heading row and indents its effects.

``` r

fit_obk_aov <- aov(value ~ Treatment * Phase + Error(id / Phase), data = d_obk)
apa_anova(fit_obk_aov)
```

| Source             | SS    | df  | MS    | F     | p      | η2p |
|--------------------|-------|-----|-------|-------|--------|-----|
| Between subjects   |       |     |       |       |        |     |
|  Treatment         | 37.35 |  2  | 18.67 |  2.91 |  .09   | .31 |
|  Error             | 83.32 | 13  |  6.41 |       |        |     |
| Within subjects    |       |     |       |       |        |     |
|  Phase             | 33.50 |  2  | 16.75 | 23.63 | \<.001 | .65 |
|  Treatment × Phase | 15.40 |  4  |  3.85 |  5.43 |  .003  | .46 |
|  Error             | 18.43 | 26  |  0.71 |       |        |     |

With `aov`, sums of squares are Type 1, and no correction is made for
violations of sphericity.

### afex models

The afex package fits repeated-measures ANOVAs with Type 3 sums of
squares and corrects the degrees of freedom of within-subjects effects
for violations of sphericity (Greenhouse–Geisser by default). In afex
models, each effect has its own error term, so the table shows the
degrees of freedom of each effect and its error term (*df*_(effect) and
*df*_(error)) and the mean squared error (*MSE*), as afex’s `nice`
function does. As in afex, generalized *η*² is the default effect size.

``` r

fit_obk <- aov_ez(
  id = "id",
  dv = "value",
  data = d_obk,
  between = "Treatment",
  within = "Phase"
)
apa_anova(fit_obk)
```

| Effect            | dfeffect | dferror | MSE  | F     | p      | η2G |
|-------------------|----------|---------|------|-------|--------|-----|
| Treatment         | 2.00     | 13.00   | 6.41 |  2.91 |  .09   | .27 |
| Phase             | 1.74     | 22.64   | 0.81 | 19.29 | \<.001 | .21 |
| Treatment × Phase | 3.48     | 22.64   | 0.81 |  5.43 |  .004  | .13 |

The degrees of freedom of the within-subjects effects are not whole
numbers because they have been corrected for sphericity. To report
partial *η*² with confidence intervals instead:

``` r

apa_anova(
  fit_obk,
  generalized = FALSE,
  anova_parameters = c(
    "Parameter", "df", "df_error", "F", "p", "Effect_Size", "CI"
  ),
  ci = 0.90
)
```

| Effect            | dfeffect | dferror | F     | p      | η2p | 90% CI       |
|-------------------|----------|---------|-------|--------|-----|--------------|
| Treatment         | 2.00     | 13.00   |  2.91 |  .09   | .31 | \[.00, .56\] |
| Phase             | 1.74     | 22.64   | 19.29 | \<.001 | .60 | \[.37, .72\] |
| Treatment × Phase | 3.48     | 22.64   |  5.43 |  .004  | .46 | \[.15, .61\] |

The sphericity correction is set when the model is fit or printed in
afex (e.g., `afex_options(correction_aov = "none")`).

## Customizing ANOVA Tables

### Notes

The table does not say which type of sums of squares or which sphericity
correction was used, so say so in a table note. `apa_anova` records how
the table was made, and `apa_note` turns that record into note text:

``` r

tbl_moore <- apa_anova(fit_moore, type = 2)
apa_note(tbl_moore)
```

```
[1] "Type II sums of squares. *&eta;*^2^~p~ = partial eta squared."
```

The note says which type of sums of squares was used and defines the
effect size. For afex models, it also says which effects have
sphericity-corrected degrees of freedom. afex corrects only effects that
involve a within-subjects factor with more than two levels, so the note
names those factors:

``` r

tbl_obk <- apa_anova(fit_obk, starred = "F")
apa_note(tbl_obk)
```

```
[1] "Type III sums of squares. Degrees of freedom and *p*-values for effects involving Phase are Greenhouse–Geisser corrected. *MSE* = mean square error; *&eta;*^2^~G~ = generalized eta squared. ^\\* ^*p* < .05. ^\\*\\* ^*p* < .01. ^\\*\\*\\* ^*p* < .001."
```

When stars or bold values are used, the note explains them, after the
general notes as APA style requires. The text is in markdown, and it
does not start with “*Note*.” because apaquarto adds that.

With [apaquarto](https://wjschne.github.io/apaquarto/), use `apa_note`
in the `apa-note` chunk option with `!expr`. Chunk options are evaluated
before the chunk runs, so make the table in an earlier chunk (as
`tbl_obk` was made above) and display it in the chunk with the note.
Write it as
[`apa7::apa_note`](https://wjschne.github.io/apa7/reference/apa_note.md),
because in documents with cached chunks, chunk options can be evaluated
before [`library(apa7)`](https://github.com/wjschne/apa7) takes effect:

```` markdown
```{r}
#| label: tbl-obk
#| tbl-cap: Mixed ANOVA of Treatment and Phase
#| apa-note: !expr apa7::apa_note(tbl_obk)
tbl_obk
```
````

| Effect            | dfeffect | dferror | MSE  | F           | p      | η2G |
|-------------------|----------|---------|------|-------------|--------|-----|
| Treatment         | 2.00     | 13.00   | 6.41 |  2.91       |  .09   | .27 |
| Phase             | 1.74     | 22.64   | 0.81 | 19.29\*\*\* | \<.001 | .21 |
| Treatment × Phase | 3.48     | 22.64   | 0.81 |  5.43\*\*   |  .004  | .13 |

Table 2: Mixed ANOVA of Treatment and Phase

*Note*. Type III sums of squares. Degrees of freedom and *p*-values for
effects involving Phase are Greenhouse–Geisser corrected. *MSE* = mean
square error; *η*²_(G) = generalized eta squared. ^(\* )*p* \< .05.
^(\*\* )*p* \< .01. ^(\*\*\* )*p* \< .001.

Without apaquarto, `apa_flextable(tbl, note = TRUE)` puts the same note
below the table (or pass your own text as `note`). Do not combine it
with the `apa-note` chunk option, or the note will appear twice.

The generated note is a starting point. To add to it, combine it with
your own text (e.g.,
`paste(apa_note(tbl_obk), "Outliers were removed.")`). With
`collapse = NULL`, `apa_note` returns one sentence per element, so you
can drop or reword sentences.

`apa_note` reads an attribute of the table. dplyr functions such as
`mutate` and `filter` keep it, but some operations (e.g., joins) drop
it, so call `apa_note` on the table that `apa_anova` returned.

### Labels and post-processing

Variable names are converted to title case (e.g., `supp` becomes Supp).
For better labels, rename the variables before fitting the model, as was
done for the Moore data above. Because the output is a tibble, you can
also change the labels afterward.

Further formatting is available with `apa_flextable`.

``` r

apa_anova(fit_tooth) |>
  apa_flextable(table_width = .8)
```

| Source            | SS       | df  | MS       | F     | p      | η2p |
|-------------------|----------|-----|----------|-------|--------|-----|
| Supplement        |   205.35 |  1  |   205.35 | 15.57 | \<.001 | .22 |
| Dose              | 2,426.43 |  2  | 1,213.22 | 92.00 | \<.001 | .77 |
| Supplement × Dose |   108.32 |  2  |    54.16 |  4.11 |  .02   | .13 |
| Error             |   712.11 | 54  |    13.19 |       |        |     |

### Model comparisons

`apa_anova` makes source tables. To compare nested models (e.g.,
`anova(m1, m2)`), use `apa_performance_comparison` or pass a list of
models to `apa_parameters`, as described in the [regression
article](https://wjschne.github.io/apa7/articles/regression.md).

### Using `apa_parameters`

`apa_parameters` returns the same table for `aov` and afex models, so
`apa_parameters(fit)` and `apa_anova(fit)` are interchangeable for these
models. To display the regression coefficients of an ANOVA model, fit it
with `lm` and use `apa_parameters`.

## References

O’Brien, R. G., & Kaiser, M. K. (1985). MANOVA method for analyzing
repeated measures designs: An extensive primer. *Psychological Bulletin,
97*(2), 316–333. <https://doi.org/10.1037/0033-2909.97.2.316>

Olejnik, S., & Algina, J. (2003). Generalized eta and omega squared
statistics: Measures of effect size for some common research designs.
*Psychological Methods, 8*(4), 434–447.
<https://doi.org/10.1037/1082-989X.8.4.434>

Steiger, J. H. (2004). Beyond the *F* test: Effect size confidence
intervals and tests of close fit in the analysis of variance and
contrast analysis. *Psychological Methods, 9*(2), 164–182.
<https://doi.org/10.1037/1082-989X.9.2.164>
