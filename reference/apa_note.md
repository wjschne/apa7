# Write a table note

Makes the text of an APA-style table note from what a table function
recorded about how the table was made. The text is a starting point:
edit it or add to it as needed.

## Usage

``` r
apa_note(x, collapse = " ", citations = NULL)
```

## Arguments

- x:

  a table made by an apa7 table function:
  [`apa_anova()`](https://wjschne.github.io/apa7/reference/apa_anova.md),
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md),
  [`apa_performance()`](https://wjschne.github.io/apa7/reference/apa_performance.md)
  (for `lavaan` and `metafor` models),
  [`apa_htest()`](https://wjschne.github.io/apa7/reference/apa_htest.md),
  [`apa_chisq()`](https://wjschne.github.io/apa7/reference/apa_chisq.md),
  [`apa_descriptives()`](https://wjschne.github.io/apa7/reference/apa_descriptives.md),
  [`apa_means()`](https://wjschne.github.io/apa7/reference/apa_means.md),
  [`apa_contrasts()`](https://wjschne.github.io/apa7/reference/apa_means.md),
  [`apa_slopes()`](https://wjschne.github.io/apa7/reference/apa_means.md),
  [`apa_loadings()`](https://wjschne.github.io/apa7/reference/apa_loadings.md),
  [`apa_factor_cor()`](https://wjschne.github.io/apa7/reference/apa_factor_cor.md),
  [`apa_n_factors()`](https://wjschne.github.io/apa7/reference/apa_n_factors.md),
  [`apa_reliability()`](https://wjschne.github.io/apa7/reference/apa_reliability.md),
  [`apa_collinearity()`](https://wjschne.github.io/apa7/reference/apa_collinearity.md),
  [`apa_publication_bias()`](https://wjschne.github.io/apa7/reference/apa_publication_bias.md),
  or
  [`apa_subgroups()`](https://wjschne.github.io/apa7/reference/apa_subgroups.md).
  Call `apa_note()` before changing the table, because some operations
  (e.g.,
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html))
  drop the information it needs.

- collapse:

  string that joins the sentences. Use `NULL` to get a character vector
  with one sentence per element.

- citations:

  how articles cited in the note are written: `"auto"`, `"keys"` (Pandoc
  citation keys, e.g., `[@duvalTrimFillSimple2000]`), or `"text"` (e.g.,
  "(Duval & Tweedie, 2000)"). If `NULL` (default), the `apa7.citations`
  option is used, which is `"auto"` unless set. See
  [`apa7_write_bib()`](https://wjschne.github.io/apa7/reference/apa7_write_bib.md).

## Value

character

## Details

For
[`apa_anova()`](https://wjschne.github.io/apa7/reference/apa_anova.md)
tables, the note can say

- which type of sums of squares was used,

- which effects have sphericity-corrected degrees of freedom (for `afex`
  models, only effects involving a within-subjects factor with more than
  two levels are corrected),

- what the effect size (and *MSE*, for `afex` models) stands for, and

- what stars or bold values mean.

For
[`apa_loadings()`](https://wjschne.github.io/apa7/reference/apa_loadings.md)
tables, the note can say

- how the factors were extracted and rotated (e.g., "Principal axis
  factoring with oblimin rotation"), and that loadings after an oblique
  rotation are pattern coefficients,

- which loadings are omitted (`min_loading`) or bolded (`bold_max`),

- what the *h*^2^, *u*^2^, and Complexity columns stand for, and

- that the cumulative proportion of variance is approximate when the
  factors are correlated.

For confirmatory factor analyses fit with `lavaan`, the note instead
gives the sample size and estimator, says that the loadings are
standardized, and says that blank cells are loadings fixed at 0.

For
[`apa_descriptives()`](https://wjschne.github.io/apa7/reference/apa_descriptives.md)
tables, the note says which statistics are shown, lists missing values,
and names the tests of group differences.

For
[`apa_means()`](https://wjschne.github.io/apa7/reference/apa_means.md),
[`apa_contrasts()`](https://wjschne.github.io/apa7/reference/apa_means.md),
and
[`apa_slopes()`](https://wjschne.github.io/apa7/reference/apa_means.md)
tables, the note says what the values are, how the other predictors were
held constant, on which scale the estimates are, which level was
subtracted from which, and how *p*-values were adjusted for multiple
comparisons.

For
[`apa_collinearity()`](https://wjschne.github.io/apa7/reference/apa_collinearity.md)
tables, the note names terms with generalized *VIF*s, explains the
*VIF*s of interaction terms, defines tolerance, and says which *VIF*s
are bolded.

For
[`apa_reliability()`](https://wjschne.github.io/apa7/reference/apa_reliability.md)
tables, the note gives the number of respondents, says which items were
reverse-scored and how scale scores were computed, and defines the
statistics.

For
[`apa_factor_cor()`](https://wjschne.github.io/apa7/reference/apa_factor_cor.md)
tables, the note says how the factors were extracted and rotated (for
`lavaan` models, the sample size, the estimator, and that the
correlations are standardized).

For
[`apa_htest()`](https://wjschne.github.io/apa7/reference/apa_htest.md)
tables, the note can say

- which tests were done (e.g., Welch's *t* tests) and what the groups
  were compared with or the variables were correlated with,

- that *p*-values are one-sided, if they are,

- how Cohen's *d* (or Hedges' *g*) was standardized,

- what the effect size and its confidence interval stand for, and

- what stars or bold values mean.

For
[`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
tables of regression models (`lm`, `glm`, `polr`, `clm`, `multinom`,
`zeroinfl`, `hurdle`, `lme4`, `glmmTMB`, and `nlme` models, or a list of
models), the note can say

- which robust standard errors were used (e.g., `vcov = "HC3"`),

- how the degrees of freedom of mixed-model *t* tests were approximated
  (Kenward-Roger or Satterthwaite),

- that ordinal-model thresholds are not tested,

- the scale of the zero-inflation and dispersion coefficients of
  `glmmTMB` models,

- the residual correlation structure and variance function of
  [`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) models,

- the reference category of multinomial models
  ([`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html)),

- the count distribution and the meaning of the zero model of
  zero-inflated and hurdle models (`pscl`),

- the effect-size measure and model of meta-analyses
  ([`metafor::rma`](https://wviechtb.github.io/metafor/reference/rma.uni.html))
  and, for tables of studies, the test of the pooled estimate, its
  prediction interval, and the test of heterogeneity,

- what the standardized coefficient (\\\beta\\), exponentiated
  coefficient (e.g., *OR*), confidence interval, and model-comparison
  (e.g., \\\Delta R^2\\) columns stand for, and

- what stars or bold values mean.

Abbreviations are defined from the `abbreviation` slot of each column's
[`column_format()`](https://wjschne.github.io/apa7/reference/column_format.md),
so a note defines every abbreviated header that has a definition. To
define another header, or to change a definition, set the slot in the
`column_formats` passed to the table function (or set the default with
[`apa7_defaults()`](https://wjschne.github.io/apa7/reference/apa7_defaults.md)):

    cf <- column_formats()
    cf$Sum_Squares@abbreviation <- "sum of squares"
    apa_anova(fit, anova_parameters = c("Parameter", "Sum_Squares", "df", "F", "p"),
              column_formats = cf)

The sentences are in APA order: general notes first, then probability
notes. The text does not start with "*Note*." because apaquarto adds it.
With [apaquarto](https://wjschne.github.io/apaquarto/), make the table
in one chunk and display it in another, using the note as the `apa-note`
chunk option. Write `apa7::apa_note()` there, because in documents with
cached chunks, chunk options can be evaluated before
[`library(apa7)`](https://github.com/wjschne/apa7) takes effect:

    ```{r}
    #| label: anova-fit
    tbl <- apa_anova(fit, type = 3)
    ```

    ```{r}
    #| label: tbl-anova
    #| tbl-cap: Analysis of Variance
    #| apa-note: !expr apa7::apa_note(tbl)
    tbl
    ```

The information is stored in an `"apa_note"` attribute of the table.
dplyr functions such as `mutate()` and
[`filter()`](https://rdrr.io/r/stats/filter.html) keep it, but some
operations (e.g., `bind_rows()` and joins) drop it, so call `apa_note()`
on the table that the table function returned.

## Examples

``` r
d <- mtcars
d$cyl <- factor(d$cyl)
d$am <- factor(d$am)
fit <- aov(mpg ~ cyl * am, data = d)
tbl <- apa_anova(fit, starred = "F")
apa_note(tbl)
#> [1] "Sums of squares are sequential (Type I). *&eta;*^2^~p~ = partial eta squared. ^\\* ^*p* < .05. ^\\*\\* ^*p* < .01. ^\\*\\*\\* ^*p* < .001."

# Descriptive statistics with tests of group differences
d$Transmission <- factor(d$am, labels = c("Automatic", "Manual"))
apa_descriptives(d, variables = c(mpg, hp), by = Transmission, test = TRUE) |>
  apa_note()
#> [1] "Values are *M* (*SD*) for continuous variables. *p*-values are from Welch's *t* tests (continuous variables) comparing the groups of Transmission."

# Factor analysis
fit_fa <- psych::fa(Harman74.cor$cov, 4, fm = "pa", rotate = "varimax")
apa_loadings(fit_fa, min_loading = .3, communality = TRUE) |>
  apa_note()
#> [1] "Principal axis factoring with varimax rotation. Loadings smaller than .30 in absolute value are omitted. *h*^2^ = communality."

# Regression
lm(mpg ~ cyl + wt, data = mtcars) |>
  apa_parameters(starred = "p") |>
  apa_note()
#> [1] "&beta; = standardized coefficient. ^\\* ^*p* < .05. ^\\*\\* ^*p* < .01. ^\\*\\*\\* ^*p* < .001."
```
