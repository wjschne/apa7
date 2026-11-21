# Write a table note

Makes the text of an APA-style table note from what a table function
recorded about how the table was made. The text is a starting point:
edit it or add to it as needed.

## Usage

``` r
apa_note(x, collapse = " ")
```

## Arguments

- x:

  a table made by
  [`apa_anova()`](https://wjschne.github.io/apa7/reference/apa_anova.md),
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md),
  [`apa_loadings()`](https://wjschne.github.io/apa7/reference/apa_loadings.md),
  or
  [`apa_factor_cor()`](https://wjschne.github.io/apa7/reference/apa_factor_cor.md)

- collapse:

  string that joins the sentences. Use `NULL` to get a character vector
  with one sentence per element.

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

For
[`apa_factor_cor()`](https://wjschne.github.io/apa7/reference/apa_factor_cor.md)
tables, the note says how the factors were extracted and rotated.

For
[`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
tables of regression models (`lm`, `glm`, `polr`, `clm`, and `lme4`
models, or a list of models), the note can say

- which robust standard errors were used (e.g., `vcov = "HC3"`),

- how the degrees of freedom of mixed-model *t* tests were approximated
  (Kenward-Roger or Satterthwaite),

- that ordinal-model thresholds are not tested,

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
dplyr functions such as
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) and
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) keep it,
but some operations (e.g.,
[`bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
and joins) drop it, so call `apa_note()` on the table that the table
function returned.

## Examples

``` r
d <- mtcars
d$cyl <- factor(d$cyl)
d$am <- factor(d$am)
fit <- aov(mpg ~ cyl * am, data = d)
tbl <- apa_anova(fit, starred = "F")
apa_note(tbl)
#> [1] "Sums of squares are sequential (Type I). *&eta;*^2^~p~ = partial eta squared. ^\\* ^*p* < .05. ^\\*\\* ^*p* < .01. ^\\*\\*\\* ^*p* < .001."

if (requireNamespace("afex", quietly = TRUE)) {
  data(obk.long, package = "afex")
  fit_afex <- afex::aov_ez(
    "id", "value", obk.long,
    between = "treatment", within = "phase", fun_aggregate = mean
  )
  apa_note(apa_anova(fit_afex))
}
#> Contrasts set to contr.sum for the following variables: treatment
#> [1] "Type III sums of squares. Degrees of freedom and *p*-values for effects involving Phase are Greenhouse–Geisser corrected. *MSE* = mean square error; *&eta;*^2^~G~ = generalized eta squared."

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
