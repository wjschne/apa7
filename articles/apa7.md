# Getting Started with apa7

The apa7 package makes tables in the style of the *Publication Manual of
the American Psychological Association* (7th edition). Most of its
functions take a fitted model or a data frame and return a finished
table: regression coefficients, ANOVA source tables, descriptive
statistics, correlations, reliability, factor loadings, marginal means,
meta-analyses, and more. It also has functions for formatting numbers
and statistics in APA style, in tables and in text.

apa7 builds its tables with the
[flextable](https://ardata-fr.github.io/flextable-book/) package, which
has the fullest support for Word (.docx) documents, so the tables look
right in Word, HTML, and PDF. It works in [Quarto](https://quarto.org/)
and R Markdown documents, and with
[apaquarto](https://wjschne.github.io/apaquarto/), a Quarto extension
for writing whole papers in APA style.

``` r

library(apa7)
```

## Example: Report a Regression Analysis

Fit a model as you usually would, and pass it to `apa_parameters`:

``` r

fit <- lm(mpg ~ wt + hp, data = mtcars)
apa_parameters(fit)
```

| Variable | B     | SE   | β    | t(29) | p      |
|----------|-------|------|------|-------|--------|
| Constant | 37.23 | 1.60 |  .00 | 23.28 | \<.001 |
| Wt       | −3.88 | 0.63 | −.63 | −6.13 | \<.001 |
| Hp       | −0.03 | 0.01 | −.36 | −3.52 |  .002  |

In a Quarto or R Markdown document, the result is displayed as an
APA-style table. In the console, it prints as a tibble, so you can look
at it or change it with dplyr functions before it is displayed. For
example, you might want to use `filter` to drop rows or `mutate` to
customize the display of a one or more variables.

To customize the table, or to save it to a Word file, pass it to
`apa_flextable`, which returns a flextable. Any flextable function can
change it further (e.g.,
[`flextable::bold`](https://davidgohel.github.io/flextable/reference/bold.html)
can selectively bold cells):

``` r

apa_parameters(fit) |>
  apa_flextable(font_size = 10, table_width = .6) 
```

| Variable | B     | SE   | β    | t(29) | p      |
|----------|-------|------|------|-------|--------|
| Constant | 37.23 | 1.60 |  .00 | 23.28 | \<.001 |
| Wt       | −3.88 | 0.63 | −.63 | −6.13 | \<.001 |
| Hp       | −0.03 | 0.01 | −.36 | −3.52 |  .002  |

## Table Notes

Every table records how it was made, and `apa_note` turns that record
into the text of a table note: what the abbreviations stand for, which
tests were used, how *p*-values were adjusted, and what the stars mean.

``` r

apa_parameters(fit, starred = "p") |>
  apa_note()
#> [1] "&beta; = standardized coefficient. ^\\* ^*p* < .05. ^\\*\\* ^*p* < .01. ^\\*\\*\\* ^*p* < .001."
```

`apa_flextable(note = TRUE)` puts the note below the table:

``` r

apa_parameters(fit, starred = "p") |>
  apa_flextable(note = TRUE)
```

| Variable | B | SE | β | t(29) | p |
|----|----|----|----|----|----|
| Constant | 37.23 | 1.60 |  .00 | 23.28 | \<.001\*\*\* |
| Wt | −3.88 | 0.63 | −.63 | −6.13 | \<.001\*\*\* |
| Hp | −0.03 | 0.01 | −.36 | −3.52 |  .002\*\*  |
| Note. β = standardized coefficient. \* p \< .05. \*\* p \< .01. \*\*\* p \< .001. |  |  |  |  |  |

When using [apaquarto](https://wjschne.github.io/apaquarto/), put the
note in the `apa-note` chunk option instead, as described in the [table
notes section of the regression
article](https://wjschne.github.io/apa7/articles/regression.html#table-notes).
Some notes cite the articles that describe a method; `apa7_write_bib`
writes their references so that the citations appear in your reference
list (see [citations in
notes](https://wjschne.github.io/apa7/articles/regression.html#citations-in-notes)).

## A Tour of the Table Functions

Each family of tables has an article with examples and options.

| Analysis | Functions | Article |
|:---|:---|:---|
| Regression (linear, logistic, count, ordinal, multinomial) | `apa_parameters`, `apa_performance`, `apa_performance_comparison`, `apa_collinearity` | [Regression](https://wjschne.github.io/apa7/articles/regression.html) |
| Multilevel models (lme4, glmmTMB, nlme) | `apa_parameters`, `apa_performance` | [Multilevel models](https://wjschne.github.io/apa7/articles/multilevel.html) |
| ANOVA | `apa_anova` | [ANOVA](https://wjschne.github.io/apa7/articles/anova.html) |
| Marginal means, contrasts, simple slopes | `apa_means`, `apa_contrasts`, `apa_slopes` | [Marginal means](https://wjschne.github.io/apa7/articles/marginal-means.html) |
| *t* tests, Wilcoxon tests, correlation tests, chi-square tests | `apa_htest` | [Hypothesis tests](https://wjschne.github.io/apa7/articles/htest.html) |
| Descriptive statistics (“Table 1”) | `apa_descriptives` | [Descriptive statistics](https://wjschne.github.io/apa7/articles/descriptives.html) |
| Correlation matrices | `apa_cor` | [Correlations](https://wjschne.github.io/apa7/articles/correlations.html) |
| Contingency tables | `apa_chisq` | [Contingency tables](https://wjschne.github.io/apa7/articles/chisquare.html) |
| Reliability and item analysis | `apa_reliability` | [Reliability](https://wjschne.github.io/apa7/articles/reliability.html) |
| Factor analysis | `apa_loadings`, `apa_factor_cor` | [Factor analysis](https://wjschne.github.io/apa7/articles/factor-analysis.html) |
| Structural equation models (lavaan) | `apa_parameters`, `apa_performance`, `apa_loadings` | [Structural equation models](https://wjschne.github.io/apa7/articles/lavaan.html) |
| Meta-analysis (metafor) | `apa_parameters`, `apa_performance`, `apa_publication_bias` | [Meta-analysis](https://wjschne.github.io/apa7/articles/meta-analysis.html) |
| Any data frame | `apa_flextable` and helpers | [Building tables by hand](https://wjschne.github.io/apa7/articles/summarize.html) |

Here are a few more. A descriptive statistics table, with a column for
each group:

``` r

d <- mtcars
d$am <- factor(d$am, labels = c("Automatic", "Manual"))
d$cyl <- factor(d$cyl)

apa_descriptives(d, variables = c("mpg", "hp", "cyl"), by = "am")
```

| Variable    | Automatic (n = 19) | Manual (n = 13) | Total (N = 32) |
|-------------|--------------------|-----------------|----------------|
| Mpg, M (SD) | 17.15 (3.83)       | 24.39 (6.17)    | 20.09 (6.03)   |
| Hp, M (SD)  | 160.26 (53.91)     | 126.85 (84.06)  | 146.69 (68.56) |
| Cyl, n (%)  |                    |                 |                |
|  4          | 3 (15.8)           | 8 (61.5)        | 11 (34.4)      |
|  6          | 4 (21.1)           | 3 (23.1)        | 7 (21.9)       |
|  8          | 12 (63.2)          | 2 (15.4)        | 14 (43.8)      |

A correlation matrix, with means and standard deviations:

``` r

apa_cor(mtcars[, c("mpg", "wt", "hp", "qsec")])
```

| Variable |  | M | SD | 1 |  | 2 |  | 3 |  | 4 |  |
|----|----|----|----|----|----|----|----|----|----|----|----|
| 1.  | mpg |  20.09 |  6.03 | — |  |  |  |  |  |  |  |
| 2.  | wt |   3.22 |  0.98 | −.87 | \*\*\* | — |  |  |  |  |  |
| 3.  | hp | 146.69 | 68.56 | −.78 | \*\*\* | .66 | \*\*\* | — |  |  |  |
| 4.  | qsec |  17.85 |  1.79 | .42 | \* | −.17 |  | −.71 | \*\*\* | — |  |
| \* p \< .05. \*\*\* p \< .001 |  |  |  |  |  |  |  |  |  |  |  |

A list of nested models gives one table, with the change in *R*²:

``` r

apa_parameters(list(
  lm(mpg ~ wt, data = mtcars),
  lm(mpg ~ wt + hp, data = mtcars)
))
```

| Model   | Variable | B     | SE   | β    | t     | df  | p      | R2  | ΔR2     |
|---------|----------|-------|------|------|-------|-----|--------|-----|---------|
| Model 1 |          |       |      |      |       |     |        | .75 |         |
|         | Constant | 37.29 | 1.88 |  .00 | 19.86 | 30  | \<.001 |     |         |
|         | Wt       | −5.34 | 0.56 | −.87 | −9.56 | 30  | \<.001 |     |         |
| Model 2 |          |       |      |      |       |     |        | .83 | .07\*\* |
|         | Constant | 37.23 | 1.60 |  .00 | 23.28 | 29  | \<.001 |     |         |
|         | Wt       | −3.88 | 0.63 | −.63 | −6.13 | 29  | \<.001 |     |         |
|         | Hp       | −0.03 | 0.01 | −.36 | −3.52 | 29  |  .002  |     |         |

## APA Style Outside of Tables

apa7’s formatting functions are useful in text, too, especially in
inline R code (e.g., `` `r apa_p(.0004, inline = TRUE)` ``).

*p*-values have no leading zero, and very small ones are written as
“\<.001” in tables (and “*p* \< .001” in text):

``` r

apa_p(c(.0004, .0312, .5))
#> [1] "<.001" ".03"   ".50"
apa_p(.0312, inline = TRUE)
#> [1] "*p* = .03"
```

`p2stars` converts *p*-values to stars, and `apa_p_star_note` writes the
note that explains them:

``` r

p2stars(c(.0004, .0312, .5))
#> [1] "\\*\\*\\*" "\\*"       ""
apa_p_star_note()
#> [1] "^\\* ^*p* < .05. ^\\*\\* ^*p* < .01. ^\\*\\*\\* ^*p* < .001"
```

`align_chr` rounds numbers, replaces hyphens with true minus signs, and
pads the numbers so that they line up on their decimals in a table
column. `sci_md` writes very large or very small numbers in scientific
notation:

``` r

align_chr(c(2.431, -0.4, -10))
#> [1] "  2.43" " −0.40" "−10.00"
sci_md(c(123251, 0.00045, 12.3))
#> [1] "1.23×10^5^"  "4.50×10^−4^" "12.30"
```

The markdown helpers write italics, bold, subscripts, and superscripts,
which apa7 tables and Quarto documents display:

``` r

italic_md("M")
#> [1] "*M*"
paste0("*R*", superscript_md("2"), subscript_md("adj"))
#> [1] "*R*^2^~adj~"
```

## Defaults

`apa7_defaults` sets options for all tables, such as the rounding
accuracy, the font and its size, the line spacing, the label of the
intercept, and the width of the page that tables fill (6.5 inches, the
text width of letter-size paper with 1-inch margins, unless changed to
another width in inches or to a paper size such as `"a4"`):

``` r

apa7_defaults(accuracy = .001, 
              font_family = "Arial",
              font_size = 11,
              line_spacing = 1,
              intercept_text = "Intercept",
              width_aware_padding = FALSE,
              page_width = "a4", 
              print = "tibble",
              citations = "keys"
              )

# Back to the defaults
apa7_defaults(reset = TRUE)
```

Each of these settings is also an R option

| Argument            | Option                   | Default           |
|:--------------------|:-------------------------|:------------------|
| accuracy            | apa7.accuracy            | .01               |
| font_family         | apa7.font_family         | “Times New Roman” |
| font_size           | apa7.font_size           | 2                 |
| intercept_text      | apa7.intercept_text      | “Constant”        |
| width_aware_padding | apa7.width_aware_padding | TRUE              |
| page_width          | apa7.page_width          | 6.5               |
| print               | apa7.print               | “auto”            |
| citations           | apa7.citations           | “auto”            |

For example, to set the default font family:

``` r

options(apa.font_family = "Arial")
```

## Writing a Paper with apaquarto

[apaquarto](https://wjschne.github.io/apaquarto/) is a Quarto extension
that formats a whole document in APA style, with the title page,
headings, citations, references, and figures and tables in their proper
places, and renders it to Word, HTML, PDF, or Typst. `install_apaquarto`
installs it in the current project, and `make_apaquarto` opens an app
that writes a new document for you:

``` r

install_apaquarto()
make_apaquarto()
```
