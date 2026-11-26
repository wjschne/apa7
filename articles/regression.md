# Regression Analysis Tables

``` r

library(dplyr)
library(parameters)
library(performance)
library(apa7)
library(psych)
apa7_defaults(font_family = "Open Sans", line_spacing = 1.5)
```

The apa7 package is closely aligned with the
[easystats](https://easystats.github.io/easystats/) suite of packages
for reporting statistical analyses. In most cases, it relies on
easystats functions to do most of the hard work and then converts the
output to APA style tables. For example, the easystats package
parameters has a function `model_parameters` that takes regression fit
objects and displays regression coefficient tables into a `tibble` that
is displayed as a markdown table. The `apa_parameters` function converts
the output of `model_parameters` into a `tibble` that is ready for
`apa_flextable` to display as an APA-style table. The workflow has three
steps:

1.  Make a regression model fit object with `lm`, `glm`, or a similar
    function.
2.  `apa_parameters` converts the fit object into a tibble using
    easystats functions.
3.  `apa_flextable` converts the tibble into an APA-style table.

Because step 2 returns a tibble, you can post-process it with dplyr
before step 3. In Quarto and R Markdown documents, however, the output
of `apa_parameters` is displayed as an APA-style table automatically, so
step 3 is only needed when you want to customize the table or save it.

## Linear Regression

``` r

d <- sat.act |>
  mutate(
    gender = factor(
      gender,
      levels = 1:2,
      labels = c("Male", "Female")
    ),
    education = factor(
      education,
      levels = 0:5,
      labels =     c("None", "High School", "Some College", "Bachelor", "Master", "Doctorate")
    )
  ) |>
  tidyr::drop_na()

lm1 <- lm(ACT ~ age + gender, d)
lm2 <- update(lm1, . ~ . + education)
lm3 <- update(lm2, . ~ . + SATV + SATQ)
```

In the console, `apa_parameters` prints as an ordinary tibble, so you
can inspect it. Here `as_tibble` is used to show that tibble in this
document.

``` r

apa_parameters(lm1) |>
  tibble::as_tibble()
```

    # A tibble: 3 × 6
      Variable      `*B*` `*SE*` `&beta;` `*t*(684)` `*p*`
      <chr>         <chr> <chr>  <chr>    <chr>      <chr>
    1 Constant      27.35 0.59    .00     46.55      <.001
    2 Age            0.06 0.02    .11      2.93       .004
    3 Gender–Female −0.40 0.38   −.04     −1.03       .30 

Table 1

Regression of ACT Scores on Age and Gender as a Tibble

In a rendered document, the output of `apa_parameters` is displayed as a
table without further processing.

``` r

apa_parameters(lm1)
```

| Variable      | B     | SE   | β    | t(684) | p      |
|---------------|-------|------|------|--------|--------|
| Constant      | 27.35 | 0.59 |  .00 | 46.55  | \<.001 |
| Age           |  0.06 | 0.02 |  .11 |  2.93  |  .004  |
| Gender–Female | −0.40 | 0.38 | −.04 | −1.03  |  .30   |

Table 2

Regression of ACT Scores on Age and Gender

The output is still a tibble, so ordinary dplyr verbs work. Pass the
result to `apa_flextable` to customize the table. For example, to drop
the intercept:

``` r

apa_parameters(lm1) |>
  filter(Variable != "Constant") 
```

| Variable      | B     | SE   | β    | t(684) | p     |
|---------------|-------|------|------|--------|-------|
| Age           |  0.06 | 0.02 |  .11 |  2.93  |  .004 |
| Gender–Female | −0.40 | 0.38 | −.04 | −1.03  |  .30  |

Table 3

Regression of ACT Scores on Age and Gender Without the Constant

To compare nested models, pass a list. Model fit statistics (*R*²,
change in *R*², and the *F* test of the change) are added automatically.

``` r

apa_parameters(list(lm1, lm2, lm3))
```

| Model   | Variable               | B     | SE   | β    | t     | df  | p      | R2  | ΔR2       |
|---------|------------------------|-------|------|------|-------|-----|--------|-----|-----------|
| Model 1 |                        |       |      |      |       |     |        | .01 |           |
|         | Constant               | 27.35 | 0.59 |  .00 | 46.55 | 684 | \<.001 |     |           |
|         | Age                    |  0.06 | 0.02 |  .11 |  2.93 | 684 |  .004  |     |           |
|         | Gender–Female          | −0.40 | 0.38 | −.04 | −1.03 | 684 |  .30   |     |           |
| Model 2 |                        |       |      |      |       |     |        | .04 | .02\*\*\* |
|         | Constant               | 27.54 | 0.80 |  .00 | 34.47 | 679 | \<.001 |     |           |
|         | Age                    |  0.02 | 0.03 |  .03 |  0.60 | 679 |  .55   |     |           |
|         | Gender–Female          | −0.57 | 0.38 | −.06 | −1.49 | 679 |  .14   |     |           |
|         | Education–High School  | −0.24 | 0.97 | −.01 | −0.25 | 679 |  .81   |     |           |
|         | Education–Some College | −0.78 | 1.00 | −.04 | −0.78 | 679 |  .44   |     |           |
|         | Education–Bachelor     |  0.86 | 0.71 |  .09 |  1.20 | 679 |  .23   |     |           |
|         | Education–Master       |  1.57 | 0.83 |  .13 |  1.89 | 679 |  .06   |     |           |
|         | Education–Doctorate    |  1.99 | 0.89 |  .17 |  2.25 | 679 |  .03   |     |           |
| Model 3 |                        |       |      |      |       |     |        | .43 | .39\*\*\* |
|         | Constant               |  8.15 | 1.09 |  .00 |  7.46 | 677 | \<.001 |     |           |
|         | Age                    |  0.05 | 0.02 |  .11 |  2.77 | 677 |  .006  |     |           |
|         | Gender–Female          |  0.23 | 0.30 |  .02 |  0.75 | 677 |  .45   |     |           |
|         | Education–High School  |  0.08 | 0.75 |  .00 |  0.10 | 677 |  .92   |     |           |
|         | Education–Some College |  0.14 | 0.77 |  .01 |  0.18 | 677 |  .85   |     |           |
|         | Education–Bachelor     |  0.86 | 0.55 |  .09 |  1.57 | 677 |  .12   |     |           |
|         | Education–Master       |  1.12 | 0.64 |  .09 |  1.75 | 677 |  .08   |     |           |
|         | Education–Doctorate    |  1.05 | 0.69 |  .09 |  1.52 | 677 |  .13   |     |           |
|         | SATV                   |  0.01 | 0.00 |  .31 |  8.01 | 677 | \<.001 |     |           |
|         | SATQ                   |  0.02 | 0.00 |  .39 | 10.12 | 677 | \<.001 |     |           |

Table 4

Hierarchical Regression of ACT Scores

Many hierarchical regression tables instead put *R*² and its change in
rows below each model’s coefficients. Use `performance_layout = "rows"`:

``` r

apa_parameters(list(lm1, lm2, lm3), performance_layout = "rows")
```

| Model   | Variable               | B           | SE   | β    | t     | df  | p      |
|---------|------------------------|-------------|------|------|-------|-----|--------|
| Model 1 |                        |             |      |      |       |     |        |
|         | Constant               | 27.35       | 0.59 |  .00 | 46.55 | 684 | \<.001 |
|         | Age                    |  0.06       | 0.02 |  .11 |  2.93 | 684 |  .004  |
|         | Gender–Female          | −0.40       | 0.38 | −.04 | −1.03 | 684 |  .30   |
|         | R2                     |   .01       |      |      |       |     |        |
| Model 2 |                        |             |      |      |       |     |        |
|         | Constant               | 27.54       | 0.80 |  .00 | 34.47 | 679 | \<.001 |
|         | Age                    |  0.02       | 0.03 |  .03 |  0.60 | 679 |  .55   |
|         | Gender–Female          | −0.57       | 0.38 | −.06 | −1.49 | 679 |  .14   |
|         | Education–High School  | −0.24       | 0.97 | −.01 | −0.25 | 679 |  .81   |
|         | Education–Some College | −0.78       | 1.00 | −.04 | −0.78 | 679 |  .44   |
|         | Education–Bachelor     |  0.86       | 0.71 |  .09 |  1.20 | 679 |  .23   |
|         | Education–Master       |  1.57       | 0.83 |  .13 |  1.89 | 679 |  .06   |
|         | Education–Doctorate    |  1.99       | 0.89 |  .17 |  2.25 | 679 |  .03   |
|         | R2                     |   .04       |      |      |       |     |        |
|         | ΔR2                    |   .02\*\*\* |      |      |       |     |        |
| Model 3 |                        |             |      |      |       |     |        |
|         | Constant               |  8.15       | 1.09 |  .00 |  7.46 | 677 | \<.001 |
|         | Age                    |  0.05       | 0.02 |  .11 |  2.77 | 677 |  .006  |
|         | Gender–Female          |  0.23       | 0.30 |  .02 |  0.75 | 677 |  .45   |
|         | Education–High School  |  0.08       | 0.75 |  .00 |  0.10 | 677 |  .92   |
|         | Education–Some College |  0.14       | 0.77 |  .01 |  0.18 | 677 |  .85   |
|         | Education–Bachelor     |  0.86       | 0.55 |  .09 |  1.57 | 677 |  .12   |
|         | Education–Master       |  1.12       | 0.64 |  .09 |  1.75 | 677 |  .08   |
|         | Education–Doctorate    |  1.05       | 0.69 |  .09 |  1.52 | 677 |  .13   |
|         | SATV                   |  0.01       | 0.00 |  .31 |  8.01 | 677 | \<.001 |
|         | SATQ                   |  0.02       | 0.00 |  .39 | 10.12 | 677 | \<.001 |
|         | R2                     |   .43       |      |      |       |     |        |
|         | ΔR2                    |   .39\*\*\* |      |      |       |     |        |

Table 5

Hierarchical Regression of ACT Scores With Model Fit in Rows

## Binary Logistic Regression

`apa_parameters` also accepts `glm` objects. The table reports
coefficients on the link scale (*B*), their standard errors, the
exponentiated coefficient, and the test statistic. For logistic
regression, the exponentiated coefficient is the odds ratio (*OR*), and
the test statistic is *z*.

The `Cowles` data set in the carData package ([Cowles & Davis,
1987](#ref-cowlesSubjectMatterPsychology1987)) has the neuroticism and
extraversion scores of 1,421 introductory psychology students and
whether they volunteered to take part in psychological research. Here,
the odds of volunteering depend on extraversion, neuroticism, and sex:

``` r

d_volunteer <- carData::Cowles |>
  transmute(
    Volunteered = factor(volunteer, levels = c("no", "yes"), labels = c("No", "Yes")),
    Extraversion = extraversion,
    Neuroticism = neuroticism,
    Sex = factor(sex, levels = c("female", "male"), labels = c("Female", "Male"))
  )

glm1 <- glm(
  Volunteered ~ Extraversion + Neuroticism + Sex,
  data = d_volunteer,
  family = binomial
)
apa_parameters(glm1)
```

| Variable     | B     | SE   | OR   | z     | p      |
|--------------|-------|------|------|-------|--------|
| Constant     | −1.12 | 0.25 | 0.33 | −4.48 | \<.001 |
| Extraversion |  0.07 | 0.01 | 1.07 |  4.65 | \<.001 |
| Neuroticism  |  0.01 | 0.01 | 1.01 |  0.56 |  .57   |
| Sex–Male     | −0.24 | 0.11 | 0.79 | −2.12 |  .03   |

Table 6

Logistic Regression of Volunteering on Personality and Sex

Exponentiated coefficients are added whenever the link function is logit
or log. They are labeled *OR* for logit links, *IRR* (incidence rate
ratio) for log links in count models, and *RR* (risk ratio) for log
links in binomial models. Set `exponentiate = FALSE` to leave them out,
or `exponentiate = TRUE` to add them for other links such as probit.

``` r

apa_parameters(glm1, exponentiate = FALSE)
```

| Variable     | B     | SE   | z     | p      |
|--------------|-------|------|-------|--------|
| Constant     | −1.12 | 0.25 | −4.48 | \<.001 |
| Extraversion |  0.07 | 0.01 |  4.65 | \<.001 |
| Neuroticism  |  0.01 | 0.01 |  0.56 |  .57   |
| Sex–Male     | −0.24 | 0.11 | −2.12 |  .03   |

Table 7

Logistic Regression Coefficients on the Log-Odds Scale

To choose and order the columns, use `predictor_parameters`. Columns
that the model does not report (for example, *t* for a model tested with
*z*) are dropped silently.

``` r

apa_parameters(
  glm1,
  predictor_parameters = c("Parameter", "Exp_Coefficient", "z", "p")
)
```

| Variable     | OR   | z     | p      |
|--------------|------|-------|--------|
| Constant     | 0.33 | −4.48 | \<.001 |
| Extraversion | 1.07 |  4.65 | \<.001 |
| Neuroticism  | 1.01 |  0.56 |  .57   |
| Sex–Male     | 0.79 | −2.12 |  .03   |

Table 8

Logistic Regression With Selected Columns

As with `lm`, `starred` and `bolded` mark significant coefficients.

``` r

apa_parameters(glm1, starred = "Coefficient", bolded = "p")
```

| Variable     | B           | SE   | OR   | z     | p      |
|--------------|-------------|------|------|-------|--------|
| Constant     | −1.12\*\*\* | 0.25 | 0.33 | −4.48 | \<.001 |
| Extraversion |  0.07\*\*\* | 0.01 | 1.07 |  4.65 | \<.001 |
| Neuroticism  |  0.01       | 0.01 | 1.01 |  0.56 |  .57   |
| Sex–Male     | −0.24\*     | 0.11 | 0.79 | −2.12 |  .03   |

Table 9

Logistic Regression With Significance Stars and Bold *p*-Values

### Comparing logistic regression models

A list of `glm` models is displayed with a pseudo-*R*² (Tjur’s for
logistic regression, as chosen by
[`performance::r2`](https://easystats.github.io/performance/reference/r2.html))
and a likelihood ratio test of the change, because the binomial family
has a fixed dispersion parameter.

``` r

glm0 <- glm(Volunteered ~ Extraversion, data = d_volunteer, family = binomial)
apa_parameters(list(glm0, glm1))
```

| Model   | Variable     | B     | SE   | OR   | z     | p      | R2Tjur | ΔR2Tjur |
|---------|--------------|-------|------|------|-------|--------|--------|---------|
| Model 1 |              |       |      |      |       |        | .02    |         |
|         | Constant     | −1.14 | 0.19 | 0.32 | −6.15 | \<.001 |        |         |
|         | Extraversion |  0.07 | 0.01 | 1.07 |  4.64 | \<.001 |        |         |
| Model 2 |              |       |      |      |       |        | .02    | .00     |
|         | Constant     | −1.12 | 0.25 | 0.33 | −4.48 | \<.001 |        |         |
|         | Extraversion |  0.07 | 0.01 | 1.07 |  4.65 | \<.001 |        |         |
|         | Neuroticism  |  0.01 | 0.01 | 1.01 |  0.56 |  .57   |        |         |
|         | Sex–Male     | −0.24 | 0.11 | 0.79 | −2.12 |  .03   |        |         |

Table 10

Two Logistic Regression Models of Volunteering

The model comparison can also be displayed separately with
`apa_performance_comparison`.

``` r

apa_performance_comparison(list(glm0, glm1))
```

| Model   | R2Tjur | ΔR2Tjur | χ2   | df  | p   |
|---------|--------|---------|------|-----|-----|
| Model 1 | .02    | .02     |      |     |     |
| Model 2 | .02    | .00     | 5.42 | 2   | .07 |

Table 11

Comparison of Two Logistic Regression Models of Volunteering

## Multinomial Logistic Regression

When the outcome has more than two unordered categories, multinomial
logistic regression
([`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html))
compares each category with a reference category, which is the first
level of the outcome. The `BEPS` data set in the carData package ([Fox &
Andersen, 2006](#ref-foxEffectDisplaysMultinomial2006)) has the votes of
1,525 people in the British Election Panel Study of 1997–2001
(Conservative, Labour, or Liberal Democrat), their attitudes toward
European integration (from 1 to 11, with higher scores for more
Eurosceptic attitudes), and their knowledge of the parties’ positions on
European integration (from 0 to 3). Here, Conservative is the reference
category:

``` r

d_vote <- carData::BEPS |>
  transmute(
    Vote = vote,
    Euroscepticism = Europe,
    Knowledge = political.knowledge
  )

multinom1 <- nnet::multinom(
  Vote ~ Euroscepticism + Knowledge,
  data = d_vote,
  trace = FALSE
)

apa_parameters(multinom1) |>
  apa_flextable(note = TRUE)
```

| Variable | B | SE | OR | z | p |
|----|----|----|----|----|----|
| Labour vs. Conservative |  |  |  |  |  |
|  Constant |  3.80 | 0.24 | 44.75 |  15.82 | \<.001 |
|  Euroscepticism | −0.35 | 0.02 |  0.71 | −14.84 | \<.001 |
|  Knowledge | −0.53 | 0.07 |  0.59 |  −8.13 | \<.001 |
| Liberal Democrat vs. Conservative |  |  |  |  |  |
|  Constant |  2.24 | 0.26 |  9.39 |   8.49 | \<.001 |
|  Euroscepticism | −0.29 | 0.03 |  0.75 | −11.13 | \<.001 |
|  Knowledge | −0.23 | 0.07 |  0.79 |  −3.12 |  .002 |
| Note. Coefficients compare each category of Vote with the reference category (Conservative). OR = odds ratio (relative to the reference category). |  |  |  |  |  |

Table 12

Multinomial Logistic Regression of Vote on Euroscepticism and Political
Knowledge

Each comparison gets a heading row with its coefficients indented below
it, and the exponentiated coefficients are odds ratios relative to the
reference category. More Eurosceptic voters had lower odds of voting
Labour or Liberal Democrat rather than Conservative, and so did voters
who knew more about the parties’ positions on Europe. To compare the
categories with another reference, reorder the levels of the outcome
(e.g., with `relevel`) before fitting the model. With
`category_headings = FALSE`, each row is labeled with its category
instead (e.g., “Labour: Constant”).

Lists of multinomial models are compared with McFadden’s pseudo-*R*² and
likelihood ratio tests:

``` r

multinom0 <- nnet::multinom(Vote ~ Euroscepticism, data = d_vote, trace = FALSE)
apa_performance_comparison(list(multinom0, multinom1))
```

| Model   | R2McFadden | ΔR2McFadden | χ2    | df  | p      |
|---------|------------|-------------|-------|-----|--------|
| Model 1 | .08        | .08         |       |     |        |
| Model 2 | .10        | .02         | 75.16 | 2   | \<.001 |

Table 13

Comparison of Multinomial Models of Vote

## Count Regression

For Poisson and negative binomial regression, the exponentiated
coefficients are incidence rate ratios (*IRR*). The `quine` data set in
the MASS package ([Venables & Ripley,
2002](#ref-ripleyModernApplied2002)) has the number of days that 146
children in New South Wales, Australia, were absent from school in a
year. Here, absences depend on the child’s sex and on whether the child
was classified as an average or a slow learner:

``` r

d_absences <- MASS::quine |>
  transmute(
    Absences = Days,
    Sex = factor(Sex, levels = c("F", "M"), labels = c("Female", "Male")),
    Learner = factor(Lrn, levels = c("AL", "SL"), labels = c("Average", "Slow"))
  )

glm_pois <- glm(Absences ~ Sex + Learner, data = d_absences, family = poisson)
apa_parameters(glm_pois)
```

| Variable     | B    | SE   | IRR   | z     | p      |
|--------------|------|------|-------|-------|--------|
| Constant     | 2.66 | 0.04 | 14.33 | 73.75 | \<.001 |
| Sex–Male     | 0.18 | 0.04 |  1.20 |  4.43 | \<.001 |
| Learner–Slow | 0.12 | 0.04 |  1.12 |  2.84 |  .004  |

Table 14

Poisson Regression of School Absences on Sex and Learner Status

Quasi-families and other families with an estimated dispersion parameter
are tested with *t* instead of *z*, and the degrees of freedom are shown
in the column header. Nested models are compared with *F* tests instead
of likelihood ratio tests. The absences vary much more than a Poisson
model expects, so the quasi-Poisson standard errors are about four times
as large, and neither predictor is significant.

``` r

glm_qpois <- glm(Absences ~ Sex + Learner, data = d_absences, family = quasipoisson)
apa_parameters(glm_qpois)
```

| Variable     | B    | SE   | IRR   | t(143) | p      |
|--------------|------|------|-------|--------|--------|
| Constant     | 2.66 | 0.14 | 14.33 | 18.50  | \<.001 |
| Sex–Male     | 0.18 | 0.16 |  1.20 |  1.11  |  .27   |
| Learner–Slow | 0.12 | 0.17 |  1.12 |  0.71  |  .48   |

Table 15

Quasi-Poisson Regression of School Absences on Sex and Learner Status

### Zero-inflated and hurdle models

Counts often have more zeros than a Poisson or negative binomial model
expects. A zero-inflated model
([`pscl::zeroinfl`](https://rdrr.io/pkg/pscl/man/zeroinfl.html)) adds a
logistic model of *excess* zeros, which come from cases that could only
be zero (e.g., people who would not have an affair under any
circumstances). A hurdle model
([`pscl::hurdle`](https://rdrr.io/pkg/pscl/man/hurdle.html)) instead has
a logistic model of whether the count is zero or positive, and a
zero-truncated count model of the positive counts. Either way,
`apa_parameters` puts each part under its own heading. Only the count
model’s coefficients are exponentiated (*IRR*), because the zero model’s
coefficients are log odds. The `Affairs` data set in the AER package,
from a magazine survey analyzed by Fair
([1978](#ref-fairTheoryExtramaritalAffairs1978)), has the number of
extramarital affairs that 601 married people reported having in the past
year. The counts were coded from response categories (0, 1, 2, and 3
times; 7 for 4 to 10 times; and 12 for monthly or more often), and 75%
of them are zeros. Here, the number of affairs depends on how happy
people rated their marriages (from 1, very unhappy, to 5, very happy)
and how religious they were (from 1, anti-religious, to 5, very
religious), and so does the probability of an excess zero:

``` r

data("Affairs", package = "AER")
d_affairs <- Affairs |>
  transmute(
    Affairs = affairs,
    Happiness = rating,
    Religiousness = religiousness
  )

zinb <- pscl::zeroinfl(
  Affairs ~ Happiness + Religiousness | Happiness + Religiousness,
  data = d_affairs,
  dist = "negbin"
)

apa_parameters(zinb) |>
  apa_flextable(note = TRUE)
```

| Variable | B | SE | IRR | z | p |
|----|----|----|----|----|----|
| Count model |  |  |  |  |  |
|  Constant |  2.59 | 0.33 | 13.35 |  7.86 | \<.001 |
|  Happiness | −0.18 | 0.07 |  0.84 | −2.67 |  .007 |
|  Religiousness | −0.13 | 0.07 |  0.87 | −1.88 |  .06  |
| Zero-inflation model |  |  |  |  |  |
|  Constant | −1.78 | 0.48 |  | −3.75 | \<.001 |
|  Happiness |  0.50 | 0.09 |  |  5.36 | \<.001 |
|  Religiousness |  0.25 | 0.09 |  |  2.74 |  .006 |
| Note. The count model is negative binomial with dispersion parameter θ = 1.34, and its coefficients are on the log scale. Zero-inflation coefficients are log odds of an excess zero (a zero not produced by the count model). IRR = incidence rate ratio. |  |  |  |  |  |

Table 16

Zero-Inflated Negative Binomial Regression of Extramarital Affairs

People who were happier in their marriages or more religious were more
likely to be among those who would not have an affair at all, and
happier people who did have affairs had fewer of them. The note names
the count distribution, gives the negative binomial dispersion parameter
(*θ*), and explains the zero model. For a hurdle model, the zero model’s
heading is “Zero hurdle model”, and its coefficients are the log odds of
a count greater than zero:

``` r

pscl::hurdle(
  Affairs ~ Happiness + Religiousness | Happiness + Religiousness,
  data = d_affairs,
  dist = "negbin"
) |>
  apa_parameters() |>
  apa_note()
```

    [1] "The count model is zero-truncated negative binomial with dispersion parameter *θ* = 1.35, and its coefficients are on the log scale. Zero hurdle coefficients are log odds of a count greater than zero. *IRR* = incidence rate ratio."

`apa_performance_comparison` compares nested zero-inflated or hurdle
models with likelihood ratio tests.

For nested counts, or counts with many zeros, negative binomial and
zero-inflated mixed models fit with glmmTMB are described in the
[multilevel
article](https://wjschne.github.io/apa7/articles/multilevel.html#count-outcomes-with-glmmtmb).

## Generalized Additive Models

Generalized additive models (GAMs), fit with
[`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html), let a predictor’s
effect be a smooth curve instead of a straight line. The `Wong` data set
in the carData package ([Wong et al.,
2001](#ref-wongMathematicalModelsCognitive2001)) has the performance IQ
of 200 patients tested after they woke from a coma caused by a traumatic
brain injury, with the number of days in the coma and the number of days
between waking and testing. Some patients were tested more than once, so
only each patient’s first test is kept. Here, performance IQ depends on
smooth functions of the length of the coma and the time since waking:

``` r

d_coma <- carData::Wong |>
  arrange(id, days) |>
  distinct(id, .keep_all = TRUE) |>
  transmute(
    IQ = piq,
    Coma = duration,
    Recovery = days
  )

fit_gam <- mgcv::gam(
  IQ ~ s(Coma) + s(Recovery),
  data = d_coma,
  method = "REML"
)

apa_parameters(fit_gam) |>
  apa_flextable(note = TRUE)
```

| Variable | B | SE | t(195.33) | edf | F | p |
|----|----|----|----|----|----|----|
| Parametric terms |  |  |  |  |  |  |
|  Constant | 83.34 | 0.95 | 88.09 |  |  | \<.001 |
| Smooth terms |  |  |  |  |  |  |
|  s(Coma) |  |  |  | 2.67 | 5.88 | \<.001 |
|  s(Recovery) |  |  |  | 1.00 | 1.27 |  .26  |
| Note. Smoothing parameters were estimated by restricted maximum likelihood (REML). Smooth terms are tested with approximate F tests. edf = effective degrees of freedom. |  |  |  |  |  |  |

Table 17

Generalized Additive Model of Performance IQ After a Coma

The parametric terms (here, only the constant) have coefficients and *t*
tests, as in other regression models. Each smooth term has its effective
degrees of freedom (*edf*: 1 for a straight line, more for a curvier
shape) and an approximate *F* test of whether the smooth differs from
zero. Here, the smooth of the time since waking is nearly a straight
line (*edf* close to 1), and the smooth of the coma’s length is curved:
patients in longer comas had lower IQs, but the decline levels off after
about two or three months of coma. For non-Gaussian families (e.g.,
`family = binomial`), the parametric terms are tested with *z*, the
smooth terms with *χ*², and exponentiated coefficients (e.g., *OR*) are
added. Use `terms = "parametric"` or `terms = "smooth"` to show only one
kind of term. Smooths of a factor-`by` variable (e.g.,
`s(Coma, by = Sex)`) get a row for each level.

`apa_performance` gives the adjusted *R*² and the percentage of the
deviance explained:

``` r

apa_performance(fit_gam)
```

| R2adj | Deviance explained (%) | AIC      |
|-------|------------------------|----------|
| .08   | 9.8                    | 1,612.78 |

Table 18

Fit of the Generalized Additive Model

The smooth terms are shrunk toward simpler shapes, so nested GAMs cannot
be compared with the usual *F* or likelihood ratio tests. Lists of GAMs
are compared by *R*² and *AIC* instead.

## Collinearity

`apa_collinearity` reports the variance inflation factor (*VIF*) and
tolerance of each predictor, as computed by
[`performance::check_collinearity`](https://easystats.github.io/performance/reference/check_collinearity.html).
In the third ACT model above, the two SAT scores are correlated, but not
enough to inflate their standard errors much:

``` r

apa_collinearity(lm3) |>
  apa_flextable(note = TRUE)
```

| Variable | VIF | 95% CI for VIF | Tolerance |
|----|----|----|----|
| Age | 1.78 | \[1.61, 1.92\] | .56 |
| Gender | 1.07 | \[1.03, 1.10\] | .93 |
| Education | 1.84 | \[1.67, 2.00\] | .54 |
| SATV | 1.74 | \[1.58, 1.88\] | .58 |
| SATQ | 1.79 | \[1.62, 1.93\] | .56 |
| Note. For Education, which has more than 1 df, VIF is the generalized VIF ([Fox](#ref-foxGeneralizedCollinearityDiagnostics1992)[ ](#ref-foxGeneralizedCollinearityDiagnostics1992)[&](#ref-foxGeneralizedCollinearityDiagnostics1992)[ ](#ref-foxGeneralizedCollinearityDiagnostics1992)[Monette,](#ref-foxGeneralizedCollinearityDiagnostics1992)[ ](#ref-foxGeneralizedCollinearityDiagnostics1992)[1992](#ref-foxGeneralizedCollinearityDiagnostics1992)). Tolerance = 1/VIF, the proportion of a predictor’s variance not explained by the other predictors. VIF = variance inflation factor; CI = confidence interval. |  |  |  |

Table 19

Collinearity Diagnostics of the Full Model of ACT Scores

Education has more than 1 degree of freedom, so its *VIF* is the
generalized *VIF*, and the note says so. Add `"SE_factor"` to `columns`
for *VIF*^(1/(2*df*)), which can be compared across terms with different
degrees of freedom. To bold large *VIF*s, set `flag` to your cutoff
(e.g., `flag = 5` or `flag = 10`). In models with interactions, the
*VIF*s of the interaction and its components are high in part because
product terms are correlated with their components, and the note
explains that this does not affect the test of the interaction.

``` r

lm(ACT ~ SATV * SATQ, data = d) |>
  apa_collinearity(flag = 10) |>
  apa_flextable(note = TRUE)
```

| Variable | VIF | 95% CI for VIF | Tolerance |
|----|----|----|----|
| SATV | 18.20 | \[16.26, 20.09\] | .05 |
| SATQ | 19.39 | \[17.32, 21.41\] | .05 |
| SATV × SATQ | 57.17 | \[51.06, 63.12\] | .02 |
| Note. The VIFs of interaction terms and their components are high partly because product terms are correlated with their components; this does not affect the tests of the interactions. Tolerance = 1/VIF, the proportion of a predictor’s variance not explained by the other predictors. VIF = variance inflation factor; CI = confidence interval. VIFs of 10 or more are bolded. |  |  |  |

Table 20

Collinearity Diagnostics With an Uncentered Interaction

With centered predictors, these *VIF*s are small, and the note leaves
this out.

``` r

lm(ACT ~ SATV * SATQ, data = datawizard::center(d)) |>
  apa_collinearity(flag = 10) |>
  apa_flextable(note = TRUE)
```

| Variable | VIF | 95% CI for VIF | Tolerance |
|----|----|----|----|
| SATV | 1.78 | \[1.63, 1.94\] | .56 |
| SATQ | 1.76 | \[1.61, 1.92\] | .57 |
| SATV × SATQ | 1.20 | \[1.13, 1.27\] | .84 |
| Note. Tolerance = 1/VIF, the proportion of a predictor’s variance not explained by the other predictors. VIF = variance inflation factor; CI = confidence interval. |  |  |  |

Table 21

Collinearity Diagnostics With a Centered Interaction

## Table Notes

A regression table often needs a note that defines its symbols and
explains its stars. `apa_parameters` records how each table was made,
and `apa_note` turns that record into note text. For the hierarchical
regression above, the note defines the standardized coefficient and the
change in *R*² and explains the stars on the change in *R*²:

``` r

tbl_lm <- apa_parameters(list(lm1, lm2, lm3))
apa_note(tbl_lm)
```

    [1] "&beta; = standardized coefficient; &Delta;*R*^2^ = change in *R*^2^. ^\\* ^*p* ≤ .05. ^\\*\\* ^*p* ≤ .01. ^\\*\\*\\* ^*p* ≤ .001."

The note changes with the table. With robust standard errors (`vcov`)
and confidence intervals, it says which standard errors were used and
defines CI:

``` r

apa_parameters(
  lm3,
  predictor_parameters = c("Parameter", "Coefficient", "SE", "CI", "t", "df_error", "p"),
  vcov = "HC3"
) |>
  apa_note()
```

    [1] "Standard errors are heteroskedasticity-consistent (HC3). CI = confidence interval."

For logistic regression, it defines the odds ratio. For count models, it
defines the incidence rate ratio, and for ordinal models, it says that
thresholds are not tested. For mixed models (`lme4`), it gives the
method used to approximate the degrees of freedom of the *t* tests
(Kenward–Roger by default).

``` r

apa_parameters(glm1, starred = "p") |>
  apa_note()
```

    [1] "*OR* = odds ratio. ^\\* ^*p* ≤ .05. ^\\*\\* ^*p* ≤ .01. ^\\*\\*\\* ^*p* ≤ .001."

The text is in markdown and does not start with “*Note*.” because
apaquarto adds that. With
[apaquarto](https://wjschne.github.io/apaquarto/), use `apa_note` in the
`apa-note` chunk option with `!expr`. Chunk options are evaluated before
the chunk runs, so make the table in an earlier chunk (as `tbl_lm` was
made above) and display it in the chunk with the note. Write it as
[`apa7::apa_note`](https://wjschne.github.io/apa7/reference/apa_note.md),
because in documents with cached chunks, chunk options can be evaluated
before [`library(apa7)`](https://github.com/wjschne/apa7) takes effect:

```` markdown
```{r}
#| label: tbl-lm-note
#| apa7-width: landscape
#| tbl-cap: Hierarchical Regression Predicting ACT Scores
#| apa-note: !expr apa7::apa_note(tbl_lm)
tbl_lm
```
````

| Model   | Variable               | B     | SE   | β    | t     | df  | p      | R2  | ΔR2       |
|---------|------------------------|-------|------|------|-------|-----|--------|-----|-----------|
| Model 1 |                        |       |      |      |       |     |        | .01 |           |
|         | Constant               | 27.35 | 0.59 |  .00 | 46.55 | 684 | \<.001 |     |           |
|         | Age                    |  0.06 | 0.02 |  .11 |  2.93 | 684 |  .004  |     |           |
|         | Gender–Female          | −0.40 | 0.38 | −.04 | −1.03 | 684 |  .30   |     |           |
| Model 2 |                        |       |      |      |       |     |        | .04 | .02\*\*\* |
|         | Constant               | 27.54 | 0.80 |  .00 | 34.47 | 679 | \<.001 |     |           |
|         | Age                    |  0.02 | 0.03 |  .03 |  0.60 | 679 |  .55   |     |           |
|         | Gender–Female          | −0.57 | 0.38 | −.06 | −1.49 | 679 |  .14   |     |           |
|         | Education–High School  | −0.24 | 0.97 | −.01 | −0.25 | 679 |  .81   |     |           |
|         | Education–Some College | −0.78 | 1.00 | −.04 | −0.78 | 679 |  .44   |     |           |
|         | Education–Bachelor     |  0.86 | 0.71 |  .09 |  1.20 | 679 |  .23   |     |           |
|         | Education–Master       |  1.57 | 0.83 |  .13 |  1.89 | 679 |  .06   |     |           |
|         | Education–Doctorate    |  1.99 | 0.89 |  .17 |  2.25 | 679 |  .03   |     |           |
| Model 3 |                        |       |      |      |       |     |        | .43 | .39\*\*\* |
|         | Constant               |  8.15 | 1.09 |  .00 |  7.46 | 677 | \<.001 |     |           |
|         | Age                    |  0.05 | 0.02 |  .11 |  2.77 | 677 |  .006  |     |           |
|         | Gender–Female          |  0.23 | 0.30 |  .02 |  0.75 | 677 |  .45   |     |           |
|         | Education–High School  |  0.08 | 0.75 |  .00 |  0.10 | 677 |  .92   |     |           |
|         | Education–Some College |  0.14 | 0.77 |  .01 |  0.18 | 677 |  .85   |     |           |
|         | Education–Bachelor     |  0.86 | 0.55 |  .09 |  1.57 | 677 |  .12   |     |           |
|         | Education–Master       |  1.12 | 0.64 |  .09 |  1.75 | 677 |  .08   |     |           |
|         | Education–Doctorate    |  1.05 | 0.69 |  .09 |  1.52 | 677 |  .13   |     |           |
|         | SATV                   |  0.01 | 0.00 |  .31 |  8.01 | 677 | \<.001 |     |           |
|         | SATQ                   |  0.02 | 0.00 |  .39 | 10.12 | 677 | \<.001 |     |           |

Table 22

Hierarchical Regression Predicting ACT Scores

*Note*. β = standardized coefficient; Δ*R*² = change in *R*².
^(\* )*p* ≤ .05. ^(\*\* )*p* ≤ .01. ^(\*\*\* )*p* ≤ .001.

Without apaquarto (e.g., in a plain Quarto or R Markdown document, or a
table saved to Word with
[`flextable::save_as_docx`](https://davidgohel.github.io/flextable/reference/save_as_docx.html)),
`apa_flextable` can put the note below the table itself. With
`note = TRUE`, it uses the note from `apa_note`; a character string is
used as the note instead. The note is added after the column widths are
set, so a long note does not stretch the table.

``` r

apa_parameters(glm1, starred = "p") |>
  apa_flextable(note = TRUE)
```

| Variable | B | SE | OR | z | p |
|----|----|----|----|----|----|
| Constant | −1.12 | 0.25 | 0.33 | −4.48 | \<.001\*\*\* |
| Extraversion |  0.07 | 0.01 | 1.07 |  4.65 | \<.001\*\*\* |
| Neuroticism |  0.01 | 0.01 | 1.01 |  0.56 |  .57      |
| Sex–Male | −0.24 | 0.11 | 0.79 | −2.12 |  .03 \*   |
| Note. OR = odds ratio. \* p ≤ .05. \*\* p ≤ .01. \*\*\* p ≤ .001. |  |  |  |  |  |

Table 23

Logistic Regression With a Generated Note

Do not use `note` with the `apa-note` chunk option, or the note will
appear twice.

The definitions come from the `abbreviation` slot of each column’s
format, so any column with a definition is defined in the note. Symbols
such as *SE* and *t* have no definition by default. To add one, or to
reword a definition, change the slot in the `column_formats` passed to
`apa_parameters` (or set it for every table with
`apa7_defaults(column_formats = cf)`):

``` r

cf <- column_formats()
cf$SE@abbreviation <- "standard error"
cf$Std_Coefficient@abbreviation <- "standardized regression coefficient"
apa_parameters(lm1, column_formats = cf) |>
  apa_note()
```

    [1] "*SE* = standard error; &beta; = standardized regression coefficient."

The generated note is a starting point. Add to it with `paste` (e.g.,
`paste(apa_note(tbl_lm), "Education was dummy coded with None as the reference group.")`),
or use `collapse = NULL` to get one sentence per element, so you can
drop or reword sentences. `apa_note` reads an attribute of the table
that dplyr functions such as `mutate` and `filter` keep but some
operations (e.g., joins) drop, so call it on the table that
`apa_parameters` returned.

### Citations in notes

Some notes cite the articles that describe a method: for example, the
notes of meta-analysis tables cite the articles on trim and fill and the
multilevel *I*², and the notes of `lavaan` model comparisons cite the
article on the scaled *χ*² difference test. In the console, and in
documents without the references, the citations are written as text
(e.g., “(Duval & Tweedie, 2000)”), which you would add to the reference
list by hand.

To have the citations formatted and added to the reference list
automatically, write the references that come with apa7 to the
document’s folder in the setup chunk and list the file in the YAML
header:

``` r

apa7::apa7_write_bib("apa7.bib")
```

``` yaml
bibliography: [references.bib, apa7.bib]
```

In a Quarto project, the bibliography can also be set for the whole
project in `_quarto.yml` or for a folder in `_metadata.yml`, and apa7
finds it there.

When the document is rendered, `apa_note` writes each citation whose
reference is in the document’s bibliography as a Pandoc citation key
(e.g., `[@duvalTrimFillSimple2000]`), which Pandoc (or
[apaquarto](https://wjschne.github.io/apaquarto/)) turns into a citation
such as “(Duval & Tweedie, 2000)” and an entry in the reference list.
This works for notes in the `apa-note` chunk option and from
`apa_flextable(note = TRUE)`. The keys are those that the Better BibTeX
extension for Zotero makes, so references exported from Zotero work,
too.

To choose the form yourself, use `apa_note(tbl, citations = "keys")` or
`citations = "text"`, or set it for all notes with
`options(apa7.citations = "keys")`. Text is the right choice for tables
saved directly to Word (e.g., with
[`flextable::save_as_docx`](https://davidgohel.github.io/flextable/reference/save_as_docx.html)),
because Pandoc never sees them.

## Printing Options

The `apa7.print` option controls how the output of `apa_parameters`,
`apa_performance`, and `apa_performance_comparison` is displayed.

- `"auto"` (default): table in Quarto and R Markdown documents, tibble
  in the console.
- `"flextable"`: table everywhere.
- `"tibble"`: tibble everywhere.

``` r

options(apa7.print = "flextable")
```

To get the table with non-default settings, pass the output to
`apa_flextable`, which always returns a flextable.

## References

Cowles, M., & Davis, C. (1987). The subject matter of psychology:
Volunteers. *British Journal of Social Psychology*, *26*(2), 97–102.
<https://doi.org/10.1111/j.2044-8309.1987.tb00769.x>

Fair, R. C. (1978). A theory of extramarital affairs. *Journal of
Political Economy*, *86*(1), 45–61. <https://doi.org/10.1086/260646>

Fox, J., & Andersen, R. (2006). Effect displays for multinomial and
proportional-odds logit models. *Sociological Methodology*, *36*(1),
225–255. <https://doi.org/10.1111/j.1467-9531.2006.00180.x>

Fox, J., & Monette, G. (1992). Generalized collinearity diagnostics.
*Journal of the American Statistical Association*, *87*(417), 178–183.
<https://doi.org/10.1080/01621459.1992.10475190>

Venables, W. N., & Ripley, B. D. (2002). *Modern applied statistics with
S* (4th ed.). Springer. <https://www.stats.ox.ac.uk/pub/MASS4/>

Wong, P. P., Monette, G., & Weiner, N. I. (2001). Mathematical models of
cognitive recovery. *Brain Injury*, *15*(6), 519–530.
<https://doi.org/10.1080/02699050116774>
