# Regression Analysis Tables

``` r

library(dplyr)
library(parameters)
library(performance)
library(apa7)
library(psych)
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

## Binary Logistic Regression

Psychologists who study volunteers have long worried that the people who
sign up for experiments differ from the people who do not. If
personality predicts who volunteers, findings from volunteer samples may
not generalize. Cowles and Davis (1987) asked 1,421 students whether
they would volunteer for a psychology experiment and measured their
neuroticism and extraversion with the Eysenck Personality Inventory. The
data are in the `Cowles` data set from the carData package.

Neuroticism and extraversion are centered at their means so that, when
the model includes their interaction, each trait’s coefficient is its
effect for a person of average standing on the other trait.

``` r

d_vol <- carData::Cowles |>
  mutate(
    Volunteer = volunteer == "yes",
    Sex = sex,
    Neuroticism = neuroticism - mean(neuroticism),
    Extraversion = extraversion - mean(extraversion)
  )

glm_vol1 <- glm(
  Volunteer ~ Sex + Neuroticism + Extraversion,
  data = d_vol,
  family = binomial
)
```

`apa_parameters` accepts `glm` objects. The table reports the
coefficients on the log-odds scale (*B*), their standard errors, the
exponentiated coefficients, and the test statistics. For logistic
regression, the exponentiated coefficient is the odds ratio (*OR*), and
the test statistic is *z*.

``` r

apa_parameters(glm_vol1)
```

| Variable     | B     | SE   | OR   | z     | p      |
|--------------|-------|------|------|-------|--------|
| Constant     | −0.22 | 0.07 | 0.80 | −3.04 |  .002  |
| Sex–Male     | −0.24 | 0.11 | 0.79 | −2.12 |  .03   |
| Neuroticism  |  0.01 | 0.01 | 1.01 |  0.56 |  .57   |
| Extraversion |  0.07 | 0.01 | 1.07 |  4.65 | \<.001 |

Each odds ratio is the multiplicative change in the odds of volunteering
for a one-point increase in the predictor (or, for Sex, for men compared
with women), holding the other predictors constant. For the constant,
the exponentiated coefficient is the odds of volunteering for a woman of
average neuroticism and extraversion, not an odds ratio.

Exponentiated coefficients are added whenever the link function is logit
or log. They are labeled *OR* for logit links, *IRR* (incidence rate
ratio) for log links in count models, and *RR* (risk ratio) for log
links in binomial models. Set `exponentiate = FALSE` to leave them out,
or `exponentiate = TRUE` to add them for other links such as probit.

``` r

apa_parameters(glm_vol1, exponentiate = FALSE)
```

| Variable     | B     | SE   | z     | p      |
|--------------|-------|------|-------|--------|
| Constant     | −0.22 | 0.07 | −3.04 |  .002  |
| Sex–Male     | −0.24 | 0.11 | −2.12 |  .03   |
| Neuroticism  |  0.01 | 0.01 |  0.56 |  .57   |
| Extraversion |  0.07 | 0.01 |  4.65 | \<.001 |

To choose and order the columns, use `predictor_parameters`. Columns
that the model does not report (for example, *t* for a model tested with
*z*) are dropped silently.

``` r

apa_parameters(
  glm_vol1,
  predictor_parameters = c("Parameter", "Exp_Coefficient", "z", "p")
)
```

| Variable     | OR   | z     | p      |
|--------------|------|-------|--------|
| Constant     | 0.80 | −3.04 |  .002  |
| Sex–Male     | 0.79 | −2.12 |  .03   |
| Neuroticism  | 1.01 |  0.56 |  .57   |
| Extraversion | 1.07 |  4.65 | \<.001 |

As with `lm`, `starred` and `bolded` mark significant coefficients.

``` r

apa_parameters(glm_vol1, starred = "Coefficient", bolded = "p")
```

| Variable     | B           | SE   | OR   | z     | p      |
|--------------|-------------|------|------|-------|--------|
| Constant     | −0.22\*\*   | 0.07 | 0.80 | −3.04 |  .002  |
| Sex–Male     | −0.24\*     | 0.11 | 0.79 | −2.12 |  .03   |
| Neuroticism  |  0.01       | 0.01 | 1.01 |  0.56 |  .57   |
| Extraversion |  0.07\*\*\* | 0.01 | 1.07 |  4.65 | \<.001 |

### Comparing logistic regression models

Does the effect of extraversion depend on neuroticism? A second model
adds their interaction. A list of `glm` models is displayed with a
pseudo-*R*² (Tjur’s for logistic regression, as chosen by
[`performance::r2`](https://easystats.github.io/performance/reference/r2.html))
and a likelihood ratio test of the change, because the binomial family
has a fixed dispersion parameter.

``` r

glm_vol2 <- update(glm_vol1, . ~ . + Neuroticism:Extraversion)
apa_parameters(list(glm_vol1, glm_vol2))
```

| Model   | Variable                   | B     | SE   | OR   | z     | p      | R2Tjur | ΔR2Tjur |
|---------|----------------------------|-------|------|------|-------|--------|--------|---------|
| Model 1 |                            |       |      |      |       |        | .02    |         |
|         | Constant                   | −0.22 | 0.07 | 0.80 | −3.04 |  .002  |        |         |
|         | Sex–Male                   | −0.24 | 0.11 | 0.79 | −2.12 |  .03   |        |         |
|         | Neuroticism                |  0.01 | 0.01 | 1.01 |  0.56 |  .57   |        |         |
|         | Extraversion               |  0.07 | 0.01 | 1.07 |  4.65 | \<.001 |        |         |
| Model 2 |                            |       |      |      |       |        | .03    | .01\*\* |
|         | Constant                   | −0.24 | 0.07 | 0.79 | −3.22 |  .001  |        |         |
|         | Sex–Male                   | −0.25 | 0.11 | 0.78 | −2.21 |  .03   |        |         |
|         | Neuroticism                |  0.00 | 0.01 | 1.00 |  0.43 |  .67   |        |         |
|         | Extraversion               |  0.07 | 0.01 | 1.07 |  4.76 | \<.001 |        |         |
|         | Neuroticism × Extraversion | −0.01 | 0.00 | 0.99 | −2.92 |  .004  |        |         |

The interaction improves the fit, but the gain in Tjur’s *R*² is small.
Its odds ratio is below 1: the more neurotic a person is, the less
extraversion increases the odds of volunteering.

The model comparison can also be displayed separately with
`apa_performance_comparison`.

``` r

apa_performance_comparison(list(glm_vol1, glm_vol2))
```

| Model   | R2Tjur | ΔR2Tjur | χ2   | df  | p    |
|---------|--------|---------|------|-----|------|
| Model 1 | .02    | .02     |      |     |      |
| Model 2 | .03    | .01     | 8.62 | 1   | .003 |

## Count Regression

For Poisson and negative binomial regression, the exponentiated
coefficients are incidence rate ratios (*IRR*).

``` r

glm_pois <- glm(carb ~ wt + hp, data = mtcars, family = poisson)
apa_parameters(glm_pois)
```

| Variable | B    | SE   | IRR  | z    | p      |
|----------|------|------|------|------|--------|
| Constant | 0.14 | 0.40 | 1.15 | 0.35 |  .73   |
| Wt       | 0.00 | 0.13 | 1.00 | 0.03 |  .97   |
| Hp       | 0.01 | 0.00 | 1.01 | 3.34 | \<.001 |

Quasi-families and other families with an estimated dispersion parameter
are tested with *t* instead of *z*, and the degrees of freedom are shown
in the column header. Nested models are compared with *F* tests instead
of likelihood ratio tests.

``` r

glm_qpois <- glm(carb ~ wt + hp, data = mtcars, family = quasipoisson)
apa_parameters(glm_qpois)
```

| Variable | B    | SE   | IRR  | t(29) | p      |
|----------|------|------|------|-------|--------|
| Constant | 0.14 | 0.27 | 1.15 | 0.51  |  .61   |
| Wt       | 0.00 | 0.09 | 1.00 | 0.05  |  .96   |
| Hp       | 0.01 | 0.00 | 1.01 | 4.92  | \<.001 |

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
