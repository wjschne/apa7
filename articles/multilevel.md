# Multilevel Model Tables

``` r

library(dplyr)
library(lme4)
library(apa7)
library(performance)
library(parameters)
```

When observations are nested (trials within participants, students
within classrooms) or crossed (participants responding to the same set
of items), ordinary regression treats correlated observations as
independent and understates uncertainty. Multilevel (mixed-effects)
models account for the nesting with random effects. `apa_parameters`
accepts models fit with
[`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) and
[`lme4::glmer`](https://rdrr.io/pkg/lme4/man/glmer.html) and displays
the fixed effects as in an ordinary regression table, followed by the
random-effect standard deviations and correlations.

## Linear Mixed Models

How quickly does performance deteriorate when people do not get enough
sleep, and are some people more vulnerable than others? Belenky et
al. (2003) restricted 18 participants to 3 hours of sleep per night and
measured their average reaction time on a vigilance task each day. The
data are in the `sleepstudy` data set from the lme4 package. Days 0 and
1 were adaptation and training, and Day 2 was the baseline, so we keep
Days 2 through 9 and count days of sleep restriction from the baseline.

``` r

d_sleep <- sleepstudy |>
  filter(Days >= 2) |>
  mutate(Days = Days - 2) |> 
  rename(Participant = Subject)
```

### How much do participants differ?

A model with only a random intercept for each participant shows how much
of the variability in reaction time lies between participants. The
intraclass correlation (ICC) is the proportion of the variance that is
due to stable differences between participants.

``` r

sleep0 <- lmer(Reaction ~ 1 + (1 | Participant), data = d_sleep)
apa_performance(sleep0)
```

| R2marginal | R2conditional | ICC |
|------------|---------------|-----|
| 0          | .49           | .49 |

About half of the variance in reaction time is between participants, so
treating the 144 observations as independent would be a mistake.

Also displayed is the marginal *R*², the proportion of variance
explained by fixed effects. Because there are no fixed effects in this
model, the *R*²_(marginal) is 0. The conditional *R*² is the proportion
of variance explained by both fixed and random effects. Because the
random effect of participants is the only effect, the *R*²_(conditional)
is equal to the ICC in this model.

### Random intercepts

The next model adds days of sleep restriction as a fixed effect. Each
participant still has their own intercept, but all participants are
assumed to slow down at the same rate.

``` r

sleep1 <- lmer(Reaction ~ Days + (1 | Participant), data = d_sleep)
model_parameters(sleep1, ci_method = "boot") |> 
  tibble::as_tibble()
```

    Computing bootstrap confidence intervals ...

    # A tibble: 4 × 11
      Parameter      Coefficient    SE    CI CI_low CI_high     t df_error         p
      <chr>                <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl>    <int>     <dbl>
    1 (Intercept)          268.  10.9   0.95 246.     288.   24.6      140  8.83e-53
    2 Days                  11.4  1.10  0.95   9.38    13.3  10.4      140  3.88e-19
    3 SD (Intercept)        41.8 NA     0.95  28.2     56.1  NA         NA NA
    4 SD (Observati…        30.2 NA     0.95  26.6     34.7  NA         NA NA
    # ℹ 2 more variables: Effects <chr>, Group <chr>

``` r

apa_parameters(sleep1)
```

| Variable                  | B      | SE    | t     | df     | p      |
|---------------------------|--------|-------|-------|--------|--------|
| Constant                  | 267.97 | 10.87 | 24.65 |  22.15 | \<.001 |
| Days                      |  11.44 |  1.10 | 10.41 | 125.00 | \<.001 |
| Participant:SD (Constant) |  41.80 |       |       |        |        |
| Residual SD               |  30.22 |       |       |        |        |

The table has two parts. The fixed effects are read like ordinary
regression coefficients: at baseline, the average reaction time is about
268 ms, and it increases by about 11 ms per day of sleep restriction.
Below them, the random effects show the standard deviation (*SD*) of
participants’ intercepts around the average intercept, and the residual
*SD* of observations around each participant’s own line.

For linear mixed models, `apa_parameters` tests fixed effects with *t*
tests that use Kenward–Roger degrees of freedom, which account for the
number of participants as well as the number of observations. Because
the intercept and the slope have different degrees of freedom here, they
are shown in a *df* column. To use a different method, pass `ci_method`
(e.g., `ci_method = "satterthwaite"`, which requires the lmerTest
package).

### Random slopes

Do participants differ in how quickly they deteriorate? Adding a random
slope for days lets each participant have their own rate of change.

``` r

sleep2 <- lmer(Reaction ~ Days + (Days | Participant), data = d_sleep)
apa_parameters(sleep2)
```

| Variable                      | B      | SE   | t(17) | p      |
|-------------------------------|--------|------|-------|--------|
| Constant                      | 267.97 | 8.27 | 32.42 | \<.001 |
| Days                          |  11.44 | 1.85 |  6.20 | \<.001 |
| Participant:SD (Constant)     |  30.96 |      |       |        |
| Participant:SD (Days)         |   6.77 |      |       |        |
| Participant:r (Constant Days) |    .18 |      |       |        |
| Residual SD                   |  25.53 |      |       |        |

The average slope is unchanged, but its standard error is larger,
because the model now recognizes that participants’ slopes vary: the
*SD* of the slopes is about 7 ms per day around the average of 11 ms per
day. The correlation (*r*) between participants’ intercepts and slopes
is small. When all fixed effects share the same degrees of freedom, as
here, they appear in the *t* column header.

The random-effect rows can be omitted with `random = FALSE`, and
confidence intervals can be added with `"CI"`.

``` r

apa_parameters(
  sleep2,
  predictor_parameters = c("Parameter", "Coefficient", "SE", "CI", "t", "df_error", "p"),
  random = FALSE
)
```

| Variable | B      | SE   | 95% CI             | t(17) | p      |
|----------|--------|------|--------------------|-------|--------|
| Constant | 267.97 | 8.27 | \[250.53, 285.41\] | 32.42 | \<.001 |
| Days     |  11.44 | 1.85 | \[  7.54,  15.33\] |  6.20 | \<.001 |

### Model fit

For mixed models, `apa_performance` reports the marginal *R*² (the
variance explained by the fixed effects), the conditional *R*² (the
variance explained by the fixed and random effects together), and the
ICC.

``` r

apa_performance(sleep2)
```

| R2marginal | R2conditional | ICC |
|------------|---------------|-----|
| .21        | .81           | .76 |

### Comparing models

A list of mixed models is compared with likelihood ratio tests, and the
change in marginal *R*² is shown for each model. When the models differ
in their fixed effects, the likelihood ratio test is based on maximum
likelihood rather than restricted maximum likelihood (REML) estimates,
as it should be.

``` r

apa_parameters(list(sleep0, sleep1, sleep2))
```

| Model | Variable | B | SE | t | df | p | R2marginal | ΔR2marginal | R2conditional | ΔR2conditional |
|----|----|----|----|----|----|----|----|----|----|----|
| Model 1 |  |  |  |  |  |  | .00 |  | .49 |  |
|  | Constant | 307.99 | 10.17 | 30.29 |  17.00 | \<.001 |  |  |  |  |
|  | Participant:SD (Constant) |  40.62 |  |  |  |  |  |  |  |  |
|  | Residual SD |  41.11 |  |  |  |  |  |  |  |  |
| Model 2 |  |  |  |  |  |  | .21 | .21\*\*\* | .73 | .23 |
|  | Constant | 267.97 | 10.87 | 24.65 |  22.15 | \<.001 |  |  |  |  |
|  | Days |  11.44 |  1.10 | 10.41 | 125.00 | \<.001 |  |  |  |  |
|  | Participant:SD (Constant) |  41.80 |  |  |  |  |  |  |  |  |
|  | Residual SD |  30.22 |  |  |  |  |  |  |  |  |
| Model 3 |  |  |  |  |  |  | .21 | .00\*\*\* | .81 | .08 |
|  | Constant | 267.97 |  8.27 | 32.42 |  17.00 | \<.001 |  |  |  |  |
|  | Days |  11.44 |  1.85 |  6.20 |  17.00 | \<.001 |  |  |  |  |
|  | Participant:SD (Constant) |  30.96 |  |  |  |  |  |  |  |  |
|  | Participant:SD (Days) |   6.77 |  |  |  |  |  |  |  |  |
|  | Participant:r (Constant Days) |    .18 |  |  |  |  |  |  |  |  |
|  | Residual SD |  25.53 |  |  |  |  |  |  |  |  |

The random slope does not change the marginal *R*², because the fixed
effects are the same in both models, but the likelihood ratio test shows
that the slopes vary across participants. (A likelihood ratio test of a
variance is conservative, because a variance cannot be negative, so the
true *p* value is smaller than the one shown.)

The comparison can also be displayed on its own with
`apa_performance_comparison`, here including the model with no
predictors:

``` r

apa_performance_comparison(list(sleep0, sleep1, sleep2)) |> 
  apa_flextable()
```

| Model   | R2marginal | ΔR2marginal | R2conditional | ΔR2conditional | χ2    | df  | p      |
|---------|------------|-------------|---------------|----------------|-------|-----|--------|
| Model 1 | .00        | .00         | .49           | .49            |       |     |        |
| Model 2 | .21        | .21         | .73           | .23            | 78.62 | 1   | \<.001 |
| Model 3 | .21        | .00         | .81           | .08            | 25.31 | 2   | \<.001 |

## Generalized Linear Mixed Models

`glmer` models are displayed like `glm` models, with exponentiated
coefficients (e.g., odds ratios) and *z* tests for the fixed effects.

The `VerbAgg` data set from the lme4 package contains 316 people’s
responses to 24 items describing verbally aggressive reactions to
frustrating situations. Each item describes a behavior (cursing,
scolding, or shouting) in a situation in which someone else is to blame
or in which the respondent is to blame, and asks whether the respondent
would *want* to react that way or would actually *do* so. Do people with
higher trait anger endorse more verbally aggressive reactions, and are
people less willing to say that they would act aggressively than that
they would want to?

Because every person responds to every item, people and items are
crossed random effects.

``` r

d_va <- VerbAgg |>
  mutate(
    Response = r2 == "Y",
    Gender = factor(Gender, labels = c("Female", "Male")),
    Behavior = btype,
    Mode = mode,
    Situation = situ,
    Person = id,
    Item = item
  )

glmer_va <- glmer(
  Response ~ Anger + Gender + Behavior + Mode + Situation +
    (1 | Person) + (1 | Item),
  data = d_va,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)
apa_parameters(glmer_va)
```

| Variable             | B     | SE   | OR   | z      | p      |
|----------------------|-------|------|------|--------|--------|
| Constant             |  0.55 | 0.39 | 1.74 |   1.43 |  .15   |
| Anger                |  0.06 | 0.02 | 1.06 |   3.42 | \<.001 |
| Gender–Male          |  0.32 | 0.19 | 1.38 |   1.67 |  .09   |
| Behavior–Scold       | −1.06 | 0.18 | 0.35 |  −5.74 | \<.001 |
| Behavior–Shout       | −2.10 | 0.19 | 0.12 | −11.23 | \<.001 |
| Mode–Do              | −0.71 | 0.15 | 0.49 |  −4.67 | \<.001 |
| Situation–Self       | −1.05 | 0.15 | 0.35 |  −6.96 | \<.001 |
| Person:SD (Constant) |  1.34 |      |      |        |        |
| Item:SD (Constant)   |  0.34 |      |      |        |        |

Each point of trait anger multiplies the odds of endorsing an item by
about 1.06. Scolding and shouting are endorsed less often than cursing,
reactions are endorsed less often when the respondent is to blame than
when someone else is, and people are less likely to say they would *do*
something than that they would *want* to. The random-effect *SD*s are on
the log-odds scale and show that people differ much more than items do,
after the item characteristics are taken into account.
