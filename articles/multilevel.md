# Multilevel Model Tables

``` r

library(dplyr)
library(lme4)
library(apa7)
library(performance)
library(parameters)
apa7_defaults(font_family = "Open Sans", line_spacing = 1.5)
```

When observations are nested (trials within participants, students
within classrooms) or crossed (participants responding to the same set
of items), ordinary regression treats correlated observations as
independent and understates uncertainty. Multilevel (mixed-effects)
models account for the nesting with random effects. `apa_parameters`
accepts models fit with
[`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html),
[`lme4::glmer`](https://rdrr.io/pkg/lme4/man/glmer.html),
[`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html), and
[`glmmTMB::glmmTMB`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html) and
displays the fixed effects as in an ordinary regression table, followed
by the random-effect standard deviations and correlations, each under
its own heading.

## Linear Mixed Models

How quickly does performance deteriorate when people do not get enough
sleep, and are some people more vulnerable than others? Belenky et al.
([2003](#ref-belenkyPatternsPerformanceDegradation2003)) restricted 18
participants to 3 hours of sleep per night and measured their average
reaction time on a vigilance task each day. The data are in the
`sleepstudy` data set from the lme4 package. Days 0 and 1 were
adaptation and training, and Day 2 was the baseline, so we keep Days 2
through 9 and count days of sleep restriction from the baseline.

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

Table 1

Fit of the Unconditional Model of Reaction Time

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

sleep1 <- lmer(Reaction ~ Days + (1 | Participant), 
               data = d_sleep)

apa_parameters(
  sleep1,
  predictor_parameters = c(
    "Parameter", 
    "Coefficient", 
    "SE", "CI", 
    "t", "df_error", "p")
)
```

| Variable            | B      | SE    | 95% CI             | t     | df     | p      |
|---------------------|--------|-------|--------------------|-------|--------|--------|
| Fixed effects       |        |       |                    |       |        |        |
|  Constant           | 267.97 | 10.87 | \[245.43, 290.50\] | 24.65 |  22.15 | \<.001 |
|  Days               |  11.44 |  1.10 | \[  9.26,  13.61\] | 10.41 | 125.00 | \<.001 |
| Random effects (SD) |        |       |                    |       |        |        |
|  Participant        |        |       |                    |       |        |        |
|   Constant          |  41.80 |       |                    |       |        |        |
|  Residual           |  30.22 |       |                    |       |        |        |

Table 2

Reaction Time by Days of Sleep Restriction With Random Intercepts

The table has two parts, each under a heading. The fixed effects are
read like ordinary regression coefficients: at baseline, the average
reaction time is about 268 ms, and it increases by about 11 ms per day
of sleep restriction. Below them, the random effects show the standard
deviation (*SD*) of participants’ intercepts around the average
intercept, and the residual *SD* of observations around each
participant’s own line.

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

sleep2 <- lmer(Reaction ~ Days + (Days | Participant), 
               data = d_sleep)

apa_parameters(
  sleep2,
  predictor_parameters = c(
    "Parameter", 
    "Coefficient", 
    "SE", "CI", 
    "t", "df_error", "p")
)
```

| Variable             | B      | SE   | 95% CI             | t(17) | p      |
|----------------------|--------|------|--------------------|-------|--------|
| Fixed effects        |        |      |                    |       |        |
|  Constant            | 267.97 | 8.27 | \[250.53, 285.41\] | 32.42 | \<.001 |
|  Days                |  11.44 | 1.85 | \[  7.54,  15.33\] |  6.20 | \<.001 |
| Random effects (SD)  |        |      |                    |       |        |
|  Participant         |        |      |                    |       |        |
|   Constant           |  30.96 |      |                    |       |        |
|   Days               |   6.77 |      |                    |       |        |
|   r (Constant, Days) |    .18 |      |                    |       |        |
|  Residual            |  25.53 |      |                    |       |        |

Table 3

Reaction Time by Days of Sleep Restriction With Random Intercepts and
Slopes

The average slope is unchanged, but its standard error is larger,
because the model now recognizes that participants’ slopes vary: the
*SD* of the slopes is about 7 ms per day around the average of 11 ms per
day. The correlation (*r*) between participants’ intercepts and slopes
is small. When all fixed effects share the same degrees of freedom, as
here, they appear in the *t* column header.

The “Random effects” heading names the statistic of the rows below it
(*SD*), and each cluster (here, Participant) has a subheading with its
terms indented below it. The correlation names its own statistic (*r*).
With several clusters, crossed or nested, each gets its own subheading.
With `random_scale = "variance"`, the random effects are variances
(*Var*) and covariances (*Cov*) instead:

``` r

apa_parameters(sleep2, random_scale = "variance")
```

| Variable               | B      | SE   | t(17) | p      |
|------------------------|--------|------|-------|--------|
| Fixed effects          |        |      |       |        |
|  Constant              | 267.97 | 8.27 | 32.42 | \<.001 |
|  Days                  |  11.44 | 1.85 |  6.20 | \<.001 |
| Random effects (Var)   |        |      |       |        |
|  Participant           |        |      |       |        |
|   Constant             | 958.35 |      |       |        |
|   Days                 |  45.78 |      |       |        |
|   Cov (Constant, Days) |  37.20 |      |       |        |
|  Residual              | 651.60 |      |       |        |

Table 4

Random-Slope Model With Random Effects as Variances and Covariances

The random-effect rows can be omitted with `random = FALSE`, and the
“Fixed effects” and “Random effects” headings can be omitted with
`effect_headings = FALSE`, in which case each random-effect label names
its cluster and statistic (e.g., Participant: *SD* (Days)). Confidence
intervals can be added with `"CI"`.

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

Table 5

Fixed Effects of the Random-Slope Model

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

Table 6

Fit of the Random-Slope Model

### Comparing models

A list of mixed models is compared with likelihood ratio tests, and the
change in marginal *R*² is shown for each model. When the models differ
in their fixed effects, the likelihood ratio test is based on maximum
likelihood rather than restricted maximum likelihood (REML) estimates,
as it should be.

``` r

apa_parameters(list(sleep0, sleep1, sleep2)) |> 
  # as_tibble() |> 
  rename(`Marginal_*R*^2^` = `*R*^2^~marginal~`,
         `Marginal_&Delta;*R*^2^`=`&Delta;*R*^2^~marginal~`,
         `Conditional_*R*^2^` = `*R*^2^~conditional~`,
         `Conditional_&Delta;*R*^2^`=`&Delta;*R*^2^~conditional~`) |> 
  apa_flextable() |> 
  flextable::merge_h_range(i = ~ Model != "", j1 = 1, j2 = 2)
```

| Model | Variable | B | SE | t | df | p | Marginal |  |  | Conditional |  |
|----|----|----|----|----|----|----|----|----|----|----|----|
|  |  |  |  |  |  |  | R2 | ΔR2 |  | R2 | ΔR2 |
| Model 1 |  |  |  |  |  |  | .00 |  |  | .49 |  |
|  | Fixed effects |  |  |  |  |  |  |  |  |  |  |
|  |  Constant | 307.99 | 10.17 | 30.29 |  17.00 | \<.001 |  |  |  |  |  |
|  | Random effects (SD) |  |  |  |  |  |  |  |  |  |  |
|  |  Participant |  |  |  |  |  |  |  |  |  |  |
|  |   Constant |  40.62 |  |  |  |  |  |  |  |  |  |
|  |  Residual |  41.11 |  |  |  |  |  |  |  |  |  |
| Model 2 |  |  |  |  |  |  | .21 | .21\*\*\* |  | .73 | .23 |
|  | Fixed effects |  |  |  |  |  |  |  |  |  |  |
|  |  Constant | 267.97 | 10.87 | 24.65 |  22.15 | \<.001 |  |  |  |  |  |
|  |  Days |  11.44 |  1.10 | 10.41 | 125.00 | \<.001 |  |  |  |  |  |
|  | Random effects (SD) |  |  |  |  |  |  |  |  |  |  |
|  |  Participant |  |  |  |  |  |  |  |  |  |  |
|  |   Constant |  41.80 |  |  |  |  |  |  |  |  |  |
|  |  Residual |  30.22 |  |  |  |  |  |  |  |  |  |
| Model 3 |  |  |  |  |  |  | .21 | .00\*\*\* |  | .81 | .08 |
|  | Fixed effects |  |  |  |  |  |  |  |  |  |  |
|  |  Constant | 267.97 |  8.27 | 32.42 |  17.00 | \<.001 |  |  |  |  |  |
|  |  Days |  11.44 |  1.85 |  6.20 |  17.00 | \<.001 |  |  |  |  |  |
|  | Random effects (SD) |  |  |  |  |  |  |  |  |  |  |
|  |  Participant |  |  |  |  |  |  |  |  |  |  |
|  |   Constant |  30.96 |  |  |  |  |  |  |  |  |  |
|  |   Days |   6.77 |  |  |  |  |  |  |  |  |  |
|  |   r (Constant, Days) |    .18 |  |  |  |  |  |  |  |  |  |
|  |  Residual |  25.53 |  |  |  |  |  |  |  |  |  |

Table 7

Three Multilevel Models of Reaction Time

The random slope does not change the marginal *R*², because the fixed
effects are the same in both models, but the likelihood ratio test shows
that the slopes vary across participants.

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

Table 8

Comparison of Three Multilevel Models of Reaction Time

### Correlated residuals with nlme

lme4 assumes that, after the random effects are taken into account, a
participant’s residuals are independent. In daily measurements, though,
a participant who is unusually slow on one day is often unusually slow
the next day too. The nlme package can model such residual correlation.
Here the random-slope model is refit with
[`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) and a first-order
autoregressive (AR(1)) correlation among each participant’s residuals:

``` r

sleep_lme <- nlme::lme(
  Reaction ~ Days,
  random = ~ Days | Participant,
  correlation = nlme::corAR1(),
  data = d_sleep
)
tbl_sleep_lme <- apa_parameters(sleep_lme)
tbl_sleep_lme
```

| Variable                      | B      | SE   | t(125) | p      |
|-------------------------------|--------|------|--------|--------|
| Fixed effects                 |        |      |        |        |
|  Constant                     | 266.85 | 7.82 | 34.13  | \<.001 |
|  Days                         |  11.76 | 1.89 |  6.21  | \<.001 |
| Random effects (SD)           |        |      |        |        |
|  Participant                  |        |      |        |        |
|   Constant                    |  20.36 |      |        |        |
|   Days                        |   5.46 |      |        |        |
|   r (Constant, Days)          |    .97 |      |        |        |
|  Residual                     |  30.91 |      |        |        |
|  Residual autocorrelation (φ) |    .45 |      |        |        |

Table 9

Random-Slope Model With First-Order Autoregressive Residuals

The fixed effects are tested with *t* tests whose degrees of freedom are
computed by nlme from the grouping levels at which each effect varies,
rather than with Kenward–Roger degrees of freedom. The parameters of the
residual structure are shown below the residual *SD*: residuals on
adjacent days correlate about .45. The table note names the structure:

``` r

apa_note(tbl_sleep_lme)
```

    [1] "Degrees of freedom are computed by nlme from the grouping levels at which each effect varies. Residuals within Participant have a first-order autoregressive (AR(1)) correlation structure. *SD* = standard deviation; *r* = correlation."

Because the AR(1) structure and the random slopes both describe how a
participant’s reaction times hang together over days, they compete with
each other: with correlated residuals, the intercept *SD* is smaller and
the correlation between intercepts and slopes is nearly 1, a sign that
these data cannot fully separate the two. A likelihood ratio test
compares the model with and without correlated residuals:

``` r

sleep_lme0 <- nlme::lme(
  Reaction ~ Days,
  random = ~ Days | Participant,
  data = d_sleep
)
apa_performance_comparison(
  list(`Independent residuals` = sleep_lme0, `AR(1) residuals` = sleep_lme)
)
```

| Model | R2marginal | ΔR2marginal | R2conditional | ΔR2conditional | χ2 | df | p |
|----|----|----|----|----|----|----|----|
| Independent residuals | .21 | .21 | .81 |  .81 |  |  |  |
| AR(1) residuals | .22 | .01 | .72 | −.09 | 9.64 | 1 | .002 |

Table 10

Comparison of Models With Independent and Autoregressive Residuals

Variance functions (the `weights` argument, e.g.,
`nlme::varIdent(form = ~ 1 | Group)` for a separate residual *SD* in
each group) are displayed the same way, with a row for each parameter
and a sentence in the note.

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

| Variable            | B     | SE   | OR   | z      | p      |
|---------------------|-------|------|------|--------|--------|
| Fixed effects       |       |      |      |        |        |
|  Constant           |  0.55 | 0.39 | 1.74 |   1.43 |  .15   |
|  Anger              |  0.06 | 0.02 | 1.06 |   3.41 | \<.001 |
|  Gender–Male        |  0.32 | 0.19 | 1.38 |   1.67 |  .09   |
|  Behavior–Scold     | −1.06 | 0.18 | 0.35 |  −5.73 | \<.001 |
|  Behavior–Shout     | −2.10 | 0.19 | 0.12 | −11.20 | \<.001 |
|  Mode–Do            | −0.71 | 0.15 | 0.49 |  −4.67 | \<.001 |
|  Situation–Self     | −1.05 | 0.15 | 0.35 |  −6.94 | \<.001 |
| Random effects (SD) |       |      |      |        |        |
|  Person             |       |      |      |        |        |
|   Constant          |  1.34 |      |      |        |        |
|  Item               |       |      |      |        |        |
|   Constant          |  0.34 |      |      |        |        |

Table 11

Logistic Mixed Model of Endorsing Verbally Aggressive Reactions

Each point of trait anger multiplies the odds of endorsing an item by
about 1.06. Scolding and shouting are endorsed less often than cursing,
reactions are endorsed less often when the respondent is to blame than
when someone else is, and people are less likely to say they would *do*
something than that they would *want* to. The random-effect *SD*s are on
the log-odds scale and show that people differ much more than items do,
after the item characteristics are taken into account.

## Count Outcomes with glmmTMB

Counts often vary more than a Poisson model allows (overdispersion) or
have more zeros than it predicts. The
[glmmTMB](https://glmmtmb.github.io/glmmTMB/) package fits mixed models
for such data, including negative binomial models and models with a
separate zero-inflation part. `apa_parameters` displays them like
`glmer` models.

In this made-up daily-diary study, 120 adults who drink heavily recorded
the number of alcoholic drinks they had each day for 2 weeks. Half of
them had been assigned to a brief intervention and half to a control
condition. Do people in the intervention drink less, and do they drink
more on weekends?

``` r

set.seed(12)
n_people <- 120
people <- tibble(
  Person = factor(1:n_people),
  Condition = factor(rep(c("Control", "Intervention"), each = n_people / 2)),
  person_effect = rnorm(n_people, mean = 0, sd = .6)
)

d_drinks <- tidyr::expand_grid(people, `Diary Day` = 1:14) |>
  mutate(
    Day = factor(`Diary Day` %% 7 %in% c(6, 0), labels = c("Weekday", "Weekend")),
    intervention = Condition == "Intervention",
    # Expected drinks on a drinking day
    mu = exp(log(2.5) - .5 * intervention + .6 * (Day == "Weekend") + person_effect),
    # Some days are abstinent days, more often in the intervention
    abstinent = rbinom(n(), 1, plogis(-1.2 + intervention)),
    Drinks = ifelse(
      abstinent == 1,
      0,
      rnbinom(n(), size = ifelse(intervention, 1, 3), mu = mu)
    )
  ) |>
  select(Person, Condition, `Diary Day`, Day, Drinks)
```

The counts have a mean of 2 but a variance of 12.27, and 48% of them are
zeros, far more variability than a Poisson model allows.

### Negative binomial models

A negative binomial model adds a dispersion parameter that lets the
variance grow faster than the mean. People are a random effect, because
each person reported on many days.

``` r

tmb_nb <- glmmTMB::glmmTMB(
  Drinks ~ Condition + Day + (1 | Person),
  family = glmmTMB::nbinom2,
  data = d_drinks
)
apa_parameters(tmb_nb)
```

| Variable                | B     | SE   | IRR  | z     | p      |
|-------------------------|-------|------|------|-------|--------|
| Fixed effects           |       |      |      |       |        |
|  Constant               |  0.65 | 0.08 | 1.91 |  7.66 | \<.001 |
|  Condition–Intervention | −0.73 | 0.12 | 0.48 | −6.24 | \<.001 |
|  Day–Weekend            |  0.63 | 0.08 | 1.88 |  7.85 | \<.001 |
| Random effects (SD)     |       |      |      |       |        |
|  Person                 |       |      |      |       |        |
|   Constant              |  0.49 |      |      |       |        |

Table 12

Negative Binomial Mixed Model of Daily Drinks

As with `glmer`, coefficients are tested with *z* tests, and
coefficients of models with a log link are exponentiated. For count
models, the exponentiated coefficients are incidence rate ratios
(*IRR*): people in the intervention had about half as many drinks as
people in the control condition, and people had almost twice as many
drinks on weekends as on weekdays. The person *SD* is on the log scale.
The dispersion parameter itself is not displayed, because it is not
tested.

### Zero-inflated models

Another account of the many zeros is that some days are abstinent days,
on which a person would not drink at all, while on the other days the
number of drinks varies as usual. A zero-inflated model has two parts: a
conditional model of the counts, and a logistic model of whether a zero
is one of these excess zeros. Here a zero-inflated Poisson model lets
the probability of an excess zero differ between the conditions:

``` r

tmb_zip <- glmmTMB::glmmTMB(
  Drinks ~ Condition + Day + (1 | Person),
  ziformula = ~ Condition,
  family = poisson,
  data = d_drinks
)
tbl_zip <- apa_parameters(tmb_zip)
tbl_zip
```

| Variable                 | B     | SE   | IRR  | z      | p      |
|--------------------------|-------|------|------|--------|--------|
| Conditional model        |       |      |      |        |        |
|  Fixed effects           |       |      |      |        |        |
|   Constant               |  0.96 | 0.08 | 2.62 |  12.59 | \<.001 |
|   Condition–Intervention | −0.24 | 0.11 | 0.79 |  −2.16 |  .03   |
|   Day–Weekend            |  0.55 | 0.04 | 1.73 |  14.51 | \<.001 |
|  Random effects (SD)     |       |      |      |        |        |
|   Person                 |       |      |      |        |        |
|    Constant              |  0.54 |      |      |        |        |
| Zero-inflation model     |       |      |      |        |        |
|  Fixed effects           |       |      |      |        |        |
|   Constant               | −0.95 | 0.09 |      | −10.54 | \<.001 |
|   Condition–Intervention |  1.18 | 0.12 |      |   9.74 | \<.001 |

Table 13

Zero-Inflated Poisson Mixed Model of Daily Drinks

Each part of the model has its own heading, with its fixed and random
effects under subheadings. Only the conditional model’s coefficients are
exponentiated, because the zero-inflation coefficients are log odds: the
probability of an excess zero is about .28 in the control condition (the
inverse logit of −0.95) and .56 in the intervention (the inverse logit
of −0.95 + 1.18). People in the intervention had more abstinent days and
somewhat fewer drinks on the other days. The table note says what the
zero-inflation coefficients are:

``` r

apa_note(tbl_zip)
```

    [1] "Zero-inflation coefficients are log odds of an excess zero (a zero not produced by the conditional model). *IRR* = incidence rate ratio; *SD* = standard deviation."

The two models explain the zeros differently, and their *AIC*s can be
compared with `apa_performance`. Here the negative binomial model fits
better:

``` r

apa_performance(
  list(`Negative binomial` = tmb_nb, `Zero-inflated Poisson` = tmb_zip),
  metrics = c("AIC", "BIC")
)
```

| Model                 | AIC      | BIC      |
|-----------------------|----------|----------|
| Negative binomial     | 5,989.98 | 6,017.12 |
| Zero-inflated Poisson | 6,300.71 | 6,333.27 |

Table 14

Information Criteria of the Negative Binomial and Zero-Inflated Poisson
Models

### Dispersion models

With `dispformula`, the dispersion parameter itself is modeled, and the
table has a “Dispersion model” part. For negative binomial models, a
larger dispersion parameter means less overdispersion, so the negative
coefficient here means that the counts of people in the intervention are
more overdispersed: many of their days have no drinks, but some have
many. As the note says, dispersion coefficients are on the log scale.

``` r

glmmTMB::glmmTMB(
  Drinks ~ Condition + Day + (1 | Person),
  dispformula = ~ Condition,
  family = glmmTMB::nbinom2,
  data = d_drinks
) |>
  apa_parameters()
```

| Variable                 | B     | SE   | IRR  | z     | p      |
|--------------------------|-------|------|------|-------|--------|
| Conditional model        |       |      |      |       |        |
|  Fixed effects           |       |      |      |       |        |
|   Constant               |  0.63 | 0.08 | 1.88 |  8.09 | \<.001 |
|   Condition–Intervention | −0.68 | 0.12 | 0.51 | −5.68 | \<.001 |
|   Day–Weekend            |  0.64 | 0.08 | 1.89 |  8.25 | \<.001 |
|  Random effects (SD)     |       |      |      |       |        |
|   Person                 |       |      |      |       |        |
|    Constant              |  0.47 |      |      |       |        |
| Dispersion model         |       |      |      |       |        |
|  Fixed effects           |       |      |      |       |        |
|   Constant               |  0.05 | 0.09 |      |  0.55 |  .58   |
|   Condition–Intervention | −1.16 | 0.13 |      | −9.13 | \<.001 |

Table 15

Negative Binomial Mixed Model With Dispersion Depending on Condition

### Fit and comparisons

`apa_performance` reports the marginal and conditional *R*² and the ICC
of glmmTMB models, and nested models are compared with likelihood ratio
tests, as for lme4 models:

``` r

tmb_nb0 <- glmmTMB::glmmTMB(
  Drinks ~ Condition + (1 | Person),
  family = glmmTMB::nbinom2,
  data = d_drinks
)
apa_performance_comparison(
  list(`Condition` = tmb_nb0, `Condition and day` = tmb_nb)
)
```

| Model | R2marginal | ΔR2marginal | R2conditional | ΔR2conditional | χ2 | df | p |
|----|----|----|----|----|----|----|----|
| Condition | .09 | .09 | .24 | .24 |  |  |  |
| Condition and day | .14 | .05 | .29 | .04 | 62.11 | 1 | \<.001 |

Table 16

Comparison of Models of Daily Drinks With and Without Day of the Week

## References

Belenky, G., Wesensten, N. J., Thorne, D. R., Thomas, M. L., Sing, H.
C., Redmond, D. P., Russo, M. B., & Balkin, T. J. (2003). Patterns of
performance degradation and restoration during sleep restriction and
subsequent recovery: A sleep dose-response study. *Journal of Sleep
Research*, *12*(1), 1–12.
<https://doi.org/10.1046/j.1365-2869.2003.00337.x>
