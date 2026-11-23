# Estimated Marginal Means, Contrasts, and Simple Slopes

``` r

library(apa7)
library(modelbased)
library(dplyr)
```

After an ANOVA or a regression with an interaction, readers usually want
to know more than whether an effect is significant. Which groups differ?
How large is the effect of a predictor at different values of a
moderator? The [modelbased](https://easystats.github.io/modelbased/)
package answers these questions, and three apa7 functions put its
answers in APA-style tables:

| Function | Table | modelbased function |
|----|----|----|
| `apa_means` | Estimated marginal means with confidence intervals | `estimate_means` |
| `apa_contrasts` | Differences between means (e.g., pairwise comparisons) | `estimate_contrasts` |
| `apa_slopes` | Simple slopes of a predictor at values of a moderator | `estimate_slopes` |

Each function takes either a model or the output of the modelbased
function. With a model, the other arguments (e.g., `by`, `contrast`,
`trend`, and `p_adjust`) go to the modelbased function, so these two
calls make the same table:

``` r

apa_contrasts(fit, contrast = "dose", p_adjust = "holm")

estimate_contrasts(fit, contrast = "dose", p_adjust = "holm") |>
  apa_contrasts()
```

With a model, variable names in `by`, `contrast`, and `trend` can be
quoted or unquoted, so `apa_contrasts(fit, contrast = dose)` works, too,
as does `by = c(supplement, dose)`. Values, such as `"hp=[sd]"`, can be
specified in a list (e.g., `list(hp = "[sd]"`).

## Estimated Marginal Means

The `ToothGrowth` data set has the tooth length of 60 guinea pigs given
one of three doses of vitamin C (0.5, 1, or 2 mg/day) by one of two
delivery methods (orange juice or ascorbic acid). Here is a two-way
ANOVA:

``` r

d_tooth <- ToothGrowth |> 
  rename(length = len,
         supplement = supp) |> 
  mutate(dose = factor(dose),
         supplement = factor(supplement, labels = c("Orange juice", "Ascorbic acid")))

fit <- lm(length ~ supplement * dose, data = d_tooth)

apa_anova(fit) |>
  apa_flextable(note = TRUE)
```

| Source | SS | df | MS | F | p | η2p |
|----|----|----|----|----|----|----|
| Supplement |   205.35 |  1 |   205.35 | 15.57 | \<.001 | .22 |
| Dose | 2,426.43 |  2 | 1,213.22 | 92.00 | \<.001 | .77 |
| Supplement × Dose |   108.32 |  2 |    54.16 |  4.11 |  .02  | .13 |
| Error |   712.11 | 54 |    13.19 |  |  |  |
| Note. Sums of squares are sequential (Type I). η2p = partial eta squared. |  |  |  |  |  |  |

The means of each dose, averaged over the two delivery methods:

``` r

apa_means(fit, by = dose) |>
  apa_flextable(note = TRUE)
```

| Dose | M | SE | 95% CI |
|----|----|----|----|
| 0.5 | 10.60 | 0.81 | \[ 8.98, 12.23\] |
| 1 | 19.73 | 0.81 | \[18.11, 21.36\] |
| 2 | 26.10 | 0.81 | \[24.47, 27.73\] |
| Note. Values are estimated marginal means of Length. Estimates are averaged over the levels of Supplement. M = estimated marginal mean; CI = confidence interval. |  |  |  |

With more than one variable in `by`, the last variable labels the rows,
and each level of the other variables gets a heading row:

``` r

apa_means(fit, by = c(supplement, dose)) |>
  apa_flextable(note = TRUE)
```

| Dose | M | SE | 95% CI |
|----|----|----|----|
| Supplement: Orange juice |  |  |  |
|  0.5 | 13.23 | 1.15 | \[10.93, 15.53\] |
|  1 | 22.70 | 1.15 | \[20.40, 25.00\] |
|  2 | 26.06 | 1.15 | \[23.76, 28.36\] |
| Supplement: Ascorbic acid |  |  |  |
|  0.5 |  7.98 | 1.15 | \[ 5.68, 10.28\] |
|  1 | 16.77 | 1.15 | \[14.47, 19.07\] |
|  2 | 26.14 | 1.15 | \[23.84, 28.44\] |
| Note. Values are estimated marginal means of Length. M = estimated marginal mean; CI = confidence interval. |  |  |  |

## Contrasts

Pairwise comparisons of the doses, with *p*-values adjusted by Holm’s
method:

``` r

apa_contrasts(fit, 
              contrast = dose, 
              p_adjust = "holm") |>
  apa_flextable(note = TRUE)
```

| Dose | Difference | SE | 95% CI | d | 95% CI for d | t(54) | p |
|----|----|----|----|----|----|----|----|
| 1 − 0.5 |  9.13 | 1.15 | \[ 6.83, 11.43\] | 2.51 | \[1.72, 3.31\] |  7.95 | \<.001 |
| 2 − 0.5 | 15.50 | 1.15 | \[13.19, 17.80\] | 4.27 | \[3.23, 5.31\] | 13.49 | \<.001 |
| 2 − 1 |  6.37 | 1.15 | \[ 4.06,  8.67\] | 1.75 | \[1.03, 2.47\] |  5.54 | \<.001 |
| Note. Values are differences between estimated marginal means of Length (the first level minus the second). Estimates are averaged over the levels of Supplement. d = difference divided by the residual standard deviation of the model (3.63); its confidence interval accounts for the uncertainty in the residual standard deviation. p-values are adjusted for multiple comparisons with Holm’s method (Holm, 1979). CI = confidence interval. |  |  |  |  |  |  |  |

Each row is the first level minus the second (e.g., 1 − 0.5), and the
note says so. To follow up the interaction, compare the doses separately
for each delivery method with `by`:

``` r

apa_contrasts(fit,
              contrast = dose,
              by = supplement,
              p_adjust = "holm") |>
  apa_flextable(note = TRUE)
```

| Dose | Difference | SE | 95% CI | d | 95% CI for d | t(54) | p |
|----|----|----|----|----|----|----|----|
| Supplement: Orange juice |  |  |  |  |  |  |  |
|  1 − 0.5 |  9.47 | 1.62 | \[ 6.21, 12.73\] | 2.61 | \[1.58, 3.64\] |  5.83 | \<.001 |
|  2 − 0.5 | 12.83 | 1.62 | \[ 9.57, 16.09\] | 3.53 | \[2.41, 4.66\] |  7.90 | \<.001 |
|  2 − 1 |  3.36 | 1.62 | \[ 0.10,  6.62\] | 0.93 | \[0.01, 1.84\] |  2.07 |  .04  |
| Supplement: Ascorbic acid |  |  |  |  |  |  |  |
|  1 − 0.5 |  8.79 | 1.62 | \[ 5.53, 12.05\] | 2.42 | \[1.41, 3.43\] |  5.41 | \<.001 |
|  2 − 0.5 | 18.16 | 1.62 | \[14.90, 21.42\] | 5.00 | \[3.68, 6.32\] | 11.18 | \<.001 |
|  2 − 1 |  9.37 | 1.62 | \[ 6.11, 12.63\] | 2.58 | \[1.55, 3.61\] |  5.77 | \<.001 |
| Note. Values are differences between estimated marginal means of Length (the first level minus the second). d = difference divided by the residual standard deviation of the model (3.63); its confidence interval accounts for the uncertainty in the residual standard deviation. p-values are adjusted for multiple comparisons with Holm’s method (Holm, 1979). CI = confidence interval. |  |  |  |  |  |  |  |

The `p_adjust` argument of `estimate_contrasts` offers several methods,
including `"tukey"`, `"bonferroni"`, `"holm"`, and `"fdr"`. The note
names the method, or says that the *p*-values are not adjusted.

### Effect sizes

The contrast tables above include Cohen’s *d* with its confidence
interval: each difference divided by the model’s residual standard
deviation (here, 3.63), as in `emmeans::eff_size`. The interval accounts
for the uncertainty in the residual standard deviation, and the note
says how *d* was computed. Use `effect_size = FALSE` to leave *d* out.

*d* is added automatically for models of continuous outcomes with an
identity link, such as `lm` and `aov`. For mixed models, differences can
be standardized by different standard deviations (e.g., the residual
*SD* for differences within clusters, or the total *SD*), so give the
one you want with `sd`:

``` r

apa_contrasts(fit_mixed, contrast = condition, sd = 12.5)
```

*d* is not computed for logistic regression and other generalized linear
models, whose differences (e.g., in probabilities) are already on an
interpretable scale.

## Simple Slopes

A moderation analysis asks whether the effect of one predictor depends
on another. In the `mtcars` data, the effect of a car’s weight on its
fuel efficiency (miles per gallon) may depend on its horsepower:

``` r

d_mtcars <- mtcars |> 
  mutate(am = factor(am, 
                     levels = c(0,1), 
                     labels = c("Automatic", "Manual"))) |> 
  rename(MPG = mpg,
         Horsepower = hp,
         Weight = wt,
         Transmission = am)


fit_mod <- lm(MPG ~ Weight * Horsepower, data = d_mtcars)

apa_parameters(fit_mod) |>
  apa_flextable(note = TRUE)
```

| Variable                            | B     | SE   | β     | t(28) | p      |
|-------------------------------------|-------|------|-------|-------|--------|
| Constant                            | 49.81 | 3.61 |   .00 | 13.82 | \<.001 |
| Weight                              | −8.22 | 1.27 | −1.33 | −6.47 | \<.001 |
| Horsepower                          | −0.12 | 0.02 | −1.37 | −4.86 | \<.001 |
| Weight × Horsepower                 |  0.03 | 0.01 |  1.58 |  3.75 | \<.001 |
| Note. β = standardized coefficient. |       |      |       |       |        |

Without `by`, `apa_slopes` gives the average slope of the predictor:

``` r

apa_slopes(fit_mod, trend = Weight) |>
  apa_flextable(note = TRUE)
```

| Predictor | Slope | SE | 95% CI | t(28) | p |
|----|----|----|----|----|----|
| Weight | −4.13 | 0.53 | \[−5.22, −3.05\] | −7.80 | \<.001 |
| Note. The estimate is the average slope of Weight (the expected change in MPG for a one-unit increase in Weight). CI = confidence interval. |  |  |  |  |  |

Simple slopes show the effect of weight at chosen values of horsepower
(here, 100, 150, and 200):

``` r

apa_slopes(fit_mod, 
           trend = Weight, 
           by = list(Horsepower = c(100, 150, 200))) |>
  apa_flextable(note = TRUE)
```

| Horsepower | Slope | SE | 95% CI | t(28) | p |
|----|----|----|----|----|----|
| 100 | −5.43 | 0.67 | \[−6.80, −4.06\] | −8.12 | \<.001 |
| 150 | −4.04 | 0.53 | \[−5.12, −2.96\] | −7.67 | \<.001 |
| 200 | −2.65 | 0.62 | \[−3.92, −1.38\] | −4.27 | \<.001 |
| Note. Values are simple slopes of Weight (the expected change in MPG for a one-unit increase in Weight) at each value of Horsepower. Johnson–Neyman intervals (Johnson & Neyman, 1936): the slope of Weight is significant (p \< .05) when Horsepower is from 52 to 236.48 (observed range: 52 to 335). CI = confidence interval. |  |  |  |  |  |

Weight lowers fuel efficiency less in more powerful cars.

The notes of the simple-slopes tables above also give the Johnson–Neyman
intervals: the values of horsepower, within its observed range, at which
the slope of weight is significant. Rather than testing the slope at a
few chosen values of the moderator, the Johnson–Neyman technique finds
the values at which the slope’s test becomes significant, so readers can
see where the effect holds. apa7 computes the boundaries exactly from
the model’s coefficients and adds the intervals to the note whenever the
slope depends on a single continuous moderator through a two-way
interaction. Use `johnson_neyman = FALSE` to leave them out.

modelbased has shortcuts for common moderator values, and `apa_slopes`
names the values they give. A numeric moderator named without values
(`by = "hp"`) is set to its mean and 1 standard deviation above and
below it, as with `by = "hp=[sd]"`:

``` r

apa_slopes(fit_mod, trend = Weight, by = Horsepower) |>
  apa_flextable(note = TRUE)
```

| Horsepower | Slope | SE | 95% CI | t(28) | p |
|----|----|----|----|----|----|
| −1SD (78.12) | −6.04 | 0.78 | \[−7.64, −4.44\] | −7.75 | \<.001 |
| M (146.69) | −4.13 | 0.53 | \[−5.22, −3.05\] | −7.80 | \<.001 |
| +1SD (215.25) | −2.22 | 0.69 | \[−3.63, −0.82\] | −3.24 |  .003 |
| Note. Values are simple slopes of Weight (the expected change in MPG for a one-unit increase in Weight) at each value of Horsepower. Johnson–Neyman intervals (Johnson & Neyman, 1936): the slope of Weight is significant (p \< .05) when Horsepower is from 52 to 236.48 (observed range: 52 to 335). CI = confidence interval. |  |  |  |  |  |

With `by = "hp=[fivenum]"`, the slopes are at the minimum, lower
quartile, median, upper quartile, and maximum:

``` r

apa_slopes(fit_mod, trend = Weight, 
           by = list(Horsepower = "[fivenum]")) |>
  apa_flextable(note = TRUE)
```

| Horsepower | Slope | SE | 95% CI | t(28) | p |
|----|----|----|----|----|----|
| Min (52) | −6.77 | 0.93 | \[−8.68, −4.86\] | −7.26 | \<.001 |
| Q1 (96) | −5.54 | 0.69 | \[−6.95, −4.13\] | −8.06 | \<.001 |
| Mdn (123) | −4.79 | 0.58 | \[−5.98, −3.61\] | −8.28 | \<.001 |
| Q3 (180) | −3.20 | 0.56 | \[−4.34, −2.07\] | −5.77 | \<.001 |
| Max (335) |  1.11 | 1.43 | \[−1.82,  4.04\] |  0.78 |  .44  |
| Note. Values are simple slopes of Weight (the expected change in MPG for a one-unit increase in Weight) at each value of Horsepower. Johnson–Neyman intervals (Johnson & Neyman, 1936): the slope of Weight is significant (p \< .05) when Horsepower is from 52 to 236.48 (observed range: 52 to 335). CI = confidence interval; Q1 = first quartile (lower hinge); Q3 = third quartile (upper hinge). |  |  |  |  |  |

Other named shortcuts are `"[quartiles]"`, `"[minmax]"`, and `"[mad]"`
(the median and 1 median absolute deviation above and below it). The
shortcuts, and the default for a numeric variable named without values,
work the same way in `apa_means` and `apa_contrasts`. For example, the
difference between manual and automatic transmissions at three values of
horsepower:

``` r

fit_am <- lm(MPG ~ Transmission * Horsepower, data = d_mtcars)

apa_contrasts(fit_am, contrast = Transmission, by = Horsepower) |>
  apa_flextable(note = TRUE)
```

| Transmission | Difference | SE | 95% CI | d | 95% CI for d | t(28) | p |
|----|----|----|----|----|----|----|----|
| Horsepower: −1SD (78.12) |  |  |  |  |  |  |  |
|  Manual − Automatic | 5.25 | 1.58 | \[2.00, 8.50\] | 1.77 | \[0.57, 2.97\] | 3.31 |  .003 |
| Horsepower: M (146.69) |  |  |  |  |  |  |  |
|  Manual − Automatic | 5.28 | 1.10 | \[3.03, 7.53\] | 1.78 | \[0.88, 2.69\] | 4.80 | \<.001 |
| Horsepower: +1SD (215.25) |  |  |  |  |  |  |  |
|  Manual − Automatic | 5.30 | 1.57 | \[2.10, 8.51\] | 1.79 | \[0.60, 2.98\] | 3.39 |  .002 |
| Note. Values are differences between estimated marginal means of MPG (the first level minus the second). d = difference divided by the residual standard deviation of the model (2.96); its confidence interval accounts for the uncertainty in the residual standard deviation. p-values are not adjusted for multiple comparisons. CI = confidence interval. |  |  |  |  |  |  |  |

## Logistic Regression

For models with a link function, modelbased reports estimates on the
response scale by default. For logistic regression, the estimates are
probabilities, which have no leading zero in APA style, and the
contrasts are differences in probabilities. The `birthwt` data set in
the MASS package has the birth weights of 189 babies. Here, the
probability of a low birth weight (under 2.5 kg) depends on the mother’s
race, holding her weight at its mean and averaging over whether she
smoked during pregnancy:

``` r

d_birth <- MASS::birthwt |> 
  mutate(race = factor(race, labels = c("White", "Black", "Other")),
         smoke = factor(smoke, labels = c("No", "Yes"))) |> 
  rename(low_birth_weight = low,
         mother_weight = lwt)

fit_logistic <- glm(
  low_birth_weight ~ race + smoke + mother_weight,
  data = d_birth,
  family = binomial
)

apa_means(fit_logistic, by = race) |>
  apa_flextable(note = TRUE)
```

| Race | Probability | 95% CI |
|----|----|----|
| White | .21 | \[.14, .31\] |
| Black | .50 | \[.30, .70\] |
| Other | .42 | \[.29, .56\] |
| Note. Values are estimated probabilities of Low Birth Weight. Mother Weight is held at its mean (129.81), and estimates are averaged over the levels of Smoke. Estimates are on the response scale (back-transformed from the logit scale). CI = confidence interval. |  |  |

``` r

apa_contrasts(fit_logistic, contrast = race, p_adjust = "holm") |>
  apa_flextable(note = TRUE)
```

| Race | Difference | SE | 95% CI | z | p |
|----|----|----|----|----|----|
| Black − White |  .27 | .11 | \[ .06, .49\] |  2.47 | .04 |
| Other − White |  .20 | .08 | \[ .04, .36\] |  2.41 | .04 |
| Other − Black | −.07 | .12 | \[−.31, .17\] | −0.60 | .55 |
| Note. Values are differences between estimated probabilities of Low Birth Weight (the first level minus the second). Mother Weight is held at its mean (129.81), and estimates are averaged over the levels of Smoke. Differences are on the response scale (back-transformed from the logit scale). p-values are adjusted for multiple comparisons with Holm’s method (Holm, 1979). CI = confidence interval. |  |  |  |  |  |

With `predict = "link"`, modelbased gives estimates on the link scale
(here, log odds) instead, and the note says so.

``` r

apa_contrasts(fit_logistic, 
              contrast = race, 
              p_adjust = "holm", 
              predict = "link") |>
  apa_flextable(note = TRUE)
```

| Race | Difference | SE | 95% CI | z | p |
|----|----|----|----|----|----|
| Black − White |  1.29 | 0.51 | \[ 0.29, 2.29\] |  2.53 | .04 |
| Other − White |  0.97 | 0.41 | \[ 0.16, 1.78\] |  2.35 | .04 |
| Other − Black | −0.32 | 0.53 | \[−1.35, 0.71\] | −0.61 | .54 |
| Note. Values are differences between estimated marginal means of Low Birth Weight (the first level minus the second). Mother Weight is held at its mean (129.81), and estimates are averaged over the levels of Smoke. Differences are on the logit scale. p-values are adjusted for multiple comparisons with Holm’s method (Holm, 1979). CI = confidence interval. |  |  |  |  |  |

## Options

- `starred = "p"` adds significance stars, and `bolded = "p"` bolds
  significant values, as in `apa_parameters`.
- `format_variable_names = FALSE` keeps variable names exactly as they
  are.
- `effect_size = FALSE` leaves Cohen’s *d* out of contrast tables, and
  `sd` sets the standard deviation it is computed with.
- Bayesian models are not yet supported.
