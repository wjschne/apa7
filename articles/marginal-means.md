# Estimated Marginal Means, Contrasts, and Simple Slopes

``` r

library(apa7)
library(modelbased)
library(dplyr)
apa7_defaults(font_family = "Open Sans", line_spacing = 1.5)
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

apa_contrasts(fit, contrast = "Authoritarianism", p_adjust = "holm")

estimate_contrasts(fit, contrast = "Authoritarianism", p_adjust = "holm") |>
  apa_contrasts()
```

With a model, variable names in `by`, `contrast`, and `trend` can be
quoted or unquoted, so `apa_contrasts(fit, contrast = Authoritarianism)`
works, too, as does `` by = c(`Partner Status`, Authoritarianism) ``.
Values, such as `"Age=[sd]"`, can be specified in a list (e.g.,
`list(Age = "[sd]")`).

## Estimated Marginal Means

In the `Moore` data set in the carData package ([Moore & Krupat,
1971](#ref-mooreRelationshipSourceStatus1971)), 45 participants made
judgments with a partner of high or low status, and the number of times
they conformed to their partner’s judgments was recorded. Their
authoritarianism was classified as low, medium, or high. Here is a
two-way ANOVA:

``` r

d_moore <- carData::Moore |>
  transmute(
    Conformity = conformity,
    `Partner Status` = factor(partner.status,
                              levels = c("low", "high"),
                              labels = c("Low", "High")),
    Authoritarianism = factor(fcategory,
                              levels = c("low", "medium", "high"),
                              labels = c("Low", "Medium", "High"))
  )

fit <- lm(Conformity ~ `Partner Status` * Authoritarianism, data = d_moore)

apa_anova(fit) |>
  apa_flextable(note = TRUE)
```

| Source | SS | df | MS | F | p | η2p |
|----|----|----|----|----|----|----|
| Partner Status | 204.33 |  1 | 204.33 | 9.74 | .003 | .20 |
| Authoritarianism |  11.61 |  2 |   5.81 | 0.28 | .76  | .01 |
| Partner Status × Authoritarianism | 175.49 |  2 |  87.74 | 4.18 | .02  | .18 |
| Error | 817.76 | 39 |  20.97 |  |  |  |
| Note. Sums of squares are sequential (Type I). η2p = partial eta squared. |  |  |  |  |  |  |

Table 1

Analysis of Variance of Conformity by Partner Status and
Authoritarianism

The means of each level of authoritarianism, averaged over the two
partner statuses:

``` r

apa_means(fit, by = Authoritarianism) |>
  apa_flextable(note = TRUE)
```

| Authoritarianism | M | SE | 95% CI |
|----|----|----|----|
| Low | 13.15 | 1.25 | \[10.61, 15.69\] |
| Medium | 10.76 | 1.34 | \[ 8.06, 13.47\] |
| High | 12.24 | 1.18 | \[ 9.84, 14.64\] |
| Note. Values are estimated marginal means of Conformity. Estimates are averaged over the levels of Partner Status. M = estimated marginal mean; CI = confidence interval. |  |  |  |

Table 2

Estimated Marginal Means of Conformity by Authoritarianism

With more than one variable in `by`, the last variable labels the rows,
and each level of the other variables gets a heading row:

``` r

apa_means(fit, by = c(`Partner Status`, Authoritarianism)) |>
  apa_flextable(note = TRUE)
```

| Authoritarianism | M | SE | 95% CI |
|----|----|----|----|
| Partner Status: Low |  |  |  |
|  Low |  8.90 | 1.45 | \[ 5.97, 11.83\] |
|  Medium |  7.25 | 2.29 | \[ 2.62, 11.88\] |
|  High | 12.63 | 1.62 | \[ 9.35, 15.90\] |
| Partner Status: High |  |  |  |
|  Low | 17.40 | 2.05 | \[13.26, 21.54\] |
|  Medium | 14.27 | 1.38 | \[11.48, 17.07\] |
|  High | 11.86 | 1.73 | \[ 8.36, 15.36\] |
| Note. Values are estimated marginal means of Conformity. M = estimated marginal mean; CI = confidence interval. |  |  |  |

Table 3

Estimated Marginal Means of Conformity by Partner Status and
Authoritarianism

## Contrasts

Pairwise comparisons of the levels of authoritarianism, with *p*-values
adjusted by Holm’s method:

``` r

apa_contrasts(fit, 
              contrast = Authoritarianism, 
              p_adjust = "holm") |>
  apa_flextable(note = TRUE)
```

| Authoritarianism | Difference | SE | 95% CI | d | 95% CI for d | t(39) | p |
|----|----|----|----|----|----|----|----|
| Medium − Low | −2.39 | 1.83 | \[−6.10, 1.32\] | −0.52 | \[−1.34, 0.30\] | −1.30 | .60 |
| High − Low | −0.91 | 1.73 | \[−4.40, 2.58\] | −0.20 | \[−0.96, 0.56\] | −0.53 | .82 |
| High − Medium |  1.48 | 1.79 | \[−2.13, 5.09\] |  0.32 | \[−0.47, 1.12\] |  0.83 | .82 |
| Note. Values are differences between estimated marginal means of Conformity (the first level minus the second). Estimates are averaged over the levels of Partner Status. d = difference divided by the residual standard deviation of the model (4.58); its confidence interval accounts for the uncertainty in the residual standard deviation. p-values are adjusted for multiple comparisons with Holm’s method ([Holm,](#ref-holmSimpleSequentiallyRejective1979)[ ](#ref-holmSimpleSequentiallyRejective1979)[1979](#ref-holmSimpleSequentiallyRejective1979)). CI = confidence interval. |  |  |  |  |  |  |  |

Table 4

Pairwise Comparisons of Levels of Authoritarianism

Each row is the first level minus the second (e.g., Medium − Low), and
the note says so. Averaged over partner status, the levels of
authoritarianism hardly differ. To follow up the interaction, compare
them separately for each partner status with `by`:

``` r

apa_contrasts(fit,
              contrast = Authoritarianism,
              by = `Partner Status`,
              p_adjust = "holm") |>
  apa_flextable(note = TRUE)
```

| Authoritarianism | Difference | SE | 95% CI | d | 95% CI for d | t(39) | p |
|----|----|----|----|----|----|----|----|
| Partner Status: Low |  |  |  |  |  |  |  |
|  Medium − Low | −1.65 | 2.71 | \[ −7.13,  3.83\] | −0.36 | \[−1.56, 0.84\] | −0.61 | .64 |
|  High − Low |  3.72 | 2.17 | \[ −0.67,  8.12\] |  0.81 | \[−0.16, 1.79\] |  1.71 | .38 |
|  High − Medium |  5.38 | 2.80 | \[ −0.30, 11.05\] |  1.17 | \[−0.09, 2.44\] |  1.92 | .31 |
| Partner Status: High |  |  |  |  |  |  |  |
|  Medium − Low | −3.13 | 2.47 | \[ −8.12,  1.87\] | −0.68 | \[−1.79, 0.42\] | −1.27 | .64 |
|  High − Low | −5.54 | 2.68 | \[−10.97, −0.12\] | −1.21 | \[−2.43, 0.01\] | −2.07 | .27 |
|  High − Medium | −2.42 | 2.21 | \[ −6.89,  2.06\] | −0.53 | \[−1.51, 0.46\] | −1.09 | .64 |
| Note. Values are differences between estimated marginal means of Conformity (the first level minus the second). d = difference divided by the residual standard deviation of the model (4.58); its confidence interval accounts for the uncertainty in the residual standard deviation. p-values are adjusted for multiple comparisons with Holm’s method ([Holm,](#ref-holmSimpleSequentiallyRejective1979)[ ](#ref-holmSimpleSequentiallyRejective1979)[1979](#ref-holmSimpleSequentiallyRejective1979)). CI = confidence interval. |  |  |  |  |  |  |  |

Table 5

Pairwise Comparisons of Levels of Authoritarianism Within Each Partner
Status

The `p_adjust` argument of `estimate_contrasts` offers several methods,
including `"tukey"`, `"bonferroni"`, `"holm"`, and `"fdr"`. The note
names the method, or says that the *p*-values are not adjusted.

### Effect sizes

The contrast tables above include Cohen’s *d* with its confidence
interval: each difference divided by the model’s residual standard
deviation (here, 4.58), as in `emmeans::eff_size`. The interval accounts
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
on another. The `globalWarm` data set in the psychTools package, from
Hayes ([2022](#ref-hayesIntroductionMediationModeration2022)), has
survey answers from 815 U.S. adults. Does the effect of negative
emotions about climate change on support for government action to
address it depend on age?

``` r

d_climate <- psychTools::globalWarm |>
  transmute(
    Support = as.numeric(govact),
    `Negative Emotions` = as.numeric(negemot),
    Age = as.numeric(age)
  )

fit_mod <- lm(Support ~ `Negative Emotions` * Age, data = d_climate)

apa_parameters(fit_mod) |>
  apa_flextable(note = TRUE)
```

| Variable                            | B     | SE   | β    | t(811) | p      |
|-------------------------------------|-------|------|------|--------|--------|
| Constant                            |  4.33 | 0.33 |  .00 | 13.15  | \<.001 |
| Negative Emotions                   |  0.15 | 0.09 |  .17 |  1.73  |  .08   |
| Age                                 | −0.03 | 0.01 | −.37 | −5.01  | \<.001 |
| Negative Emotions × Age             |  0.01 | 0.00 |  .51 |  4.48  | \<.001 |
| Note. β = standardized coefficient. |       |      |      |        |        |

Table 6

Regression of Support for Government Action on Negative Emotions, Age,
and Their Interaction

Without `by`, `apa_slopes` gives the average slope of the predictor:

``` r

apa_slopes(fit_mod, trend = `Negative Emotions`) |>
  apa_flextable(note = TRUE)
```

| Predictor | Slope | SE | 95% CI | t(811) | p |
|----|----|----|----|----|----|
| Negative Emotions | 0.50 | 0.03 | \[0.45, 0.55\] | 19.81 | \<.001 |
| Note. The estimate is the average slope of Negative Emotions (the expected change in Support for a one-unit increase in Negative Emotions). CI = confidence interval. |  |  |  |  |  |

Table 7

Average Slope of Negative Emotions

Simple slopes show the effect of negative emotions at chosen ages (here,
30, 50, and 70):

``` r

apa_slopes(fit_mod, 
           trend = `Negative Emotions`, 
           by = list(Age = c(30, 50, 70))) |>
  apa_flextable(note = TRUE)
```

| Age | Slope | SE | 95% CI | t(811) | p |
|----|----|----|----|----|----|
| 30 | 0.36 | 0.04 | \[0.28, 0.44\] |  8.63 | \<.001 |
| 50 | 0.50 | 0.03 | \[0.45, 0.55\] | 19.99 | \<.001 |
| 70 | 0.65 | 0.04 | \[0.57, 0.72\] | 16.41 | \<.001 |
| Note. Values are simple slopes of Negative Emotions (the expected change in Support for a one-unit increase in Negative Emotions) at each value of Age. Johnson–Neyman intervals ([Johnson](#ref-johnsonTestsCertainLinear1936)[ ](#ref-johnsonTestsCertainLinear1936)[&](#ref-johnsonTestsCertainLinear1936)[ ](#ref-johnsonTestsCertainLinear1936)[Neyman,](#ref-johnsonTestsCertainLinear1936)[ ](#ref-johnsonTestsCertainLinear1936)[1936](#ref-johnsonTestsCertainLinear1936)): the slope of Negative Emotions is significant (p ≤ .05) across the observed range of Age (17 to 87). CI = confidence interval. |  |  |  |  |  |

Table 8

Simple Slopes of Negative Emotions at Selected Ages

Negative emotions about climate change go with more support for
government action at every age, and more so among older people.

The notes of the simple-slopes tables above also give the Johnson–Neyman
intervals: the values of age, within its observed range, at which the
slope of negative emotions is significant (here, the whole range).
Rather than testing the slope at a few chosen values of the moderator,
the Johnson–Neyman technique finds the values at which the slope’s test
becomes significant, so readers can see where the effect holds. apa7
computes the boundaries exactly from the model’s coefficients and adds
the intervals to the note whenever the slope depends on a single
continuous moderator through a two-way interaction. Use
`johnson_neyman = FALSE` to leave them out.

modelbased has shortcuts for common moderator values, and `apa_slopes`
names the values they give. A numeric moderator named without values
(`by = "Age"`) is set to its mean and 1 standard deviation above and
below it, as with `by = "Age=[sd]"`:

``` r

apa_slopes(fit_mod, trend = `Negative Emotions`, by = Age) |>
  apa_flextable(note = TRUE)
```

| Age | Slope | SE | 95% CI | t(811) | p |
|----|----|----|----|----|----|
| −1SD (33.20) | 0.38 | 0.04 | \[0.31, 0.46\] | 10.13 | \<.001 |
| M (49.54) | 0.50 | 0.03 | \[0.45, 0.55\] | 19.81 | \<.001 |
| +1SD (65.87) | 0.62 | 0.03 | \[0.55, 0.69\] | 17.85 | \<.001 |
| Note. Values are simple slopes of Negative Emotions (the expected change in Support for a one-unit increase in Negative Emotions) at each value of Age. Johnson–Neyman intervals ([Johnson](#ref-johnsonTestsCertainLinear1936)[ ](#ref-johnsonTestsCertainLinear1936)[&](#ref-johnsonTestsCertainLinear1936)[ ](#ref-johnsonTestsCertainLinear1936)[Neyman,](#ref-johnsonTestsCertainLinear1936)[ ](#ref-johnsonTestsCertainLinear1936)[1936](#ref-johnsonTestsCertainLinear1936)): the slope of Negative Emotions is significant (p ≤ .05) across the observed range of Age (17 to 87). CI = confidence interval. |  |  |  |  |  |

Table 9

Simple Slopes of Negative Emotions at the Mean Age and 1 *SD* Above and
Below It

With `by = "Age=[fivenum]"`, the slopes are at the minimum, lower
quartile, median, upper quartile, and maximum:

``` r

apa_slopes(fit_mod, trend = `Negative Emotions`, 
           by = list(Age = "[fivenum]")) |>
  apa_flextable(note = TRUE)
```

| Age | Slope | SE | 95% CI | t(811) | p |
|----|----|----|----|----|----|
| Min (17) | 0.27 | 0.06 | \[0.15, 0.39\] |  4.49 | \<.001 |
| Q1 (36) | 0.40 | 0.03 | \[0.34, 0.47\] | 11.64 | \<.001 |
| Mdn (51) | 0.51 | 0.03 | \[0.46, 0.56\] | 20.31 | \<.001 |
| Q3 (63) | 0.60 | 0.03 | \[0.53, 0.66\] | 18.87 | \<.001 |
| Max (87) | 0.77 | 0.06 | \[0.64, 0.89\] | 12.25 | \<.001 |
| Note. Values are simple slopes of Negative Emotions (the expected change in Support for a one-unit increase in Negative Emotions) at each value of Age. Johnson–Neyman intervals ([Johnson](#ref-johnsonTestsCertainLinear1936)[ ](#ref-johnsonTestsCertainLinear1936)[&](#ref-johnsonTestsCertainLinear1936)[ ](#ref-johnsonTestsCertainLinear1936)[Neyman,](#ref-johnsonTestsCertainLinear1936)[ ](#ref-johnsonTestsCertainLinear1936)[1936](#ref-johnsonTestsCertainLinear1936)): the slope of Negative Emotions is significant (p ≤ .05) across the observed range of Age (17 to 87). CI = confidence interval; Q1 = first quartile (lower hinge); Q3 = third quartile (upper hinge). |  |  |  |  |  |

Table 10

Simple Slopes of Negative Emotions at the Five-Number Summary of Age

Other named shortcuts are `"[quartiles]"`, `"[minmax]"`, and `"[mad]"`
(the median and 1 median absolute deviation above and below it). The
shortcuts, and the default for a numeric variable named without values,
work the same way in `apa_means` and `apa_contrasts`. For example, in
the `Garcia` data set in the psych package ([Garcia et al.,
2010](#ref-garciaWomensReactionsIngroup2010)), 129 women read about a
female lawyer who was denied a promotion because of sex discrimination
and who either did not protest, protested on her own behalf, or
protested on behalf of women in general. How much the women liked the
lawyer depended on how pervasive they believed sexism to be. Here are
the differences between the protest conditions at the mean of perceived
sexism and 1 *SD* above and below it:

``` r

d_protest <- psych::Garcia |>
  transmute(
    Liking = liking,
    Sexism = sexism,
    Protest = factor(protest, levels = 0:2, labels = c("None", "Individual", "Collective"))
  )

fit_protest <- lm(Liking ~ Protest * Sexism, data = d_protest)

apa_contrasts(fit_protest, contrast = Protest, by = Sexism) |>
  apa_flextable(note = TRUE)
```

| Protest | Difference | SE | 95% CI | d | 95% CI for d | t(123) | p |
|----|----|----|----|----|----|----|----|
| Sexism: −1SD (4.33) |  |  |  |  |  |  |  |
|  Individual − None | −0.22 | 0.32 | \[−0.86, 0.41\] | −0.22 | \[−0.87, 0.42\] | −0.69 |  .49  |
|  Collective − None | −0.12 | 0.29 | \[−0.70, 0.46\] | −0.12 | \[−0.70, 0.46\] | −0.41 |  .68  |
|  Collective − Individual |  0.10 | 0.31 | \[−0.51, 0.71\] |  0.10 | \[−0.51, 0.72\] |  0.33 |  .74  |
| Sexism: M (5.12) |  |  |  |  |  |  |  |
|  Individual − None |  0.48 | 0.22 | \[ 0.05, 0.92\] |  0.48 | \[ 0.05, 0.92\] |  2.20 |  .03  |
|  Collective − None |  0.49 | 0.22 | \[ 0.06, 0.92\] |  0.49 | \[ 0.06, 0.92\] |  2.27 |  .03  |
|  Collective − Individual |  0.01 | 0.21 | \[−0.42, 0.43\] |  0.01 | \[−0.42, 0.43\] |  0.03 |  .98  |
| Sexism: +1SD (5.90) |  |  |  |  |  |  |  |
|  Individual − None |  1.19 | 0.31 | \[ 0.58, 1.80\] |  1.19 | \[ 0.57, 1.82\] |  3.88 | \<.001 |
|  Collective − None |  1.10 | 0.32 | \[ 0.47, 1.73\] |  1.10 | \[ 0.46, 1.75\] |  3.47 | \<.001 |
|  Collective − Individual | −0.09 | 0.30 | \[−0.68, 0.50\] | −0.09 | \[−0.68, 0.50\] | −0.30 |  .76  |
| Note. Values are differences between estimated marginal means of Liking (the first level minus the second). d = difference divided by the residual standard deviation of the model (1.00); its confidence interval accounts for the uncertainty in the residual standard deviation. p-values are not adjusted for multiple comparisons. CI = confidence interval. |  |  |  |  |  |  |  |

Table 11

Differences in Liking Between Protest Conditions at Three Levels of
Perceived Sexism

Women who saw sexism as pervasive liked the lawyer more when she
protested than when she did not, but for women who saw little sexism,
protesting made no difference.

## Logistic Regression

For models with a link function, modelbased reports estimates on the
response scale by default. For logistic regression, the estimates are
probabilities, which have no leading zero in APA style, and the
contrasts are differences in probabilities. The `anorexia` data set in
the MASS package ([Hand et al., 1994](#ref-handHandbookSmallData1994))
has the weights of 72 young women with anorexia before and after
treatment with cognitive behavioral therapy, family therapy, or no
therapy (control). Here, the probability of gaining weight depends on
the treatment, holding the weight before treatment at its mean:

``` r

d_anorexia <- MASS::anorexia |>
  transmute(
    `Weight Gain` = as.integer(Postwt > Prewt),
    Treatment = factor(Treat,
                       levels = c("Cont", "CBT", "FT"),
                       labels = c("Control", "Cognitive behavioral", "Family")),
    Baseline = Prewt
  )

fit_logistic <- glm(
  `Weight Gain` ~ Treatment + Baseline,
  data = d_anorexia,
  family = binomial
)

apa_means(fit_logistic, by = Treatment) |>
  apa_flextable(note = TRUE)
```

| Treatment | Probability | 95% CI |
|----|----|----|
| Control | .41 | \[.24, .61\] |
| Cognitive behavioral | .62 | \[.44, .78\] |
| Family | .77 | \[.52, .91\] |
| Note. Values are estimated probabilities of Weight Gain. Baseline is held at its mean (82.41). Estimates are on the response scale (back-transformed from the logit scale). CI = confidence interval. |  |  |

Table 12

Estimated Probabilities of Gaining Weight by Treatment

``` r

apa_contrasts(fit_logistic, contrast = Treatment, p_adjust = "holm") |>
  apa_flextable(note = TRUE)
```

| Treatment | Difference | SE | 95% CI | z | p |
|----|----|----|----|----|----|
| Cognitive behavioral − Control | .21 | .13 | \[−.05, .47\] | 1.57 | .23 |
| Family − Control | .36 | .14 | \[ .08, .64\] | 2.52 | .04 |
| Family − Cognitive behavioral | .15 | .14 | \[−.12, .41\] | 1.09 | .28 |
| Note. Values are differences between estimated probabilities of Weight Gain (the first level minus the second). Baseline is held at its mean (82.41). Differences are on the response scale (back-transformed from the logit scale). p-values are adjusted for multiple comparisons with Holm’s method ([Holm,](#ref-holmSimpleSequentiallyRejective1979)[ ](#ref-holmSimpleSequentiallyRejective1979)[1979](#ref-holmSimpleSequentiallyRejective1979)). CI = confidence interval. |  |  |  |  |  |

Table 13

Differences in the Probability of Gaining Weight Between Treatments

With `predict = "link"`, modelbased gives estimates on the link scale
(here, log odds) instead, and the note says so.

``` r

apa_contrasts(fit_logistic, 
              contrast = Treatment, 
              p_adjust = "holm", 
              predict = "link") |>
  apa_flextable(note = TRUE)
```

| Treatment | Difference | SE | 95% CI | z | p |
|----|----|----|----|----|----|
| Cognitive behavioral − Control | 0.85 | 0.56 | \[−0.24, 1.95\] | 1.53 | .25 |
| Family − Control | 1.56 | 0.71 | \[ 0.18, 2.95\] | 2.21 | .08 |
| Family − Cognitive behavioral | 0.71 | 0.69 | \[−0.64, 2.07\] | 1.03 | .30 |
| Note. Values are differences between estimated marginal means of Weight Gain (the first level minus the second). Baseline is held at its mean (82.41). Differences are on the logit scale. p-values are adjusted for multiple comparisons with Holm’s method ([Holm,](#ref-holmSimpleSequentiallyRejective1979)[ ](#ref-holmSimpleSequentiallyRejective1979)[1979](#ref-holmSimpleSequentiallyRejective1979)). CI = confidence interval. |  |  |  |  |  |

Table 14

Differences in the Log Odds of Gaining Weight Between Treatments

## Options

- `starred = "p"` adds significance stars, and `bolded = "p"` bolds
  significant values, as in `apa_parameters`.
- `format_variable_names = FALSE` keeps variable names exactly as they
  are.
- `effect_size = FALSE` leaves Cohen’s *d* out of contrast tables, and
  `sd` sets the standard deviation it is computed with.
- Bayesian models are not yet supported.

## References

Garcia, D. M., Schmitt, M. T., Branscombe, N. R., & Ellemers, N. (2010).
Women’s reactions to ingroup members who protest discriminatory
treatment: The importance of beliefs about inequality and response
appropriateness. *European Journal of Social Psychology*, *40*(5),
733–745. <https://doi.org/10.1002/ejsp.644>

Hand, D. J., Daly, F., McConway, K. J., Lunn, A. D., & Ostrowski, E.
(1994). *A handbook of small data sets*. Chapman & Hall.

Hayes, A. F. (2022). *Introduction to mediation, moderation, and
conditional process analysis: A regression-based approach* (3rd ed.).
Guilford Press.

Holm, S. (1979). A simple sequentially rejective multiple test
procedure. *Scandinavian Journal of Statistics*, *6*(2), 65–70.
<https://www.jstor.org/stable/4615733>

Johnson, P. O., & Neyman, J. (1936). Tests of certain linear hypotheses
and their application to some educational problems. *Statistical
Research Memoirs*, *1*, 57–93.

Moore, J. C., & Krupat, E. (1971). Relationship between source status,
authoritarianism, and conformity in a social influence setting.
*Sociometry*, *34*(1), 122–134. <https://doi.org/10.2307/2786355>
