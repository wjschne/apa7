# Meta-Analysis Tables

``` r

library(apa7)
library(metafor)
library(dplyr)
apa7_defaults(font_family = "Open Sans", line_spacing = 1.5)
```

`apa_parameters` and `apa_performance` make APA-style tables of
meta-analyses fit with
[`metafor::rma`](https://wviechtb.github.io/metafor/reference/rma.uni.html):

| Table | Function | Contents |
|----|----|----|
| Studies and pooled estimate | `apa_parameters` | Each study’s effect size, confidence interval, and weight, followed by the pooled estimate |
| Meta-regression | `apa_parameters` | Coefficients of the moderators, as for other regression models |
| Heterogeneity | `apa_performance` | *k*, *Q*, *τ*², *I*², and, for meta-regressions, the test of the moderators and *R*² |
| Subgroups | `apa_subgroups` | Each subgroup’s pooled estimate and the test of subgroup differences |
| Publication bias | `apa_publication_bias` | Egger’s regression test, the rank correlation test, and trim and fill, with adjusted estimates |

## Studies and the Pooled Estimate

The `dat.molloy2014` data set in the metadat package (installed with
metafor) has the correlations between conscientiousness and medication
adherence in 16 studies. Correlations are meta-analyzed as Fisher’s *z*
values (`measure = "ZCOR"` in `escalc`), and `slab` gives the study
labels:

``` r

d_adherence <- escalc(
  measure = "ZCOR",
  ri = ri,
  ni = ni,
  data = metadat::dat.molloy2014,
  slab = paste(authors, year)
)

fit_adherence <- rma(yi, vi, data = d_adherence)

apa_parameters(fit_adherence) |>
  apa_flextable(note = TRUE)
```

| Study | r | 95% CI | Weight (%) |
|----|----|----|----|
| Axelsson et al. 2009 |  .19 | \[ .00, .36\] |  5.7 |
| Axelsson et al. 2011 |  .16 | \[ .09, .23\] | 10.5 |
| Bruce et al. 2010 |  .34 | \[ .08, .56\] |  3.6 |
| Christensen et al. 1999 |  .32 | \[ .14, .48\] |  5.6 |
| Christensen & Smith 1995 |  .27 | \[ .04, .47\] |  4.4 |
| Cohen et al. 2004 |  .00 | \[−.24, .24\] |  4.1 |
| Dobbels et al. 2005 |  .18 | \[ .03, .32\] |  7.1 |
| Ediger et al. 2007 |  .05 | \[−.06, .16\] |  8.9 |
| Insel et al. 2006 |  .26 | \[ .00, .49\] |  3.8 |
| Jerant et al. 2011 |  .01 | \[−.06, .08\] | 10.6 |
| Moran et al. 1997 | −.09 | \[−.34, .18\] |  3.7 |
| O’Cleirigh et al. 2007 |  .37 | \[ .18, .54\] |  5.1 |
| Penedo et al. 2003 |  .00 | \[−.18, .18\] |  5.9 |
| Quine et al. 2012 |  .15 | \[ .07, .23\] | 10.0 |
| Stilley et al. 2004 |  .24 | \[ .09, .38\] |  6.8 |
| Wiebe & Christensen 1997 |  .04 | \[−.21, .28\] |  4.1 |
| Pooled estimate (random effects) |  .15 | \[ .09, .21\] |  |
| Note. Effect sizes are correlations, analyzed on the Fisher’s z scale and converted back for the table. The 16 studies were pooled with a random-effects model, with τ2 estimated by restricted maximum likelihood. Weights are percentages of the total weight. Test of the pooled estimate: z = 4.75, p \< .001; 95% PI \[−.04, .32\]. Heterogeneity: Q(15) = 38.16, p \< .001, τ2 = 0.01, I2 = 61.7%. CI = confidence interval; PI = prediction interval. |  |  |  |

Table 1

Correlations Between Conscientiousness and Medication Adherence

Each study’s correlation and confidence interval, and the pooled
estimate below the rule, are converted back from Fisher’s *z* to *r*.
The weights are each study’s percentage of the total weight in the
random-effects model. The note names the measure and the model, and it
gives the test of the pooled estimate, its 95% prediction interval (the
range in which the true effect of a new study is expected to fall), and
the test of heterogeneity.

The column of effect sizes is named by the measure given to `escalc`:
for example, *g* for standardized mean differences (`"SMD"`), *r* for
correlations (`"COR"` or `"ZCOR"`), and *OR*, *RR*, or *IRR* for odds
ratios, risk ratios, and incidence rate ratios. Ratios are analyzed on
the log scale and exponentiated for the table. With `transform = FALSE`,
estimates stay on the scale on which they were analyzed (e.g., log risk
ratios). With `weights = FALSE`, the weights are left out.

## Heterogeneity

`apa_performance` gives the heterogeneity statistics:

``` r

apa_performance(fit_adherence) |>
  apa_flextable(note = TRUE)
```

| k | Q(15) | p | τ2 | I2 (%) |
|----|----|----|----|----|
| 16 | 38.16 | \<.001 | 0.01 | 61.7 |
| Note. τ2 was estimated by restricted maximum likelihood. k = number of studies; Q = test of heterogeneity; τ2 = variance of the true effects; I2 = percentage of total variability due to heterogeneity. |  |  |  |  |

Table 2

Heterogeneity of the Correlations Between Conscientiousness and
Medication Adherence

With `metrics = "all"`, the table adds *τ*, *H*², and confidence
intervals of *τ*² and *I*².

## Ratio Measures

In the `dat.landenberger2005` data set ([Landenberger & Lipsey,
2005](#ref-landenbergerPositiveEffectsCognitivebehavioral2005)), 58
studies compared the number of juvenile and adult offenders who
reoffended after a cognitive behavioral therapy (CBT) program and after
no program. The effect sizes are odds ratios (the odds of reoffending
after CBT divided by the odds without it), which are analyzed on the log
scale and exponentiated for the table:

``` r

d_recidivism <- escalc(
  measure = "OR",
  ai = n.cbt.rec, bi = n.cbt.non, ci = n.ctrl.rec, di = n.ctrl.non,
  data = metadat::dat.landenberger2005,
  slab = study
) |>
  mutate(
    Sessions = sessions,
    Program = factor(program,
                     levels = c("demo", "practice", "research"),
                     labels = c("Demonstration", "Practice", "Research"))
  )

fit_recidivism <- rma(yi, vi, data = d_recidivism)

apa_parameters(fit_recidivism) |>
  apa_flextable(note = TRUE)
```

| Study | OR | 95% CI | Weight (%) |
|----|----|----|----|
| Guerra (1990) | 0.66 | \[0.26, 1.70\] | 1.1 |
| Leeman (1993) | 0.26 | \[0.06, 1.04\] | 0.6 |
| Kownacki (1995) | 0.33 | \[0.05, 2.43\] | 0.3 |
| Larson (1989) | 0.29 | \[0.04, 1.90\] | 0.4 |
| Finn (1998) | 0.66 | \[0.32, 1.38\] | 1.5 |
| Goldstein (1989).1 | 0.23 | \[0.07, 0.75\] | 0.8 |
| Goldstein (1989).2 | 0.14 | \[0.04, 0.47\] | 0.8 |
| Ross (1988) | 0.10 | \[0.02, 0.39\] | 0.6 |
| Pullen (1996) | 1.26 | \[0.33, 4.73\] | 0.7 |
| Shivrattan (1988) | 0.42 | \[0.09, 1.91\] | 0.5 |
| Robinson (1995a) | 0.79 | \[0.61, 1.03\] | 3.0 |
| Robinson (1991) | 0.46 | \[0.28, 0.74\] | 2.2 |
| Hughey (1996) | 0.88 | \[0.58, 1.34\] | 2.4 |
| Friendship (2003) | 0.60 | \[0.46, 0.78\] | 3.0 |
| Little (1993).1 | 0.60 | \[0.45, 0.81\] | 2.9 |
| Little (1993).2 | 0.73 | \[0.54, 0.99\] | 2.8 |
| Bush (1995) | 0.56 | \[0.26, 1.20\] | 1.4 |
| Curulla (1991) | 0.42 | \[0.08, 2.23\] | 0.4 |
| Porporino (1991) | 0.60 | \[0.18, 2.00\] | 0.8 |
| Kirkpatrick (1996) | 0.63 | \[0.42, 0.96\] | 2.4 |
| Bottcher (1985) | 0.40 | \[0.17, 0.93\] | 1.2 |
| Bonta (2000) | 1.02 | \[0.50, 2.09\] | 1.5 |
| Walters (1999) | 0.72 | \[0.41, 1.26\] | 2.0 |
| Henning (1996) | 0.40 | \[0.20, 0.80\] | 1.6 |
| Dowden (1999) | 0.22 | \[0.11, 0.43\] | 1.6 |
| Robinson (1995b) | 0.66 | \[0.34, 1.31\] | 1.6 |
| Knott (1995) | 0.83 | \[0.43, 1.61\] | 1.7 |
| Motiuk (1996) | 1.24 | \[0.59, 2.60\] | 1.5 |
| Coughlin (2003) | 0.34 | \[0.06, 1.87\] | 0.4 |
| Falshaw (2003) | 1.00 | \[0.83, 1.21\] | 3.2 |
| Johnson (1995) | 0.65 | \[0.30, 1.44\] | 1.4 |
| Burnett (1997) | 0.44 | \[0.10, 1.97\] | 0.5 |
| Grandberry (1998) | 1.20 | \[0.69, 2.08\] | 2.0 |
| Deschamps (1998) | 0.66 | \[0.41, 1.06\] | 2.2 |
| Armstrong (2003) | 0.75 | \[0.44, 1.29\] | 2.0 |
| Culver (1993) | 1.58 | \[0.61, 4.05\] | 1.1 |
| Cann (2003).1 | 0.88 | \[0.73, 1.05\] | 3.2 |
| Cann (2003).2 | 0.92 | \[0.79, 1.06\] | 3.3 |
| Van Voorhis (2003) | 0.90 | \[0.69, 1.17\] | 3.0 |
| Van Voorhis (2004) | 0.84 | \[0.58, 1.21\] | 2.6 |
| Golden (2003) | 0.71 | \[0.27, 1.83\] | 1.1 |
| Barnoski (2002a) | 0.78 | \[0.38, 1.58\] | 1.5 |
| Barnoski (2002b) | 0.74 | \[0.62, 0.90\] | 3.2 |
| Friedman (2002) | 0.69 | \[0.34, 1.41\] | 1.6 |
| Thambidurai (1980) | 0.99 | \[0.28, 3.48\] | 0.7 |
| Arbuthnot (1986) | 0.26 | \[0.05, 1.45\] | 0.4 |
| Spencer (1986) | 0.52 | \[0.09, 3.19\] | 0.4 |
| Greenwood (1993) | 0.66 | \[0.45, 0.95\] | 2.6 |
| Washington DOC (1998) | 0.95 | \[0.71, 1.28\] | 2.9 |
| T3 Associates (1999) | 0.68 | \[0.52, 0.87\] | 3.0 |
| Hanson (2000) | 0.56 | \[0.32, 1.01\] | 1.9 |
| Hall (2004) | 0.34 | \[0.18, 0.64\] | 1.7 |
| Landenberger (2004) | 1.15 | \[0.60, 2.19\] | 1.7 |
| Anderson (2002) | 0.23 | \[0.18, 0.28\] | 3.1 |
| McCracken (2003) | 0.35 | \[0.13, 0.96\] | 1.0 |
| Berry (1998) | 0.53 | \[0.27, 1.04\] | 1.6 |
| Pelissier (2001).1 | 0.73 | \[0.60, 0.90\] | 3.2 |
| Pelissier (2001).2 | 1.00 | \[0.58, 1.72\] | 2.0 |
| Pooled estimate (random effects) | 0.66 | \[0.58, 0.74\] |  |
| Note. Effect sizes are odds ratios, analyzed on the log scale and exponentiated for the table. The 58 studies were pooled with a random-effects model, with τ2 estimated by restricted maximum likelihood. Weights are percentages of the total weight. Test of the pooled estimate: z = −6.99, p \< .001; 95% PI \[0.34, 1.25\]. Heterogeneity: Q(57) = 213.69, p \< .001, τ2 = 0.10, I2 = 70.6%. CI = confidence interval; PI = prediction interval. |  |  |  |

Table 3

Effects of Cognitive Behavioral Therapy on the Odds of Reoffending

Odds ratios below 1 mean that fewer offenders reoffended after CBT.
Across the studies, CBT reduced the odds of reoffending by about a
third.

## Meta-Regression

Moderators are added with `mods`. Programs differed in how often they
met, so here the number of CBT sessions per week is a moderator:

``` r

fit_sessions <- rma(yi, vi, mods = ~ Sessions, data = d_recidivism)

apa_parameters(fit_sessions) |>
  apa_flextable(note = TRUE)
```

| Variable | B | SE | 95% CI | OR | z | p |
|----|----|----|----|----|----|----|
| Constant | −0.20 | 0.10 | \[−0.40,  0.00\] | 0.82 | −1.99 | .05 |
| Sessions | −0.07 | 0.03 | \[−0.12, −0.01\] | 0.93 | −2.46 | .01 |
| Note. Effect sizes are odds ratios, analyzed on the log scale, and coefficients are on that scale. The meta-regression of 58 studies used a mixed-effects model, with τ2 estimated by restricted maximum likelihood. CI = confidence interval; OR = odds ratio. |  |  |  |  |  |  |

Table 4

Meta-Regression of the Effects of Cognitive Behavioral Therapy on
Sessions per Week

``` r

apa_performance(fit_sessions) |>
  apa_flextable(note = TRUE)
```

| k | QE(56) | pE | τ2 | I2 (%) | QM(1) | pM | R2 (%) |
|----|----|----|----|----|----|----|----|
| 58 | 146.64 | \<.001 | 0.08 | 62.9 | 6.07 | .01 | 27.3 |
| Note. τ2 was estimated by restricted maximum likelihood. k = number of studies; QE = test of residual heterogeneity; τ2 = variance of the true effects; I2 = percentage of total variability due to heterogeneity; QM = omnibus test of the moderators; R2 = percentage of heterogeneity explained by the moderators. |  |  |  |  |  |  |  |

Table 5

Heterogeneity in the Meta-Regression on Sessions per Week

For a meta-regression, `apa_parameters` shows the intercept and
moderators, as for other regression models. Coefficients (*B*) are on
the scale on which the effect sizes were analyzed (here, log odds
ratios), and for ratio measures, the exponentiated coefficients are
added (here, *OR*): each additional session per week multiplies the odds
ratio by about 0.93, so programs that met more often reduced reoffending
more. `apa_performance` adds the omnibus test of the moderators
(*Q*_(M)) and the percentage of heterogeneity they explain (*R*²), and
*Q*_(E) is the test of the heterogeneity that remains.

## Subgroup Analysis

A subgroup analysis compares the pooled estimates of groups of studies.
The CBT programs had been set up for demonstration, for research, or as
routine practice. `apa_subgroups` pools each group of studies and
compares them:

``` r

apa_subgroups(fit_recidivism, Program) |>
  apa_flextable(note = TRUE)
```

| Program | k | OR | 95% CI | p | I2 (%) |
|----|----|----|----|----|----|
| Demonstration | 18 | 0.53 | \[0.40, 0.69\] | \<.001 | 65.4 |
| Practice | 31 | 0.75 | \[0.69, 0.83\] | \<.001 | 44.2 |
| Research | 9 | 0.41 | \[0.22, 0.76\] |  .005 | 55.2 |
| Overall | 58 | 0.66 | \[0.58, 0.74\] | \<.001 | 70.6 |
| Note. Effect sizes are odds ratios, analyzed on the log scale and exponentiated for the table. Each subgroup of Program was pooled with its own random-effects model, with τ2 estimated by restricted maximum likelihood. The overall estimate is from the meta-analysis of all effect sizes. Test of subgroup differences ([Borenstein](#ref-borensteinIntroductionMetaanalysis2009)[ ](#ref-borensteinIntroductionMetaanalysis2009)[et](#ref-borensteinIntroductionMetaanalysis2009)[ ](#ref-borensteinIntroductionMetaanalysis2009)[al.,](#ref-borensteinIntroductionMetaanalysis2009)[ ](#ref-borensteinIntroductionMetaanalysis2009)[2009](#ref-borensteinIntroductionMetaanalysis2009)): Qbetween(2) = 9.08, p = .01. k = number of studies; CI = confidence interval; I2 = percentage of total variability due to heterogeneity. |  |  |  |  |  |

Table 6

Effects of Cognitive Behavioral Therapy by Purpose of the Program

Programs set up for demonstration or research reduced reoffending more
than programs in routine practice.

By default, each subgroup has its own *τ*², and the subgroups are
compared with *Q*_(between), the heterogeneity of the subgroup
estimates. With `tau2 = "common"`, the subgroups share one *τ*², which
is estimated more precisely when subgroups have few studies: the
estimates come from a meta-regression with the subgroup as a categorical
moderator, and the subgroups are compared with its omnibus test
(*Q*_(M)). The subgroup variable can be a column of the data (quoted or
unquoted) or a vector (e.g., `d_recidivism$Sessions > 1`).

## Publication Bias

`apa_publication_bias` tests for publication bias (small-study effects)
and gives estimates adjusted for it, as computed by metafor: Egger’s
regression test, with its limit estimate (the predicted effect of a
study with a standard error of zero); Begg and Mazumdar’s rank
correlation test; and trim and fill, with the number of studies it
imputed (*k*₀) and the pooled estimate with them included:

``` r

apa_publication_bias(fit_adherence) |>
  apa_flextable(note = TRUE)
```

| Method | Statistic | Value | p | Adjusted r | 95% CI |
|----|----|----|----|----|----|
| Egger’s regression test | z | 1.02 | .31 | .08 | \[−.07, .22\] |
| Rank correlation test | τ |  .16 | .39 |  |  |
| Trim and fill | k0 | 2   |  | .13 | \[ .06, .19\] |
| Note. The unadjusted pooled estimate of the 16 studies is r = .15, 95% CI \[.09, .21\]. Egger’s regression test ([Egger](#ref-eggerBiasMetaanalysisDetected1997)[ ](#ref-eggerBiasMetaanalysisDetected1997)[et](#ref-eggerBiasMetaanalysisDetected1997)[ ](#ref-eggerBiasMetaanalysisDetected1997)[al.,](#ref-eggerBiasMetaanalysisDetected1997)[ ](#ref-eggerBiasMetaanalysisDetected1997)[1997](#ref-eggerBiasMetaanalysisDetected1997)) is the test of the standard error in a meta-regression of the effect sizes on their standard errors; its adjusted estimate is the predicted effect as the standard error approaches zero. The rank correlation test ([Begg](#ref-beggOperatingCharacteristicsRank1994)[ ](#ref-beggOperatingCharacteristicsRank1994)[&](#ref-beggOperatingCharacteristicsRank1994)[ ](#ref-beggOperatingCharacteristicsRank1994)[Mazumdar,](#ref-beggOperatingCharacteristicsRank1994)[ ](#ref-beggOperatingCharacteristicsRank1994)[1994](#ref-beggOperatingCharacteristicsRank1994)) is Kendall’s τ between the standardized effect sizes and their sampling variances. Trim and fill ([Duval](#ref-duvalTrimFillSimple2000)[ ](#ref-duvalTrimFillSimple2000)[&](#ref-duvalTrimFillSimple2000)[ ](#ref-duvalTrimFillSimple2000)[Tweedie,](#ref-duvalTrimFillSimple2000)[ ](#ref-duvalTrimFillSimple2000)[2000](#ref-duvalTrimFillSimple2000)) with the L0 estimator imputed 2 studies on the left side of the funnel plot; its adjusted estimate includes them. Estimates are correlations, converted from the scale on which they were analyzed. CI = confidence interval; k0 = number of imputed studies. |  |  |  |  |  |

Table 7

Tests of Publication Bias in Studies of Conscientiousness and Medication
Adherence

The adjusted estimates are on the same scale as the table of studies
(here, correlations), and the note gives the unadjusted pooled estimate
for comparison. Add `"fsn"` to `methods` for the fail-safe *N*, which is
widely criticized and is not shown by default. These methods are
available for `rma` models without moderators.

## Multilevel Meta-Analysis

When studies report several effect sizes, the effect sizes are not
independent, and a multilevel (three-level) model with `rma.mv` has a
random intercept for each level. In the `dat.konstantopoulos2011` data
set, 56 schools in 11 school districts compared the achievement of
students on modified and traditional school calendars (standardized mean
differences). The table of a multilevel model has a row for each effect
size, so this example shows only the pooled estimate
(`studies = FALSE`):

``` r

fit_schools <- rma.mv(
  yi, vi,
  random = ~ 1 | district / school,
  data = metadat::dat.konstantopoulos2011
)

apa_parameters(fit_schools, studies = FALSE) |>
  apa_flextable(note = TRUE)
```

| Variable | g | SE | 95% CI | z | p |
|----|----|----|----|----|----|
| Pooled estimate (random effects) | 0.18 | 0.08 | \[0.02, 0.35\] | 2.18 | .03 |
| Note. Effect sizes are Hedges’ g values (standardized mean differences). The 56 effect sizes were pooled with a multilevel random-effects model with random intercepts for district (11 levels) and school in district (56 levels), with variance components estimated by restricted maximum likelihood. CI = confidence interval. |  |  |  |  |  |

Table 8

Multilevel Meta-Analysis of the Effects of Modified School Calendars

``` r

apa_performance(fit_schools) |>
  apa_flextable(note = TRUE)
```

| k | Q(55) | p | σ2District | σ2School in District | I2 (%) |
|----|----|----|----|----|----|
| 56 | 578.86 | \<.001 | 0.07 | 0.03 | 95.2 |
| Note. Variance components were estimated by restricted maximum likelihood. I2 is the multilevel I2 ([Cheung,](#ref-cheungModelingDependentEffect2014)[ ](#ref-cheungModelingDependentEffect2014)[2014](#ref-cheungModelingDependentEffect2014)): the percentage of total variability due to heterogeneity at all levels, or at the level in the subscript. k = number of effect sizes; Q = test of heterogeneity; σ2 = variance of the true effects at the level in the subscript. |  |  |  |  |  |

Table 9

Heterogeneity at Each Level of the Multilevel Meta-Analysis

`apa_performance` gives the variance of the true effects at each level
(*σ*², with the level as a subscript) and the multilevel *I*²([Cheung,
2014](#ref-cheungModelingDependentEffect2014)), the percentage of the
total variability due to heterogeneity at all levels. With
`metrics = "all"`, the table adds *σ* and confidence intervals of *σ*²
for each level, and *I*² for each level. Meta-regressions of multilevel
models work as for `rma`. Random effects with an `~ inner | outer`
structure (e.g., correlated outcomes within studies) are not yet
supported.

## Citations in Notes

Some notes cite the articles that describe a method (e.g., trim and fill
and the multilevel *I*²). In the console and in this article, the
citations are written as text. To have them formatted and added to the
reference list of a Quarto or R Markdown document, see [Citations in
notes](https://wjschne.github.io/apa7/articles/regression.html#citations-in-notes)
in the regression article.

## Options

- `method = "EE"` in `rma` fits an equal-effects model; the table labels
  the pooled estimate accordingly, and there is no prediction interval
  or *τ*².
- `test = "knha"` in `rma` uses the Knapp–Hartung adjustment; the pooled
  estimate is tested with *t*, and the note says so.
- `studies = FALSE` shows only the pooled estimate, with its test,
  instead of the study rows.
- Multilevel models with random intercepts (`rma.mv` with
  `random = ~ 1 | study / effect`) are supported; random effects with an
  `~ inner | outer` structure are not yet supported.

## References

Begg, C. B., & Mazumdar, M. (1994). Operating characteristics of a rank
correlation test for publication bias. *Biometrics*, *50*(4), 1088–1101.
<https://doi.org/10.2307/2533446>

Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R.
(2009). *Introduction to meta-analysis*. Wiley.
<https://doi.org/10.1002/9780470743386>

Cheung, M. W.-L. (2014). Modeling dependent effect sizes with
three-level meta-analyses: A structural equation modeling approach.
*Psychological Methods*, *19*(2), 211–229.
<https://doi.org/10.1037/a0032968>

Duval, S., & Tweedie, R. (2000). Trim and fill: A simple
funnel‐plot–based method of testing and adjusting for publication bias
in meta‐analysis. *Biometrics*, *56*(2), 455–463.
<https://doi.org/10.1111/j.0006-341X.2000.00455.x>

Egger, M., Davey Smith, G., Schneider, M., & Minder, C. (1997). Bias in
meta-analysis detected by a simple, graphical test. *BMJ*, *315*(7109),
629–634. <https://doi.org/10.1136/bmj.315.7109.629>

Landenberger, N. A., & Lipsey, M. W. (2005). The positive effects of
cognitive-behavioral programs for offenders: A meta-analysis of factors
associated with effective treatment. *Journal of Experimental
Criminology*, *1*(4), 451–476.
<https://doi.org/10.1007/s11292-005-3541-7>
