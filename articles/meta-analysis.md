# Meta-Analysis Tables

``` r

library(apa7)
library(metafor)
library(dplyr)
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

With `metrics = "all"`, the table adds *τ*, *H*², and confidence
intervals of *τ*² and *I*².

## Ratio Measures

In the `dat.bcg` data set, 13 trials compared the risk of tuberculosis
in people who were and were not vaccinated with BCG. The effect sizes
are risk ratios, which are analyzed on the log scale and exponentiated
for the table:

``` r

d_bcg <- escalc(
  measure = "RR",
  ai = tpos, bi = tneg, ci = cpos, di = cneg,
  data = metadat::dat.bcg,
  slab = paste(author, year)
) |> 
  rename(Latitude = ablat,
         Allocation = alloc) |> 
  mutate(Allocation = factor(stringr::str_to_title(Allocation)))


fit_bcg <- rma(yi, vi, data = d_bcg)

apa_parameters(fit_bcg) |>
  apa_flextable(note = TRUE)
```

| Study | RR | 95% CI | Weight (%) |
|----|----|----|----|
| Aronson 1948 | 0.41 | \[0.13, 1.26\] |  5.1 |
| Ferguson & Simes 1949 | 0.20 | \[0.09, 0.49\] |  6.4 |
| Rosenthal et al 1960 | 0.26 | \[0.07, 0.92\] |  4.4 |
| Hart & Sutherland 1977 | 0.24 | \[0.18, 0.31\] |  9.7 |
| Frimodt-Moller et al 1973 | 0.80 | \[0.52, 1.25\] |  8.9 |
| Stein & Aronson 1953 | 0.46 | \[0.39, 0.54\] | 10.1 |
| Vandiviere et al 1973 | 0.20 | \[0.08, 0.50\] |  6.0 |
| TPT Madras 1980 | 1.01 | \[0.89, 1.14\] | 10.2 |
| Coetzee & Berjak 1968 | 0.63 | \[0.39, 1.00\] |  8.7 |
| Rosenthal et al 1961 | 0.25 | \[0.15, 0.43\] |  8.4 |
| Comstock et al 1974 | 0.71 | \[0.57, 0.89\] |  9.9 |
| Comstock & Webster 1969 | 1.56 | \[0.37, 6.53\] |  3.8 |
| Comstock et al 1976 | 0.98 | \[0.58, 1.66\] |  8.4 |
| Pooled estimate (random effects) | 0.49 | \[0.34, 0.70\] |  |
| Note. Effect sizes are risk ratios, analyzed on the log scale and exponentiated for the table. The 13 studies were pooled with a random-effects model, with τ2 estimated by restricted maximum likelihood. Weights are percentages of the total weight. Test of the pooled estimate: z = −3.97, p \< .001; 95% PI \[0.15, 1.55\]. Heterogeneity: Q(12) = 152.23, p \< .001, τ2 = 0.31, I2 = 92.2%. CI = confidence interval; PI = prediction interval. |  |  |  |

## Meta-Regression

Moderators are added with `mods`. The vaccine appeared to work better in
trials farther from the equator, so here the absolute latitude of each
trial is a moderator:

``` r

fit_latitude <- rma(yi, vi, mods = ~ Latitude, data = d_bcg)

apa_parameters(fit_latitude) |>
  apa_flextable(note = TRUE)
```

| Variable | B | SE | 95% CI | RR | z | p |
|----|----|----|----|----|----|----|
| Constant |  0.25 | 0.25 | \[−0.24,  0.74\] | 1.29 |  1.01 |  .31  |
| Latitude | −0.03 | 0.01 | \[−0.04, −0.01\] | 0.97 | −4.04 | \<.001 |
| Note. Effect sizes are risk ratios, analyzed on the log scale, and coefficients are on that scale. The meta-regression of 13 studies used a mixed-effects model, with τ2 estimated by restricted maximum likelihood. CI = confidence interval; RR = risk ratio. |  |  |  |  |  |  |

``` r

apa_performance(fit_latitude) |>
  apa_flextable(note = TRUE)
```

| k | QE(11) | pE | τ2 | I2 (%) | QM(1) | pM | R2 (%) |
|----|----|----|----|----|----|----|----|
| 13 | 30.73 | .001 | 0.08 | 68.4 | 16.36 | \<.001 | 75.6 |
| Note. τ2 was estimated by restricted maximum likelihood. k = number of studies; QE = test of residual heterogeneity; τ2 = variance of the true effects; I2 = percentage of total variability due to heterogeneity; QM = omnibus test of the moderators; R2 = percentage of heterogeneity explained by the moderators. |  |  |  |  |  |  |  |

For a meta-regression, `apa_parameters` shows the intercept and
moderators, as for other regression models. Coefficients (*B*) are on
the scale on which the effect sizes were analyzed (here, log risk
ratios), and for ratio measures, the exponentiated coefficients are
added (here, *RR*): each degree of latitude multiplies the risk ratio by
about 0.97. `apa_performance` adds the omnibus test of the moderators
(*Q*_(M)) and the percentage of heterogeneity they explain (*R*²), and
*Q*_(E) is the test of the heterogeneity that remains.

## Subgroup Analysis

A subgroup analysis compares the pooled estimates of groups of studies.
In the BCG trials, participants were assigned to the vaccine at random,
alternately, or systematically. `apa_subgroups` pools each group of
trials and compares them:

``` r

apa_subgroups(fit_bcg, Allocation) |>
  apa_flextable(note = TRUE)
```

| Allocation | k | RR | 95% CI | p | I2 (%) |
|----|----|----|----|----|----|
| Alternate | 2 | 0.58 | \[0.34, 1.01\] |  .06  | 82.0 |
| Random | 7 | 0.38 | \[0.22, 0.65\] | \<.001 | 89.9 |
| Systematic | 4 | 0.65 | \[0.32, 1.32\] |  .24  | 86.4 |
| Overall | 13 | 0.49 | \[0.34, 0.70\] | \<.001 | 92.2 |
| Note. Effect sizes are risk ratios, analyzed on the log scale and exponentiated for the table. Each subgroup of Allocation was pooled with its own random-effects model, with τ2 estimated by restricted maximum likelihood. The overall estimate is from the meta-analysis of all effect sizes. Test of subgroup differences (Borenstein et al., 2009): Qbetween(2) = 1.86, p = .39. k = number of studies; CI = confidence interval; I2 = percentage of total variability due to heterogeneity. |  |  |  |  |  |

By default, each subgroup has its own *τ*², and the subgroups are
compared with *Q*_(between), the heterogeneity of the subgroup
estimates. With `tau2 = "common"`, the subgroups share one *τ*², which
is estimated more precisely when subgroups have few studies: the
estimates come from a meta-regression with the subgroup as a categorical
moderator, and the subgroups are compared with its omnibus test
(*Q*_(M)). The subgroup variable can be a column of the data (quoted or
unquoted) or a vector (e.g., `d_bcg$Latititude > 30`).

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
| Note. The unadjusted pooled estimate of the 16 studies is r = .15, 95% CI \[.09, .21\]. Egger’s regression test (Egger et al., 1997) is the test of the standard error in a meta-regression of the effect sizes on their standard errors; its adjusted estimate is the predicted effect as the standard error approaches zero. The rank correlation test (Begg & Mazumdar, 1994) is Kendall’s τ between the standardized effect sizes and their sampling variances. Trim and fill (Duval & Tweedie, 2000) with the L0 estimator imputed 2 studies on the left side of the funnel plot; its adjusted estimate includes them. Estimates are correlations, converted from the scale on which they were analyzed. CI = confidence interval; k0 = number of imputed studies. |  |  |  |  |  |

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

``` r

apa_performance(fit_schools) |>
  apa_flextable(note = TRUE)
```

| k | Q(55) | p | σ2District | σ2School in District | I2 (%) |
|----|----|----|----|----|----|
| 56 | 578.86 | \<.001 | 0.07 | 0.03 | 95.2 |
| Note. Variance components were estimated by restricted maximum likelihood. I2 is the multilevel I2 (Cheung, 2014): the percentage of total variability due to heterogeneity at all levels, or at the level in the subscript. k = number of effect sizes; Q = test of heterogeneity; σ2 = variance of the true effects at the level in the subscript. |  |  |  |  |  |

`apa_performance` gives the variance of the true effects at each level
(*σ*², with the level as a subscript) and the multilevel *I*² (Cheung,
2014), the percentage of the total variability due to heterogeneity at
all levels. With `metrics = "all"`, the table adds *σ* and confidence
intervals of *σ*² for each level, and *I*² for each level.
Meta-regressions of multilevel models work as for `rma`. Random effects
with an `~ inner | outer` structure (e.g., correlated outcomes within
studies) are not yet supported.

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
