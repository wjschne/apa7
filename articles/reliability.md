# Reliability and Item Analysis Tables

``` r

library(apa7)
apa7_defaults(font_family = "Open Sans", line_spacing = 1.5)
```

`apa_reliability` reports useful aspects of a reliability analysis. For
each item, it shows:

- the mean (*M*) and standard deviation (*SD*), which reveal ceiling and
  floor effects
- the corrected item–total correlation (*r*_(it)), the item’s
  correlation with the sum of the *other* items in its scale. Items with
  low values (often, below .30) discriminate poorly.
- Cronbach’s α if the item were deleted. A value above the scale’s α
  means that the item lowers reliability.

Below each scale’s items are its scale-level statistics: the mean and
*SD* of the scale scores, Cronbach’s α with a confidence interval,
McDonald’s *ω*_(total), and the mean inter-item correlation. Cronbach’s
α ([Cronbach, 1951](#ref-cronbachCoefficientAlphaInternal1951)) assumes
that all items are equally good measures of what the scale measures,
while *ω*_(total) allows them to differ, so many journals now ask for
both. The mean inter-item correlation shows whether a high α comes from
strongly related items or merely from many items.

The statistics come from the
[psych](https://personality-project.org/r/psych/) package:
[`psych::alpha`](https://rdrr.io/pkg/psych/man/alpha.html) for α and the
item statistics, and a one-factor model
([`psych::fa`](https://rdrr.io/pkg/psych/man/fa.html)) for *ω*_(total).
The confidence interval of α is Feldt’s.

## Several Scales

The `bfi` data set in the psychTools package ([Revelle,
2019](#ref-revellePsychToolsToolsAccompany2019)) has the answers of
2,800 people to 25 personality items, on a scale from 1 (*very
inaccurate*) to 6 (*very accurate*). The items are named by codes (A1 to
A5 for agreeableness, and so on), and `bfi.dictionary` has their
wording. The table is easier to read with the wording, so rename the
items with it, and keep a lookup vector from codes to wording for
choosing the items of each scale:

``` r

items <- psychTools::bfi.dictionary[1:25, ]
# Item wording without the final period, named by item code
item_text <- setNames(sub("[.]$", "", items$Item), rownames(items))

d_bfi <- psychTools::bfi[, names(item_text)]
names(d_bfi) <- item_text
```

Give `apa_reliability` a named list of scales, each a vector of item
names. Items worded in the opposite direction (e.g., “Am indifferent to
the feelings of others” on the agreeableness scale) are reverse-scored
with `reverse`. `item_range` gives the lowest and highest possible
responses, which are used to reverse the items:

``` r

bfi_scales <- list(
  Agreeableness = item_text[paste0("A", 1:5)],
  Conscientiousness = item_text[paste0("C", 1:5)]
)
bfi_reverse <- item_text[c("A1", "C4", "C5")]

apa_reliability(
  d_bfi,
  scales = bfi_scales,
  reverse = bfi_reverse,
  item_range = c(1, 6)
) |>
  apa_flextable(note = TRUE)
```

| Item | M | SD | rit | α if deleted |
|----|----|----|----|----|
| Agreeableness |  |  |  |  |
|  Am indifferent to the feelings of others (R) | 4.59 | 1.40 | .31 | .72 |
|  Inquire about others’ well-being | 4.80 | 1.18 | .56 | .62 |
|  Know how to comfort others | 4.60 | 1.30 | .59 | .60 |
|  Love children | 4.68 | 1.49 | .39 | .69 |
|  Make people feel at ease | 4.55 | 1.26 | .49 | .64 |
|  Scale mean (k = 5) | 4.64 | 0.90 |  |  |
|  α \[95% CI\] | .70 \[.69, .72\] |  |  |  |
|  ωtotal | .72 |  |  |  |
|  Mean inter-item r | .33 |  |  |  |
| Conscientiousness |  |  |  |  |
|  Am exacting in my work | 4.51 | 1.24 | .46 | .70 |
|  Continue until everything is perfect | 4.36 | 1.32 | .51 | .68 |
|  Do things according to a plan | 4.30 | 1.29 | .47 | .69 |
|  Do things in a half-way manner (R) | 4.45 | 1.37 | .56 | .66 |
|  Waste my time (R) | 3.69 | 1.63 | .48 | .69 |
|  Scale mean (k = 5) | 4.26 | 0.95 |  |  |
|  α \[95% CI\] | .73 \[.71, .75\] |  |  |  |
|  ωtotal | .73 |  |  |  |
|  Mean inter-item r | .35 |  |  |  |
| Note. Each scale uses the respondents with complete responses on its items (Agreeableness: n = 2,709; Conscientiousness: n = 2,707). Items marked (R) were reverse-scored. Scale scores are the means of the items. rit = corrected item–total correlation (the correlation of the item with the sum of the other items); α if deleted = Cronbach’s α without the item; α = Cronbach’s α; CI = 95% confidence interval ([Feldt](#ref-feldtStatisticalInferenceCoefficient1987)[ ](#ref-feldtStatisticalInferenceCoefficient1987)[et](#ref-feldtStatisticalInferenceCoefficient1987)[ ](#ref-feldtStatisticalInferenceCoefficient1987)[al.,](#ref-feldtStatisticalInferenceCoefficient1987)[ ](#ref-feldtStatisticalInferenceCoefficient1987)[1987](#ref-feldtStatisticalInferenceCoefficient1987)); ωtotal = McDonald’s ωtotal ([McDonald,](#ref-mcdonaldTestTheoryUnified1999)[ ](#ref-mcdonaldTestTheoryUnified1999)[1999](#ref-mcdonaldTestTheoryUnified1999)) from a one-factor model. |  |  |  |  |

Table 1

Item Analysis of the Agreeableness and Conscientiousness Scales

Each scale has a heading, with its items and statistics indented below
it. Reverse-scored items are marked (R). Each scale uses the respondents
who answered all of its items, and the note gives their number.

“Am indifferent to the feelings of others” has the weakest item–total
correlation of the agreeableness items (.31), and deleting it would
raise α slightly, from .70 to .72. With `flag = TRUE`, item–total
correlations below .30 (or another cutoff, with `min_item_total`) and
values of α if deleted that exceed the scale’s α are bolded:

``` r

apa_reliability(
  d_bfi,
  scales = bfi_scales,
  reverse = bfi_reverse,
  item_range = c(1, 6),
  flag = TRUE
) |>
  apa_flextable(note = TRUE)
```

| Item | M | SD | rit | α if deleted |
|----|----|----|----|----|
| Agreeableness |  |  |  |  |
|  Am indifferent to the feelings of others (R) | 4.59 | 1.40 | .31 | .72 |
|  Inquire about others’ well-being | 4.80 | 1.18 | .56 | .62 |
|  Know how to comfort others | 4.60 | 1.30 | .59 | .60 |
|  Love children | 4.68 | 1.49 | .39 | .69 |
|  Make people feel at ease | 4.55 | 1.26 | .49 | .64 |
|  Scale mean (k = 5) | 4.64 | 0.90 |  |  |
|  α \[95% CI\] | .70 \[.69, .72\] |  |  |  |
|  ωtotal | .72 |  |  |  |
|  Mean inter-item r | .33 |  |  |  |
| Conscientiousness |  |  |  |  |
|  Am exacting in my work | 4.51 | 1.24 | .46 | .70 |
|  Continue until everything is perfect | 4.36 | 1.32 | .51 | .68 |
|  Do things according to a plan | 4.30 | 1.29 | .47 | .69 |
|  Do things in a half-way manner (R) | 4.45 | 1.37 | .56 | .66 |
|  Waste my time (R) | 3.69 | 1.63 | .48 | .69 |
|  Scale mean (k = 5) | 4.26 | 0.95 |  |  |
|  α \[95% CI\] | .73 \[.71, .75\] |  |  |  |
|  ωtotal | .73 |  |  |  |
|  Mean inter-item r | .35 |  |  |  |
| Note. Each scale uses the respondents with complete responses on its items (Agreeableness: n = 2,709; Conscientiousness: n = 2,707). Items marked (R) were reverse-scored. Scale scores are the means of the items. rit = corrected item–total correlation (the correlation of the item with the sum of the other items); α if deleted = Cronbach’s α without the item; α = Cronbach’s α; CI = 95% confidence interval ([Feldt](#ref-feldtStatisticalInferenceCoefficient1987)[ ](#ref-feldtStatisticalInferenceCoefficient1987)[et](#ref-feldtStatisticalInferenceCoefficient1987)[ ](#ref-feldtStatisticalInferenceCoefficient1987)[al.,](#ref-feldtStatisticalInferenceCoefficient1987)[ ](#ref-feldtStatisticalInferenceCoefficient1987)[1987](#ref-feldtStatisticalInferenceCoefficient1987)); ωtotal = McDonald’s ωtotal ([McDonald,](#ref-mcdonaldTestTheoryUnified1999)[ ](#ref-mcdonaldTestTheoryUnified1999)[1999](#ref-mcdonaldTestTheoryUnified1999)) from a one-factor model. Bold values are item–total correlations below .30 and values of α if deleted above the scale’s α. |  |  |  |  |

Table 2

Item Analysis With Flagged Items

## One Row per Scale

When the items have been examined and the paper reports only the scales,
`level = "scales"` gives one row per scale. This table pairs naturally
with a correlation table of the scale scores (see `apa_cor`).

``` r

apa_reliability(
  d_bfi,
  scales = list(
    Agreeableness = item_text[paste0("A", 1:5)],
    Conscientiousness = item_text[paste0("C", 1:5)],
    Extraversion = item_text[paste0("E", 1:5)],
    Neuroticism = item_text[paste0("N", 1:5)],
    Openness = item_text[paste0("O", 1:5)]
  ),
  reverse = item_text[c("A1", "C4", "C5", "E1", "E2", "O2", "O5")],
  item_range = c(1, 6),
  level = "scales"
) |>
  apa_flextable(note = TRUE)
```

| Scale | k | M | SD | α | 95% CI | ωtotal | Mean r |
|----|----|----|----|----|----|----|----|
| Agreeableness | 5 | 4.64 | 0.90 | .70 | \[.69, .72\] | .72 | .33 |
| Conscientiousness | 5 | 4.26 | 0.95 | .73 | \[.71, .75\] | .73 | .35 |
| Extraversion | 5 | 4.14 | 1.06 | .76 | \[.75, .77\] | .76 | .39 |
| Neuroticism | 5 | 3.16 | 1.19 | .81 | \[.80, .82\] | .82 | .47 |
| Openness | 5 | 4.59 | 0.81 | .60 | \[.58, .63\] | .62 | .24 |
| Note. Each scale uses the respondents with complete responses on its items (Agreeableness: n = 2,709; Conscientiousness: n = 2,707; Extraversion: n = 2,713; Neuroticism: n = 2,694; Openness: n = 2,726). Reverse-keyed items were reverse-scored. Scale scores are the means of the items. α = Cronbach’s α; CI = 95% confidence interval ([Feldt](#ref-feldtStatisticalInferenceCoefficient1987)[ ](#ref-feldtStatisticalInferenceCoefficient1987)[et](#ref-feldtStatisticalInferenceCoefficient1987)[ ](#ref-feldtStatisticalInferenceCoefficient1987)[al.,](#ref-feldtStatisticalInferenceCoefficient1987)[ ](#ref-feldtStatisticalInferenceCoefficient1987)[1987](#ref-feldtStatisticalInferenceCoefficient1987)); ωtotal = McDonald’s ωtotal ([McDonald,](#ref-mcdonaldTestTheoryUnified1999)[ ](#ref-mcdonaldTestTheoryUnified1999)[1999](#ref-mcdonaldTestTheoryUnified1999)) from a one-factor model; k = number of items; Mean r = mean inter-item correlation. |  |  |  |  |  |  |  |

Table 3

Reliability of the Big Five Scales

## Test Items Scored 0 or 1

For tests whose items are scored correct (1) or incorrect (0), *M* is
the item’s difficulty (the proportion of correct answers), *r*_(it) is
the corrected point-biserial correlation, and α equals the
Kuder–Richardson formula 20 (KR-20; [Kuder & Richardson,
1937](#ref-kuderTheoryEstimationTest1937)). `apa_reliability` detects
such items and says so in the table and the note. Here, 300 simulated
students answered 6 items of increasing difficulty:

``` r

set.seed(1)
ability <- rnorm(300)
difficulty <- seq(-1, 1, length.out = 6)
test_items <- sapply(difficulty, \(b) rbinom(300, 1, plogis(ability - b)))
colnames(test_items) <- paste("Item", 1:6)

apa_reliability(as.data.frame(test_items)) |>
  apa_flextable(note = TRUE)
```

| Item | M | SD | rit | α if deleted |
|----|----|----|----|----|
| Item 1 | 0.71 | 0.45 | .14 | .50 |
| Item 2 | 0.63 | 0.48 | .27 | .43 |
| Item 3 | 0.58 | 0.49 | .29 | .42 |
| Item 4 | 0.45 | 0.50 | .22 | .46 |
| Item 5 | 0.34 | 0.48 | .28 | .42 |
| Item 6 | 0.30 | 0.46 | .28 | .43 |
| Scale mean (k = 6) | 0.50 | 0.25 |  |  |
| α (KR-20) \[95% CI\] | .49 \[.39, .57\] |  |  |  |
| ωtotal | .49 |  |  |  |
| Mean inter-item r | .14 |  |  |  |
| Note. N = 300 respondents with complete responses. Scale scores are the means of the items. Items are scored 0 or 1: for these, M is item difficulty (the proportion of 1s), rit is the corrected point-biserial correlation, and α equals KR-20. rit = corrected item–total correlation (the correlation of the item with the sum of the other items); α if deleted = Cronbach’s α without the item; α = Cronbach’s α; CI = 95% confidence interval ([Feldt](#ref-feldtStatisticalInferenceCoefficient1987)[ ](#ref-feldtStatisticalInferenceCoefficient1987)[et](#ref-feldtStatisticalInferenceCoefficient1987)[ ](#ref-feldtStatisticalInferenceCoefficient1987)[al.,](#ref-feldtStatisticalInferenceCoefficient1987)[ ](#ref-feldtStatisticalInferenceCoefficient1987)[1987](#ref-feldtStatisticalInferenceCoefficient1987)); ωtotal = McDonald’s ωtotal ([McDonald,](#ref-mcdonaldTestTheoryUnified1999)[ ](#ref-mcdonaldTestTheoryUnified1999)[1999](#ref-mcdonaldTestTheoryUnified1999)) from a one-factor model. |  |  |  |  |

Table 4

Item Analysis of Six Simulated Binary Test Items

With a single scale, there is no scale heading. Six items of modest
discrimination give a low KR-20; longer tests are usually needed for
dependable scores.

## Options

- `omega = FALSE` leaves out *ω*_(total).
- `score = "sum"` reports the mean and *SD* of the scale sums instead of
  the scale means.
- `ci` sets the confidence level of the interval of α (e.g.,
  `ci = .90`).
- `format_variable_names = FALSE` keeps item and scale names exactly as
  they are.

## References

Cronbach, L. J. (1951). Coefficient alpha and the internal structure of
tests. *Psychometrika*, *16*(3), 297–334.
<https://doi.org/10.1007/bf02310555>

Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987). Statistical
inference for coefficient alpha. *Applied Psychological Measurement*,
*11*(1), 93–103. <https://doi.org/10.1177/014662168701100107>

Kuder, G. F., & Richardson, M. W. (1937). The theory of the estimation
of test reliability. *Psychometrika*, *2*(3), 151–160.
<https://doi.org/10.1007/BF02288391>

McDonald, R. P. (1999). *Test theory: A unified treatment*. Lawrence
Erlbaum. <https://doi.org/10.4324/9781410601087>

Revelle, W. (2019). *psychTools: Tools to accompany the psych package
for psychological research* (Version 2.6.4) \[Computer software\].
Comprehensive R Archive Network.
<https://doi.org/10.32614/CRAN.package.psychTools>
