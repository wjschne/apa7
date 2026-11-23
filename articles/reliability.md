# Reliability and Item Analysis Tables

``` r

library(apa7)
library(psych)
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
α assumes that all items are equally good measures of what the scale
measures, while *ω*_(total) allows them to differ, so many journals now
ask for both. The mean inter-item correlation shows whether a high α
comes from strongly related items or merely from many items.

The statistics come from the
[psych](https://personality-project.org/r/psych/) package:
[`psych::alpha`](https://rdrr.io/pkg/psych/man/alpha.html) for α and the
item statistics, and a one-factor model
([`psych::fa`](https://rdrr.io/pkg/psych/man/fa.html)) for *ω*_(total).
The confidence interval of α is Feldt’s.

## Several Scales

The `bfi` data set in the psych package has 25 personality items from
2,800 people, answered on a 6-point scale. Give `apa_reliability` a
named list of scales, each a vector of item names. Items worded in the
opposite direction (e.g., A1, “Am indifferent to the feelings of
others”) are reverse-scored with `reverse`. `item_range` gives the
lowest and highest possible responses, which are used to reverse the
items:

``` r

bfi_scales <- list(
  Agreeableness = paste0("A", 1:5),
  Conscientiousness = paste0("C", 1:5)
)
bfi_reverse <- c("A1", "C4", "C5")

apa_reliability(
  bfi,
  scales = bfi_scales,
  reverse = bfi_reverse,
  item_range = c(1, 6)
) |>
  apa_flextable(note = TRUE)
```

| Item | M | SD | rit | α if deleted |
|----|----|----|----|----|
| Agreeableness |  |  |  |  |
|  A1 (R) | 4.59 | 1.40 | .31 | .72 |
|  A2 | 4.80 | 1.18 | .56 | .62 |
|  A3 | 4.60 | 1.30 | .59 | .60 |
|  A4 | 4.68 | 1.49 | .39 | .69 |
|  A5 | 4.55 | 1.26 | .49 | .64 |
|  Scale mean (k = 5) | 4.64 | 0.90 |  |  |
|  α \[95% CI\] | .70 \[.69, .72\] |  |  |  |
|  ωtotal | .72 |  |  |  |
|  Mean inter-item r | .33 |  |  |  |
| Conscientiousness |  |  |  |  |
|  C1 | 4.51 | 1.24 | .46 | .70 |
|  C2 | 4.36 | 1.32 | .51 | .68 |
|  C3 | 4.30 | 1.29 | .47 | .69 |
|  C4 (R) | 4.45 | 1.37 | .56 | .66 |
|  C5 (R) | 3.69 | 1.63 | .48 | .69 |
|  Scale mean (k = 5) | 4.26 | 0.95 |  |  |
|  α \[95% CI\] | .73 \[.71, .75\] |  |  |  |
|  ωtotal | .73 |  |  |  |
|  Mean inter-item r | .35 |  |  |  |
| Note. Each scale uses the respondents with complete responses on its items (Agreeableness: n = 2,709; Conscientiousness: n = 2,707). Items marked (R) were reverse-scored. Scale scores are the means of the items. rit = corrected item–total correlation (the correlation of the item with the sum of the other items); α if deleted = Cronbach’s α without the item; α = Cronbach’s α; CI = 95% confidence interval (Feldt et al., 1987); ωtotal = McDonald’s ωtotal (McDonald, 1999) from a one-factor model. |  |  |  |  |

Each scale has a heading, with its items and statistics indented below
it. Reverse-scored items are marked (R). Each scale uses the respondents
who answered all of its items, and the note gives their number.

A1 has the weakest item–total correlation of the agreeableness items,
and deleting it would raise α slightly, from .70 to .72. With
`flag = TRUE`, item–total correlations below .30 (or another cutoff,
with `min_item_total`) and values of α if deleted that exceed the
scale’s α are bolded:

``` r

apa_reliability(
  bfi,
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
|  A1 (R) | 4.59 | 1.40 | .31 | .72 |
|  A2 | 4.80 | 1.18 | .56 | .62 |
|  A3 | 4.60 | 1.30 | .59 | .60 |
|  A4 | 4.68 | 1.49 | .39 | .69 |
|  A5 | 4.55 | 1.26 | .49 | .64 |
|  Scale mean (k = 5) | 4.64 | 0.90 |  |  |
|  α \[95% CI\] | .70 \[.69, .72\] |  |  |  |
|  ωtotal | .72 |  |  |  |
|  Mean inter-item r | .33 |  |  |  |
| Conscientiousness |  |  |  |  |
|  C1 | 4.51 | 1.24 | .46 | .70 |
|  C2 | 4.36 | 1.32 | .51 | .68 |
|  C3 | 4.30 | 1.29 | .47 | .69 |
|  C4 (R) | 4.45 | 1.37 | .56 | .66 |
|  C5 (R) | 3.69 | 1.63 | .48 | .69 |
|  Scale mean (k = 5) | 4.26 | 0.95 |  |  |
|  α \[95% CI\] | .73 \[.71, .75\] |  |  |  |
|  ωtotal | .73 |  |  |  |
|  Mean inter-item r | .35 |  |  |  |
| Note. Each scale uses the respondents with complete responses on its items (Agreeableness: n = 2,709; Conscientiousness: n = 2,707). Items marked (R) were reverse-scored. Scale scores are the means of the items. rit = corrected item–total correlation (the correlation of the item with the sum of the other items); α if deleted = Cronbach’s α without the item; α = Cronbach’s α; CI = 95% confidence interval (Feldt et al., 1987); ωtotal = McDonald’s ωtotal (McDonald, 1999) from a one-factor model. Bold values are item–total correlations below .30 and values of α if deleted above the scale’s α. |  |  |  |  |

## One Row per Scale

When the items have been examined and the paper reports only the scales,
`level = "scales"` gives one row per scale. This table pairs naturally
with a correlation table of the scale scores (see `apa_cor`).

``` r

apa_reliability(
  bfi,
  scales = list(
    Agreeableness = paste0("A", 1:5),
    Conscientiousness = paste0("C", 1:5),
    Extraversion = paste0("E", 1:5),
    Neuroticism = paste0("N", 1:5),
    Openness = paste0("O", 1:5)
  ),
  reverse = c("A1", "C4", "C5", "E1", "E2", "O2", "O5"),
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
| Note. Each scale uses the respondents with complete responses on its items (Agreeableness: n = 2,709; Conscientiousness: n = 2,707; Extraversion: n = 2,713; Neuroticism: n = 2,694; Openness: n = 2,726). Reverse-keyed items were reverse-scored. Scale scores are the means of the items. α = Cronbach’s α; CI = 95% confidence interval (Feldt et al., 1987); ωtotal = McDonald’s ωtotal (McDonald, 1999) from a one-factor model; k = number of items; Mean r = mean inter-item correlation. |  |  |  |  |  |  |  |

## Test Items Scored 0 or 1

For tests whose items are scored correct (1) or incorrect (0), *M* is
the item’s difficulty (the proportion of correct answers), *r*_(it) is
the corrected point-biserial correlation, and α equals the
Kuder–Richardson formula 20 (KR-20). `apa_reliability` detects such
items and says so in the table and the note. Here, 300 simulated
students answered 6 items of increasing difficulty:

``` r

set.seed(1)
ability <- rnorm(300)
difficulty <- seq(-1, 1, length.out = 6)
test_items <- sapply(difficulty, \(b) rbinom(300, 1, plogis(ability - b)))
colnames(test_items) <- paste0("Item", 1:6)

apa_reliability(as.data.frame(test_items)) |>
  apa_flextable(note = TRUE)
```

| Item | M | SD | rit | α if deleted |
|----|----|----|----|----|
| Item1 | 0.71 | 0.45 | .14 | .50 |
| Item2 | 0.63 | 0.48 | .27 | .43 |
| Item3 | 0.58 | 0.49 | .29 | .42 |
| Item4 | 0.45 | 0.50 | .22 | .46 |
| Item5 | 0.34 | 0.48 | .28 | .42 |
| Item6 | 0.30 | 0.46 | .28 | .43 |
| Scale mean (k = 6) | 0.50 | 0.25 |  |  |
| α (KR-20) \[95% CI\] | .49 \[.39, .57\] |  |  |  |
| ωtotal | .49 |  |  |  |
| Mean inter-item r | .14 |  |  |  |
| Note. N = 300 respondents with complete responses. Scale scores are the means of the items. Items are scored 0 or 1: for these, M is item difficulty (the proportion of 1s), rit is the corrected point-biserial correlation, and α equals KR-20. rit = corrected item–total correlation (the correlation of the item with the sum of the other items); α if deleted = Cronbach’s α without the item; α = Cronbach’s α; CI = 95% confidence interval (Feldt et al., 1987); ωtotal = McDonald’s ωtotal (McDonald, 1999) from a one-factor model. |  |  |  |  |

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
