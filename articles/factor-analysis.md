# Factor Analysis Tables

``` r

library(dplyr)
library(tibble)
library(stringr)
library(apa7)
library(psych)
```

Two functions make APA-style tables from exploratory factor analyses and
principal components analyses run with the
[psych](https://personality-project.org/r/psych/) package:

- `apa_loadings` makes a table of factor loadings, with optional columns
  for communalities, uniquenesses, and complexity, and optional rows for
  variance explained.
- `apa_factor_cor` makes a table of the correlations among factors after
  an oblique rotation.

Both accept models from
[`psych::fa`](https://rdrr.io/pkg/psych/man/fa.html) and
[`psych::principal`](https://rdrr.io/pkg/psych/man/principal.html) and
from the easystats functions
[`parameters::factor_analysis`](https://easystats.github.io/parameters/reference/principal_components.html)
and
[`parameters::principal_components`](https://easystats.github.io/parameters/reference/principal_components.html).

The workflow is the same as for the other apa7 tables:

1.  Fit a model with
    [`psych::fa`](https://rdrr.io/pkg/psych/man/fa.html) or
    [`parameters::factor_analysis`](https://easystats.github.io/parameters/reference/principal_components.html)
    (factor analysis), or with
    [`psych::principal`](https://rdrr.io/pkg/psych/man/principal.html)
    or
    [`parameters::principal_components`](https://easystats.github.io/parameters/reference/principal_components.html)
    (principal components).
2.  `apa_loadings` or `apa_factor_cor` converts the fit object into a
    tibble.
3.  In Quarto and R Markdown documents, the tibble is displayed as an
    APA-style table automatically. Pass it to `apa_flextable` to
    customize the table or save it.

## Data

The `bfi` data set in the psych package has 2,800 people’s ratings of 25
personality items, five items for each of the Big Five personality
traits. The `bfi.dictionary` data set has the text of each item, which
makes better row labels than item codes such as `A1` and `N3`. Here the
item codes are replaced with the item text (minus the final period).

``` r

item_text <- bfi.dictionary |>
  rownames_to_column("code") |>
  filter(code %in% colnames(bfi)[1:25]) |>
  mutate(Item = str_remove(Item, "[.]$")) |>
  select(Item, code) |>
  deframe()

d_bfi <- bfi |>
  select(A1:O5) |>
  rename(all_of(item_text))
```

## Factor Loadings

### A basic table

A five-factor solution with principal axis factoring (`fm = "pa"`) and
an oblimin rotation (the default in
[`psych::fa`](https://rdrr.io/pkg/psych/man/fa.html)) finds the Big
Five:

``` r

fit_bfi <- fa(d_bfi, nfactors = 5, fm = "pa")
```

By default, `apa_loadings`

- sorts the items so that they are grouped by the factor on which they
  load most strongly,
- leaves loadings smaller than .20 in absolute value blank, and
- puts the variable names in title case.

The bfi items are short sentences, so `format_variable_names = FALSE`
keeps them as they are.

``` r

apa_loadings(fit_bfi, format_variable_names = FALSE)
```

| Variable                                 | PA2  | PA3  | PA4  | PA5  | PA1  |
|------------------------------------------|------|------|------|------|------|
| Get angry easily                         |  .81 |      |      |      |      |
| Get irritated easily                     |  .78 |      |      |      |      |
| Have frequent mood swings                |  .71 |      |      |      |      |
| Panic easily                             |  .49 | −.20 |      |  .21 |      |
| Often feel blue                          |  .47 | −.39 |      |      |      |
| Find it difficult to approach others     |      | −.68 |      |      |      |
| Make friends easily                      |      |  .59 |      |  .29 |      |
| Don’t talk a lot                         |      | −.56 |      |      |      |
| Take charge                              |      |  .42 |  .27 |      |  .21 |
| Know how to captivate people             |      |  .42 |      |  .25 |  .28 |
| Continue until everything is perfect     |      |      |  .67 |      |      |
| Do things in a half-way manner           |      |      | −.61 |      |      |
| Do things according to a plan            |      |      |  .57 |      |      |
| Waste my time                            |      |      | −.55 |      |      |
| Am exacting in my work                   |      |      |  .55 |      |      |
| Know how to comfort others               |      |      |      |  .66 |      |
| Inquire about others’ well-being         |      |      |      |  .64 |      |
| Make people feel at ease                 |      |  .23 |      |  .53 |      |
| Love children                            |      |      |      |  .43 |      |
| Am indifferent to the feelings of others |  .21 |      |      | −.41 |      |
| Carry the conversation to a higher level |      |      |      |      |  .61 |
| Will not probe deeply into a subject     |      |      |      |      | −.54 |
| Am full of ideas                         |      |      |      |      |  .51 |
| Avoid difficult reading material         |      |      |      |      | −.46 |
| Spend time reflecting on things          |      | −.32 |      |      |  .37 |

Table 1: Factor Loadings of Big Five Personality Items

### Naming the factors

psych names factors by the fitting method and a number (e.g., PA1 for
the first principal axis factor). The `factor_names` argument gives them
meaningful names. Give the names in the order the factors appear in the
table, which is the order psych gives them (by the variance they
explain):

``` r

big5 <- c("Neuroticism",
          "Extraversion",
          "Conscientiousness",
          "Agreeableness",
          "Openness")

apa_loadings(fit_bfi,
             factor_names = big5,
             format_variable_names = FALSE)
```

| Variable | Neuroticism | Extraversion | Conscientiousness | Agreeableness | Openness |
|----|----|----|----|----|----|
| Get angry easily |  .81 |  |  |  |  |
| Get irritated easily |  .78 |  |  |  |  |
| Have frequent mood swings |  .71 |  |  |  |  |
| Panic easily |  .49 | −.20 |  |  .21 |  |
| Often feel blue |  .47 | −.39 |  |  |  |
| Find it difficult to approach others |  | −.68 |  |  |  |
| Make friends easily |  |  .59 |  |  .29 |  |
| Don’t talk a lot |  | −.56 |  |  |  |
| Take charge |  |  .42 |  .27 |  |  .21 |
| Know how to captivate people |  |  .42 |  |  .25 |  .28 |
| Continue until everything is perfect |  |  |  .67 |  |  |
| Do things in a half-way manner |  |  | −.61 |  |  |
| Do things according to a plan |  |  |  .57 |  |  |
| Waste my time |  |  | −.55 |  |  |
| Am exacting in my work |  |  |  .55 |  |  |
| Know how to comfort others |  |  |  |  .66 |  |
| Inquire about others’ well-being |  |  |  |  .64 |  |
| Make people feel at ease |  |  .23 |  |  .53 |  |
| Love children |  |  |  |  .43 |  |
| Am indifferent to the feelings of others |  .21 |  |  | −.41 |  |
| Carry the conversation to a higher level |  |  |  |  |  .61 |
| Will not probe deeply into a subject |  |  |  |  | −.54 |
| Am full of ideas |  |  |  |  |  .51 |
| Avoid difficult reading material |  |  |  |  | −.46 |
| Spend time reflecting on things |  | −.32 |  |  |  .37 |

Table 2: Factor Loadings of Big Five Personality Items

A named vector such as `c(PA1 = "Neuroticism", PA2 = "Extraversion")`
also works, and it can rename just some of the factors. Be careful with
it, though. After an oblimin rotation, the labels psych gives the
factors can change from one run to the next even when the solution is
the same, so a label-based mapping that was right yesterday can be wrong
today. The order of the factors does not change, so the unnamed form is
safer. Either way, read the items in each column to check the names.

### Which loadings to show

The `min_loading` argument sets the smallest absolute loading that is
displayed. Many authors show only loadings of .30 or higher. To show
every loading, set `min_loading = 0`.

``` r

apa_loadings(fit_bfi,
             factor_names = big5,
             format_variable_names = FALSE,
             min_loading = .30)
```

| Variable | Neuroticism | Extraversion | Conscientiousness | Agreeableness | Openness |
|----|----|----|----|----|----|
| Get angry easily |  .81 |  |  |  |  |
| Get irritated easily |  .78 |  |  |  |  |
| Have frequent mood swings |  .71 |  |  |  |  |
| Panic easily |  .49 |  |  |  |  |
| Often feel blue |  .47 | −.39 |  |  |  |
| Find it difficult to approach others |  | −.68 |  |  |  |
| Make friends easily |  |  .59 |  |  |  |
| Don’t talk a lot |  | −.56 |  |  |  |
| Take charge |  |  .42 |  |  |  |
| Know how to captivate people |  |  .42 |  |  |  |
| Continue until everything is perfect |  |  |  .67 |  |  |
| Do things in a half-way manner |  |  | −.61 |  |  |
| Do things according to a plan |  |  |  .57 |  |  |
| Waste my time |  |  | −.55 |  |  |
| Am exacting in my work |  |  |  .55 |  |  |
| Know how to comfort others |  |  |  |  .66 |  |
| Inquire about others’ well-being |  |  |  |  .64 |  |
| Make people feel at ease |  |  |  |  .53 |  |
| Love children |  |  |  |  .43 |  |
| Am indifferent to the feelings of others |  |  |  | −.41 |  |
| Carry the conversation to a higher level |  |  |  |  |  .61 |
| Will not probe deeply into a subject |  |  |  |  | −.54 |
| Am full of ideas |  |  |  |  |  .51 |
| Avoid difficult reading material |  |  |  |  | −.46 |
| Spend time reflecting on things |  | −.32 |  |  |  .37 |

Table 3: Factor Loadings of .30 or Higher

To keep the items in their original order, set `sort_loading = FALSE`.
The `digits` argument sets the number of decimal places.

### Highlighting primary loadings

When some items load on more than one factor, readers need to see which
factor each item belongs to. With `bold_max = TRUE`, the largest loading
in each row (in absolute value) is bolded. Here all loadings of .20 or
higher are shown, so the cross-loadings are visible next to the bolded
primary loadings:

``` r

apa_loadings(fit_bfi,
             factor_names = big5,
             format_variable_names = FALSE,
             bold_max = TRUE)
```

| Variable | Neuroticism | Extraversion | Conscientiousness | Agreeableness | Openness |
|----|----|----|----|----|----|
| Get angry easily |  .81 |  |  |  |  |
| Get irritated easily |  .78 |  |  |  |  |
| Have frequent mood swings |  .71 |  |  |  |  |
| Panic easily |  .49 | −.20 |  |  .21 |  |
| Often feel blue |  .47 | −.39 |  |  |  |
| Find it difficult to approach others |  | −.68 |  |  |  |
| Make friends easily |  |  .59 |  |  .29 |  |
| Don’t talk a lot |  | −.56 |  |  |  |
| Take charge |  |  .42 |  .27 |  |  .21 |
| Know how to captivate people |  |  .42 |  |  .25 |  .28 |
| Continue until everything is perfect |  |  |  .67 |  |  |
| Do things in a half-way manner |  |  | −.61 |  |  |
| Do things according to a plan |  |  |  .57 |  |  |
| Waste my time |  |  | −.55 |  |  |
| Am exacting in my work |  |  |  .55 |  |  |
| Know how to comfort others |  |  |  |  .66 |  |
| Inquire about others’ well-being |  |  |  |  .64 |  |
| Make people feel at ease |  |  .23 |  |  .53 |  |
| Love children |  |  |  |  .43 |  |
| Am indifferent to the feelings of others |  .21 |  |  | −.41 |  |
| Carry the conversation to a higher level |  |  |  |  |  .61 |
| Will not probe deeply into a subject |  |  |  |  | −.54 |
| Am full of ideas |  |  |  |  |  .51 |
| Avoid difficult reading material |  |  |  |  | −.46 |
| Spend time reflecting on things |  | −.32 |  |  |  .37 |

Table 4: Factor Loadings With Primary Loadings in Bold

A loading that is hidden by `min_loading` is never bolded, so an item
with no loading above the cutoff has no bold value.

### Communalities, uniquenesses, and complexity

Three optional columns describe each item:

- `communality = TRUE` adds *h*², the proportion of the item’s variance
  explained by the factors.
- `uniqueness = TRUE` adds *u*² = 1 − *h*².
- `complexity = TRUE` adds Hofmann’s (1978) complexity index, which is 1
  when an item loads on a single factor and increases as its loadings
  are spread over more factors.

``` r

apa_loadings(fit_bfi,
             factor_names = big5,
             format_variable_names = FALSE,
             min_loading = .30,
             communality = TRUE,
             complexity = TRUE)
```

| Variable | Neuroticism | Extraversion | Conscientiousness | Agreeableness | Openness | h2 | Complexity |
|----|----|----|----|----|----|----|----|
| Get angry easily |  .81 |  |  |  |  | .65 | 1.08 |
| Get irritated easily |  .78 |  |  |  |  | .60 | 1.04 |
| Have frequent mood swings |  .71 |  |  |  |  | .55 | 1.07 |
| Panic easily |  .49 |  |  |  |  | .35 | 1.96 |
| Often feel blue |  .47 | −.39 |  |  |  | .49 | 2.27 |
| Find it difficult to approach others |  | −.68 |  |  |  | .54 | 1.07 |
| Make friends easily |  |  .59 |  |  |  | .53 | 1.49 |
| Don’t talk a lot |  | −.56 |  |  |  | .35 | 1.21 |
| Take charge |  |  .42 |  |  |  | .40 | 2.60 |
| Know how to captivate people |  |  .42 |  |  |  | .44 | 2.55 |
| Continue until everything is perfect |  |  |  .67 |  |  | .45 | 1.17 |
| Do things in a half-way manner |  |  | −.61 |  |  | .45 | 1.18 |
| Do things according to a plan |  |  |  .57 |  |  | .32 | 1.11 |
| Waste my time |  |  | −.55 |  |  | .43 | 1.44 |
| Am exacting in my work |  |  |  .55 |  |  | .33 | 1.19 |
| Know how to comfort others |  |  |  |  .66 |  | .52 | 1.07 |
| Inquire about others’ well-being |  |  |  |  .64 |  | .45 | 1.04 |
| Make people feel at ease |  |  |  |  .53 |  | .46 | 1.49 |
| Love children |  |  |  |  .43 |  | .28 | 1.74 |
| Am indifferent to the feelings of others |  |  |  | −.41 |  | .19 | 1.97 |
| Carry the conversation to a higher level |  |  |  |  |  .61 | .46 | 1.17 |
| Will not probe deeply into a subject |  |  |  |  | −.54 | .30 | 1.21 |
| Am full of ideas |  |  |  |  |  .51 | .31 | 1.13 |
| Avoid difficult reading material |  |  |  |  | −.46 | .26 | 1.75 |
| Spend time reflecting on things |  | −.32 |  |  |  .37 | .25 | 2.69 |

Table 5: Factor Loadings, Communalities, and Complexity

### Variance explained

With `variance = TRUE`, three rows are added below the loadings: the sum
of squared loadings for each factor, the proportion of the total
variance it explains, and the cumulative proportion. A rule separates
these rows from the loadings.

``` r

apa_loadings(fit_bfi,
             factor_names = big5,
             format_variable_names = FALSE,
             min_loading = .30,
             variance = TRUE)
```

| Variable | Neuroticism | Extraversion | Conscientiousness | Agreeableness | Openness |
|----|----|----|----|----|----|
| Get angry easily |  .81 |  |  |  |  |
| Get irritated easily |  .78 |  |  |  |  |
| Have frequent mood swings |  .71 |  |  |  |  |
| Panic easily |  .49 |  |  |  |  |
| Often feel blue |  .47 | −.39 |  |  |  |
| Find it difficult to approach others |  | −.68 |  |  |  |
| Make friends easily |  |  .59 |  |  |  |
| Don’t talk a lot |  | −.56 |  |  |  |
| Take charge |  |  .42 |  |  |  |
| Know how to captivate people |  |  .42 |  |  |  |
| Continue until everything is perfect |  |  |  .67 |  |  |
| Do things in a half-way manner |  |  | −.61 |  |  |
| Do things according to a plan |  |  |  .57 |  |  |
| Waste my time |  |  | −.55 |  |  |
| Am exacting in my work |  |  |  .55 |  |  |
| Know how to comfort others |  |  |  |  .66 |  |
| Inquire about others’ well-being |  |  |  |  .64 |  |
| Make people feel at ease |  |  |  |  .53 |  |
| Love children |  |  |  |  .43 |  |
| Am indifferent to the feelings of others |  |  |  | −.41 |  |
| Carry the conversation to a higher level |  |  |  |  |  .61 |
| Will not probe deeply into a subject |  |  |  |  | −.54 |
| Am full of ideas |  |  |  |  |  .51 |
| Avoid difficult reading material |  |  |  |  | −.46 |
| Spend time reflecting on things |  | −.32 |  |  |  .37 |
| Sum of squared loadings | 2.57 | 2.20 | 2.03 | 1.98 | 1.59 |
| Proportion of variance | .10 | .09 | .08 | .08 | .06 |
| Cumulative proportion | .10 | .19 | .27 | .35 | .41 |

Table 6: Factor Loadings and Variance Explained

With an oblique rotation, the factors are correlated, so the variance
they explain overlaps. The sums of squared loadings describe the pattern
matrix, and the cumulative proportion is only approximate. Report the
factor correlations as well (see below). After an orthogonal rotation,
the cumulative proportion is exact.

## Factor Correlations

After an oblique rotation, report the correlations among the factors.
`apa_factor_cor` numbers the factors in the first column and uses the
numbers as column headers. The factors are in the same order as in
`apa_loadings`, so the same `factor_names` vector works for both.

``` r

apa_factor_cor(fit_bfi, factor_names = big5)
```

| Factor               | 1    | 2    | 3    | 4    | 5   |
|----------------------|------|------|------|------|-----|
| 1. Neuroticism       | —    |      |      |      |     |
| 2. Extraversion      | −.21 | —    |      |      |     |
| 3. Conscientiousness | −.19 |  .23 | —    |      |     |
| 4. Agreeableness     | −.04 |  .33 |  .20 | —    |     |
| 5. Openness          | −.01 |  .17 |  .19 |  .19 | —   |

Table 7: Correlations Among Big Five Factors

Neuroticism correlates negatively with the other factors because its
items describe negative emotions, while the other four factors are
scored in the socially desirable direction.

Factors from an orthogonal rotation (e.g., `rotate = "varimax"`) are
uncorrelated by definition, so `apa_factor_cor` gives an error for them,
as it does for one-factor solutions.

## Principal Components

`apa_loadings` and `apa_factor_cor` work the same way with principal
components from
[`psych::principal`](https://rdrr.io/pkg/psych/man/principal.html). Here
the components are rotated with varimax, an orthogonal rotation.

``` r

fit_pca <- principal(d_bfi, nfactors = 5, rotate = "varimax")
```

``` r

apa_loadings(fit_pca,
             factor_names = big5,
             format_variable_names = FALSE,
             min_loading = .30,
             variance = TRUE)
```

| Variable | Neuroticism | Extraversion | Conscientiousness | Agreeableness | Openness |
|----|----|----|----|----|----|
| Get angry easily |  .79 |  |  |  |  |
| Have frequent mood swings |  .79 |  |  |  |  |
| Get irritated easily |  .79 |  |  |  |  |
| Often feel blue |  .65 | −.34 |  |  |  |
| Panic easily |  .63 |  |  |  |  |
| Find it difficult to approach others |  | −.72 |  |  |  |
| Make friends easily |  |  .70 |  |  |  |
| Don’t talk a lot |  | −.68 |  |  |  |
| Know how to captivate people |  |  .63 |  |  |  |
| Take charge |  |  .58 |  .34 |  |  |
| Continue until everything is perfect |  |  |  .74 |  |  |
| Do things according to a plan |  |  |  .67 |  |  |
| Do things in a half-way manner |  |  | −.67 |  |  |
| Am exacting in my work |  |  |  .65 |  |  |
| Waste my time |  .33 |  | −.62 |  |  |
| Inquire about others’ well-being |  |  |  |  .71 |  |
| Know how to comfort others |  |  .35 |  |  .68 |  |
| Am indifferent to the feelings of others |  |  |  | −.63 |  |
| Make people feel at ease |  |  .44 |  |  .56 |  |
| Love children |  |  |  |  .53 |  |
| Will not probe deeply into a subject |  |  |  |  | −.68 |
| Carry the conversation to a higher level |  |  .36 |  |  |  .64 |
| Avoid difficult reading material |  |  |  |  | −.59 |
| Am full of ideas |  |  |  |  |  .59 |
| Spend time reflecting on things |  |  |  |  |  .50 |
| Sum of squared loadings | 3.18 | 3.09 | 2.56 | 2.32 | 2.11 |
| Proportion of variance | .13 | .12 | .10 | .09 | .08 |
| Cumulative proportion | .13 | .25 | .35 | .45 | .53 |

Table 8: Varimax-Rotated Principal Components of Big Five Personality
Items

## Output From the parameters Package

`apa_loadings` and `apa_factor_cor` also accept output from
[`parameters::factor_analysis`](https://easystats.github.io/parameters/reference/principal_components.html)
and
[`parameters::principal_components`](https://easystats.github.io/parameters/reference/principal_components.html),
the easystats functions for factor analysis and principal components.
The loadings come from the model that parameters fits, so the `sort` and
`threshold` arguments of the parameters functions make no difference.
Use `sort_loading` and `min_loading` instead.

``` r

efa_bfi <- parameters::factor_analysis(d_bfi, n = 5, rotation = "oblimin")

apa_loadings(efa_bfi,
             factor_names = big5,
             format_variable_names = FALSE,
             min_loading = .30)
```

| Variable | Neuroticism | Extraversion | Conscientiousness | Agreeableness | Openness |
|----|----|----|----|----|----|
| Get angry easily |  .81 |  |  |  |  |
| Get irritated easily |  .78 |  |  |  |  |
| Have frequent mood swings |  .71 |  |  |  |  |
| Panic easily |  .49 |  |  |  |  |
| Often feel blue |  .47 | −.39 |  |  |  |
| Find it difficult to approach others |  | −.68 |  |  |  |
| Make friends easily |  |  .59 |  |  |  |
| Don’t talk a lot |  | −.56 |  |  |  |
| Take charge |  |  .42 |  |  |  |
| Know how to captivate people |  |  .42 |  |  |  |
| Continue until everything is perfect |  |  |  .67 |  |  |
| Do things in a half-way manner |  |  | −.61 |  |  |
| Do things according to a plan |  |  |  .57 |  |  |
| Waste my time |  |  | −.55 |  |  |
| Am exacting in my work |  |  |  .55 |  |  |
| Know how to comfort others |  |  |  |  .66 |  |
| Inquire about others’ well-being |  |  |  |  .64 |  |
| Make people feel at ease |  |  |  |  .53 |  |
| Love children |  |  |  |  .43 |  |
| Am indifferent to the feelings of others |  |  |  | −.41 |  |
| Carry the conversation to a higher level |  |  |  |  |  .61 |
| Will not probe deeply into a subject |  |  |  |  | −.54 |
| Am full of ideas |  |  |  |  |  .51 |
| Avoid difficult reading material |  |  |  |  | −.46 |
| Spend time reflecting on things |  | −.32 |  |  |  .37 |

Table 9: Factor Loadings From parameters::factor_analysis

``` r

apa_factor_cor(efa_bfi, factor_names = big5)
```

| Factor               | 1    | 2    | 3    | 4    | 5   |
|----------------------|------|------|------|------|-----|
| 1. Neuroticism       | —    |      |      |      |     |
| 2. Extraversion      | −.21 | —    |      |      |     |
| 3. Conscientiousness | −.19 |  .23 | —    |      |     |
| 4. Agreeableness     | −.04 |  .33 |  .20 | —    |     |
| 5. Openness          | −.01 |  .17 |  .19 |  .19 | —   |

Table 10: Factor Correlations From parameters::factor_analysis

Without a rotation,
[`parameters::principal_components`](https://easystats.github.io/parameters/reference/principal_components.html)
uses [`stats::prcomp`](https://rdrr.io/r/stats/prcomp.html). The
loadings are the correlations between the items and the components (when
`standardize = TRUE`, the default), and the sums of squared loadings are
the eigenvalues. Unrotated components are hard to interpret, so they
keep their default names here.

``` r

parameters::principal_components(d_bfi, n = 5) |>
  apa_loadings(format_variable_names = FALSE,
               min_loading = .30,
               bold_max = TRUE,
               variance = TRUE)
```

| Variable                                 | PC1  | PC2  | PC3  | PC4  | PC5  |
|------------------------------------------|------|------|------|------|------|
| Find it difficult to approach others     |  .64 |      |      |      |  .35 |
| Make friends easily                      | −.62 |      |  .38 |      |      |
| Make people feel at ease                 | −.61 |      |  .31 |      |      |
| Know how to captivate people             | −.56 |  .36 |      |      |      |
| Know how to comfort others               | −.56 |  .32 |      |      |  .32 |
| Take charge                              | −.55 |  .31 |      |      | −.31 |
| Often feel blue                          |  .55 |  .48 |      |      |      |
| Waste my time                            |  .52 |      |      |  .35 |      |
| Inquire about others’ well-being         | −.50 |  .31 |      |      |  .41 |
| Do things in a half-way manner           |  .49 |      |  .48 |      |      |
| Love children                            | −.46 |      |      | −.33 |      |
| Don’t talk a lot                         |  .44 |      | −.33 |      |  .34 |
| Have frequent mood swings                |  .41 |  .67 |      |      |      |
| Get irritated easily                     |  .42 |  .65 |      |      |      |
| Get angry easily                         |  .44 |  .65 |      |      |      |
| Panic easily                             |  .37 |  .50 |      |      |      |
| Am exacting in my work                   | −.38 |      | −.52 |      |      |
| Continue until everything is perfect     | −.36 |      | −.51 | −.38 |      |
| Will not probe deeply into a subject     |      |      |  .37 | −.51 |      |
| Carry the conversation to a higher level | −.43 |  .33 |      |  .47 |      |
| Avoid difficult reading material         |      |      |  .41 | −.46 |      |
| Do things according to a plan            | −.35 |      | −.39 | −.44 |      |
| Am full of ideas                         | −.36 |      |      |  .42 |      |
| Am indifferent to the feelings of others |      |      |      |      | −.61 |
| Spend time reflecting on things          |      |  .34 |      |      |  .41 |
| Sum of squared loadings                  | 5.13 | 2.75 | 2.14 | 1.85 | 1.55 |
| Proportion of variance                   | .21  | .11  | .09  | .07  | .06  |
| Cumulative proportion                    | .21  | .32  | .40  | .48  | .54  |

Table 11: Unrotated Principal Components From
parameters::principal_components

## Formatted Variable Names

When the variables have names rather than text, the default
`format_variable_names = TRUE` makes them readable. The `Harman74.cor`
data set has the correlations among 24 ability tests, with names such as
`SentenceCompletion` and `VisualPerception`. Because
[`psych::fa`](https://rdrr.io/pkg/psych/man/fa.html) accepts a
correlation matrix, give it the sample size (`n.obs`) so that it can
compute fit statistics.

``` r

fit_harman <- fa(Harman74.cor$cov,
                 nfactors = 4,
                 n.obs = Harman74.cor$n.obs,
                 fm = "pa",
                 rotate = "varimax")

apa_loadings(fit_harman,
             factor_names = c("Verbal", "Spatial", "Speed", "Memory"),
             min_loading = .30,
             communality = TRUE)
```

| Variable                 | Verbal | Spatial | Speed | Memory | h2  |
|--------------------------|--------|---------|-------|--------|-----|
| Sentence Completion      | .81    |         |       |        | .73 |
| Word Meaning             | .81    |         |       |        | .74 |
| Pargraph Comprehension   | .76    |         |       |        | .68 |
| General Information      | .73    |         |       |        | .64 |
| Word Classification      | .57    |  .34    |       |        | .51 |
| Visual Perception        |        |  .68    |       |        | .55 |
| Paper Form Board         |        |  .55    |       |        | .34 |
| Flags                    |        |  .53    |       |        | .35 |
| Series Completion        | .37    |  .52    |       |        | .51 |
| Cubes                    |        |  .45    |       |        | .23 |
| Deduction                | .38    |  .42    |       |        | .42 |
| Problem Reasoning        | .37    |  .41    |       |        | .40 |
| Addition                 |        |         |  .82  |        | .74 |
| Counting Dots            |        |         |  .71  |        | .55 |
| Straight Curved Capitals |        |  .42    |  .54  |        | .51 |
| Code                     |        |         |  .54  | .37    | .47 |
| Arithmetic Problems      | .36    |         |  .49  |        | .49 |
| Numerical Puzzles        |        |  .40    |  .43  |        | .42 |
| Object Number            |        |         |       | .58    | .41 |
| Word Recognition         |        |         |       | .56    | .36 |
| Number Recognition       |        |         |       | .52    | .31 |
| Figure Recognition       |        |  .42    |       | .52    | .45 |
| Number Figure            |        |  .31    |  .34  | .45    | .41 |
| Figure Word              |        |         |       | .35    | .23 |

Table 12: Factor Loadings of 24 Ability Tests

The names are split into words and put in title case (e.g.,
SentenceCompletion becomes Sentence Completion). Names that are
misspelled in the data stay misspelled (the data set has
PargraphComprehension), but the tibble can be fixed before it is
displayed.

## Customizing

### Notes

A table of loadings should say how the factors were extracted and
rotated and which loadings were omitted. `apa_loadings` records these
details, and `apa_note` turns them into note text:

``` r

tbl_bfi <- apa_loadings(fit_bfi,
                        factor_names = big5,
                        format_variable_names = FALSE,
                        min_loading = .30,
                        bold_max = TRUE,
                        communality = TRUE,
                        variance = TRUE)
apa_note(tbl_bfi)
```

```
[1] "Principal axis factoring with oblimin rotation. Loadings are pattern coefficients. Loadings smaller than .30 in absolute value are omitted. The largest loading for each variable is in bold. *h*^2^ = communality. Because the factors are correlated, the variance they explain overlaps, and the cumulative proportion is approximate."
```

The note names the extraction method and rotation, says that loadings
after an oblique rotation are pattern coefficients, gives the cutoff for
omitted loadings, explains bold values, and defines *h*² (and *u*² and
Complexity, if shown). When there are variance rows and the factors are
correlated, it says that the cumulative proportion is approximate. The
text is in markdown and does not start with “*Note*.” because apaquarto
adds that.

With [apaquarto](https://wjschne.github.io/apaquarto/), use `apa_note`
in the `apa-note` chunk option with `!expr`. Chunk options are evaluated
before the chunk runs, so make the table in an earlier chunk (as
`tbl_bfi` was made above) and display it in the chunk with the note.
Write it as
[`apa7::apa_note`](https://wjschne.github.io/apa7/reference/apa_note.md),
because in documents with cached chunks, chunk options can be evaluated
before [`library(apa7)`](https://github.com/wjschne/apa7) takes effect:

```` markdown
```{r}
#| label: tbl-note
#| tbl-cap: Factor Loadings of Big Five Personality Items
#| apa-note: !expr apa7::apa_note(tbl_bfi)
tbl_bfi
```
````

| Variable | Neuroticism | Extraversion | Conscientiousness | Agreeableness | Openness | h2 |
|----|----|----|----|----|----|----|
| Get angry easily |  .81 |  |  |  |  | .65 |
| Get irritated easily |  .78 |  |  |  |  | .60 |
| Have frequent mood swings |  .71 |  |  |  |  | .55 |
| Panic easily |  .49 |  |  |  |  | .35 |
| Often feel blue |  .47 | −.39 |  |  |  | .49 |
| Find it difficult to approach others |  | −.68 |  |  |  | .54 |
| Make friends easily |  |  .59 |  |  |  | .53 |
| Don’t talk a lot |  | −.56 |  |  |  | .35 |
| Take charge |  |  .42 |  |  |  | .40 |
| Know how to captivate people |  |  .42 |  |  |  | .44 |
| Continue until everything is perfect |  |  |  .67 |  |  | .45 |
| Do things in a half-way manner |  |  | −.61 |  |  | .45 |
| Do things according to a plan |  |  |  .57 |  |  | .32 |
| Waste my time |  |  | −.55 |  |  | .43 |
| Am exacting in my work |  |  |  .55 |  |  | .33 |
| Know how to comfort others |  |  |  |  .66 |  | .52 |
| Inquire about others’ well-being |  |  |  |  .64 |  | .45 |
| Make people feel at ease |  |  |  |  .53 |  | .46 |
| Love children |  |  |  |  .43 |  | .28 |
| Am indifferent to the feelings of others |  |  |  | −.41 |  | .19 |
| Carry the conversation to a higher level |  |  |  |  |  .61 | .46 |
| Will not probe deeply into a subject |  |  |  |  | −.54 | .30 |
| Am full of ideas |  |  |  |  |  .51 | .31 |
| Avoid difficult reading material |  |  |  |  | −.46 | .26 |
| Spend time reflecting on things |  | −.32 |  |  |  .37 | .25 |
| Sum of squared loadings | 2.57 | 2.20 | 2.03 | 1.98 | 1.59 |  |
| Proportion of variance | .10 | .09 | .08 | .08 | .06 |  |
| Cumulative proportion | .10 | .19 | .27 | .35 | .41 |  |

Table 13: Factor Loadings of Big Five Personality Items

*Note*. Principal axis factoring with oblimin rotation. Loadings are
pattern coefficients. Loadings smaller than .30 in absolute value are
omitted. The largest loading for each variable is in bold. *h*² =
communality. Because the factors are correlated, the variance they
explain overlaps, and the cumulative proportion is approximate.

`apa_note` also works for factor correlation tables:

``` r

apa_factor_cor(fit_bfi, factor_names = big5) |>
  apa_note()
```

```
[1] "Principal axis factoring with oblimin rotation."
```

Without apaquarto, `apa_flextable(tbl, note = TRUE)` puts the same note
below the table (or pass your own text as `note`). Do not combine it
with the `apa-note` chunk option, or the note will appear twice.

The generated note is a starting point. Add to it with `paste` (e.g., to
give the sample size), or use `collapse = NULL` to get one sentence per
element, so you can drop or reword sentences. `apa_note` reads an
attribute of the table that dplyr functions such as `mutate` and
`filter` keep but some operations (e.g., joins) drop, so call it on the
table that `apa_loadings` returned.

### Post-processing

The output is a tibble, so dplyr functions can change it before it is
displayed. For example, to fix the misspelled test name:

``` r

apa_loadings(fit_harman,
             factor_names = c("Verbal", "Spatial", "Speed", "Memory"),
             min_loading = .30) |>
  mutate(Variable = str_replace(Variable, "Pargraph", "Paragraph"))
```

| Variable                 | Verbal | Spatial | Speed | Memory |
|--------------------------|--------|---------|-------|--------|
| Sentence Completion      | .81    |         |       |        |
| Word Meaning             | .81    |         |       |        |
| Paragraph Comprehension  | .76    |         |       |        |
| General Information      | .73    |         |       |        |
| Word Classification      | .57    |  .34    |       |        |
| Visual Perception        |        |  .68    |       |        |
| Paper Form Board         |        |  .55    |       |        |
| Flags                    |        |  .53    |       |        |
| Series Completion        | .37    |  .52    |       |        |
| Cubes                    |        |  .45    |       |        |
| Deduction                | .38    |  .42    |       |        |
| Problem Reasoning        | .37    |  .41    |       |        |
| Addition                 |        |         |  .82  |        |
| Counting Dots            |        |         |  .71  |        |
| Straight Curved Capitals |        |  .42    |  .54  |        |
| Code                     |        |         |  .54  | .37    |
| Arithmetic Problems      | .36    |         |  .49  |        |
| Numerical Puzzles        |        |  .40    |  .43  |        |
| Object Number            |        |         |       | .58    |
| Word Recognition         |        |         |       | .56    |
| Number Recognition       |        |         |       | .52    |
| Figure Recognition       |        |  .42    |       | .52    |
| Number Figure            |        |  .31    |  .34  | .45    |
| Figure Word              |        |         |       | .35    |

Table 14: Factor Loadings of 24 Ability Tests

The variable names are formatted by `apa_loadings`, not by
`apa_flextable`. The `Variable` column is marked so that `apa_flextable`
leaves it alone, so item text is never changed when the table is
displayed, even after dplyr functions like `mutate` and `filter`.

### Further formatting with flextable

Pass the table to `apa_flextable` for more options (e.g., `table_width`)
and then to flextable functions for more control. The rule above the
variance rows is added by `apa_flextable`, which finds the row by its
label, so the rule stays in place even if rows are filtered or reordered
first. Here the table is set in a smaller font:

``` r

apa_loadings(fit_bfi,
             factor_names = big5,
             format_variable_names = FALSE,
             min_loading = .30,
             variance = TRUE) |>
  apa_flextable(font_size = 10)
```

| Variable | Neuroticism | Extraversion | Conscientiousness | Agreeableness | Openness |
|----|----|----|----|----|----|
| Get angry easily |  .81 |  |  |  |  |
| Get irritated easily |  .78 |  |  |  |  |
| Have frequent mood swings |  .71 |  |  |  |  |
| Panic easily |  .49 |  |  |  |  |
| Often feel blue |  .47 | −.39 |  |  |  |
| Find it difficult to approach others |  | −.68 |  |  |  |
| Make friends easily |  |  .59 |  |  |  |
| Don’t talk a lot |  | −.56 |  |  |  |
| Take charge |  |  .42 |  |  |  |
| Know how to captivate people |  |  .42 |  |  |  |
| Continue until everything is perfect |  |  |  .67 |  |  |
| Do things in a half-way manner |  |  | −.61 |  |  |
| Do things according to a plan |  |  |  .57 |  |  |
| Waste my time |  |  | −.55 |  |  |
| Am exacting in my work |  |  |  .55 |  |  |
| Know how to comfort others |  |  |  |  .66 |  |
| Inquire about others’ well-being |  |  |  |  .64 |  |
| Make people feel at ease |  |  |  |  .53 |  |
| Love children |  |  |  |  .43 |  |
| Am indifferent to the feelings of others |  |  |  | −.41 |  |
| Carry the conversation to a higher level |  |  |  |  |  .61 |
| Will not probe deeply into a subject |  |  |  |  | −.54 |
| Am full of ideas |  |  |  |  |  .51 |
| Avoid difficult reading material |  |  |  |  | −.46 |
| Spend time reflecting on things |  | −.32 |  |  |  .37 |
| Sum of squared loadings | 2.57 | 2.20 | 2.03 | 1.98 | 1.59 |
| Proportion of variance | .10 | .09 | .08 | .08 | .06 |
| Cumulative proportion | .10 | .19 | .27 | .35 | .41 |

Table 15: Factor Loadings and Variance Explained

## References

Hofmann, R. J. (1978). Complexity and simplicity as objective indices
descriptive of factor solutions. *Multivariate Behavioral Research,
13*(2), 247–250. <https://doi.org/10.1207/s15327906mbr1302_9>

Revelle, W. (2026). *psych: Procedures for psychological, psychometric,
and personality research* (Version 2.6.9) \[Computer software\].
Northwestern University. <https://CRAN.R-project.org/package=psych>
