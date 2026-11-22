# Changelog

## apa7 (development version)

- New
  [`apa_htest()`](https://wjschne.github.io/apa7/reference/apa_htest.md)
  runs *t* tests (Welch’s or Student’s, two-group or one-sample),
  Wilcoxon rank-sum and signed-rank tests, correlation tests (Pearson,
  Spearman, or Kendall), and chi-square tests of independence on the
  variables in a data frame, and makes a table with one row per outcome
  (e.g., `apa_htest(d, mpg + hp ~ am)`). It shows group means and
  standard deviations (or medians) under group spanners, the test
  statistic, *df*, *p*, and an effect size with a two-sided confidence
  interval: Cohen’s *d* or Hedges’ *g*, the rank-biserial correlation,
  or Cramér’s *V*, *φ*, or Cohen’s *w*. Effect sizes are computed from
  the data, so they do not depend on where the function is called, and
  Cohen’s *d* for Welch’s test is exact rather than approximated from
  *t*.
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  says which tests were done, how *d* was standardized, and what the
  abbreviations stand for.
- [`apa_htest()`](https://wjschne.github.io/apa7/reference/apa_htest.md)
  runs paired *t* tests and Wilcoxon signed-rank tests of paired
  variables with [`Pair()`](https://rdrr.io/r/stats/Pair.html) in the
  formula (e.g., `apa_htest(d, Pair(pre, post) ~ 1)`, or several pairs
  joined with `+`). Paired *t* tests show *d*_(av) by default, which is
  comparable to *d* for independent groups; *d*_(z), *d*_(rm), and
  Hedges’ *g* versions come from
  [`effectsize::repeated_measures_d()`](https://easystats.github.io/effectsize/reference/repeated_measures_d.html).
  New `pair_labels` argument sets the headings above each pair’s
  descriptive statistics.
- [`apa_anova()`](https://wjschne.github.io/apa7/reference/apa_anova.md)
  makes *F* tables of the fixed effects of linear mixed models from
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) or
  [`lmerTest::lmer()`](https://rdrr.io/pkg/lmerTest/man/lmer.html), and
  formats ANOVA tables from lmerTest’s
  [`anova()`](https://rdrr.io/r/stats/anova.html). Tests come from
  lmerTest, with Kenward-Roger (default, as in
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md))
  or Satterthwaite (`ddf = "Satterthwaite"`) denominator degrees of
  freedom, and are Type III by default. The table shows *df*_(effect),
  *df*_(error), *F*, and *p*. Partial effect sizes, computed from *F*
  and its degrees of freedom, are shown only when requested (e.g.,
  `effect_size = "eta"`).
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  gives the type of tests, the degrees-of-freedom method, and how the
  effect size was computed.
- Table notes are now single-spaced, even when the table is
  double-spaced. This applies to `apa_flextable(note = TRUE)`,
  [`apa_cor()`](https://wjschne.github.io/apa7/reference/apa_cor.md),
  [`apa_chisq()`](https://wjschne.github.io/apa7/reference/apa_chisq.md),
  and footers styled by
  [`apa_style()`](https://wjschne.github.io/apa7/reference/apa_style.md).
  `line_spacing` now sets only the spacing of the header and body.

## apa7 0.2.0 *2026-10-18*

CRAN release: 2026-11-21

- New
  [`apa_anova()`](https://wjschne.github.io/apa7/reference/apa_anova.md)
  makes APA-style ANOVA source tables for `aov` and `lm` models (Type 1
  sums of squares, or Types 2 and 3 via
  [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html)),
  repeated-measures `aov` models with `Error()` strata, `afex` models,
  and tables from [`stats::anova()`](https://rdrr.io/r/stats/anova.html)
  and [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html). Effect
  sizes (partial, non-partial, or generalized *η*², *ω*², or *ε*²) and
  their two-sided confidence intervals come from `effectsize`.

- New
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  writes the text of a table note. For
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  tables of regression models (including lists of models and `lme4`
  models), it says which robust standard errors were used, how
  mixed-model degrees of freedom were approximated, and that ordinal
  thresholds are not tested, and it defines the standardized
  coefficient, exponentiated coefficient (e.g., *OR*), confidence
  interval, and *R*² change columns. For
  [`apa_loadings()`](https://wjschne.github.io/apa7/reference/apa_loadings.md)
  and
  [`apa_factor_cor()`](https://wjschne.github.io/apa7/reference/apa_factor_cor.md)
  tables, it gives the extraction method and rotation, says that oblique
  loadings are pattern coefficients, and explains omitted and bolded
  loadings and the *h*², *u*², and Complexity columns. For
  [`apa_anova()`](https://wjschne.github.io/apa7/reference/apa_anova.md)
  tables, it gives the type of sums of squares, which effects have
  sphericity-corrected degrees of freedom (`afex` models), what the
  effect size and *MSE* stand for, and what stars or bold values mean.
  With apaquarto, use it as `#| apa-note: !expr apa7::apa_note(tbl)`.
  Without apaquarto, `apa_flextable(note = TRUE)` places the note below
  the table.

- [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
  now returns the ANOVA table for `aov`, repeated-measures `aov`, and
  `afex` models. Before, `aov` models were treated as `lm` models and
  gave a coefficients table.

- [`apa_loadings()`](https://wjschne.github.io/apa7/reference/apa_loadings.md)
  has been reworked:

  - It now returns an `apa7_tbl`, so it renders as a flextable in Quarto
    and R Markdown. It supports
    [`psych::principal()`](https://rdrr.io/pkg/psych/man/principal.html)
    as well as [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html),
    and output from
    [`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html)
    and
    [`parameters::principal_components()`](https://easystats.github.io/parameters/reference/principal_components.html)
    (including unrotated components from
    [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html)).
  - New `bold_max` argument bolds the largest loading in each row.
  - New `factor_names` argument relabels the factors.
  - New `variance` argument adds rows for the sum of squared loadings
    and the proportion and cumulative proportion of variance explained,
    with a rule above them.
  - New `communality`, `digits`, and `format_variable_names` arguments.
    [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
    no longer reformats item text in the `Variable` column.
  - Small loadings are now blank strings rather than `NA`.
  - The unused `column_formats` argument has been removed.

- New
  [`apa_factor_cor()`](https://wjschne.github.io/apa7/reference/apa_factor_cor.md)
  makes a table of factor correlations from obliquely rotated
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html),
  [`psych::principal()`](https://rdrr.io/pkg/psych/man/principal.html),
  and
  [`parameters::factor_analysis()`](https://easystats.github.io/parameters/reference/principal_components.html)
  solutions.

- [`column_format()`](https://wjschne.github.io/apa7/reference/column_format.md)
  gains an `abbreviation` slot that says what a header stands for (e.g.,
  `"standardized coefficient"` for β).
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  defines every abbreviated header in a table from these slots, for any
  table type, so setting the slot in `column_formats` adds or changes a
  definition. The default column formats define common abbreviations
  (e.g., CI, *AIC*, *BIC*, RMSEA, effect sizes, and *R*² variants), and
  [`column_formats()`](https://wjschne.github.io/apa7/reference/column_formats.md)
  gains a `get_abbreviations` getter. New column formats `h2`, `u2`, and
  `Complexity` set the headers of those
  [`apa_loadings()`](https://wjschne.github.io/apa7/reference/apa_loadings.md)
  columns.

- [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
  gains a `note` argument: `TRUE` adds the note from
  [`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
  below the table, and a character string adds a custom note. The note
  does not affect column widths.

- [`apa_flextable()`](https://wjschne.github.io/apa7/reference/apa_flextable.md)
  leaves alone any columns named in a `"no_format_columns"` attribute of
  the data, and draws a rule above the row named in a `"rule_above"`
  attribute.

- Partial *η*² columns (`Eta2_partial`) are now headed *η*²_(p). New
  column formats: `Sum_Squares`, `Mean_Square`, `Eta2_generalized`,
  `Omega2`, `Omega2_partial`, `Epsilon2`, and `Epsilon2_partial`.

- [`make_apaquarto()`](https://wjschne.github.io/apa7/reference/make_apaquarto.md)
  and
  [`install_apaquarto()`](https://wjschne.github.io/apa7/reference/install_apaquarto.md)
  now list missing packages separately and ask a short “Install them
  now?” question, so the prompt is readable in the console (e.g., R on
  macOS). New `ask` argument installs missing packages without asking
  when `FALSE`. If installation is declined, the function stops and
  prints the
  [`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
  command to run.

## apa7 0.1.3 *2026-05-25*

CRAN release: 2026-05-26

- Used AI (Claude) and old-fashioned effort to generate a more complete
  set of unit tests, fix minor bugs, and make documentation more
  consistent.
- User prompted to install any missing packages when running the
  `make_apaquarto` shiny app locally.

## apa7 0.1.1 *2025-09-07*

- Fix `align_chr` so that setting `format_numeric_character = FALSE`
  works.
- `list_column` gains a `unite` parameter that joins list and text
  columns.
- `add_star_column` no longer add stars if the column value is missing.

## apa7 0.1.0 *2025-08-19*

CRAN release: 2025-09-10

- Initial Cran Release

## apa7 0.0.0.9002

- removed apaquarto installation functions

## apa7 0.0.0.9001

- Added S7 objects for automatic column formats

## apa7 0.0.0.9000

- Initial development version
