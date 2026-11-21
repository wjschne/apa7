# apa7 Development Priorities

Working checklist for apa7 development. Check items off (`- [x]`) as they are finished, and add new items under the right section. Keep finished items in place until the next release, then move them to the **Done** section (and to NEWS.md).

Last reviewed: 2026-11-20

## Before CRAN submission

- [x] Commit the current changes in `R/tables.R`, `vignettes/articles/multilevel.qmd`, and `vignettes/articles/regression.qmd`
- [x] Make the version numbers match (both are 0.2.0)
- [x] Make `apa_parameters()` handle `aov` objects correctly (they now return the `apa_anova()` table)
- [ ] Put new model packages (e.g., `afex`, `car`, `glmmTMB`) in Suggests and guard them with `requireNamespace()` in examples and tests (`afex`, `car`, `lme4`, `MASS`, `carData`, `ordinal`, and `sandwich` done; add `glmmTMB` with its methods)
- [x] Update NEWS.md with the new functions and methods
- [x] Run `R CMD check --as-cran` locally with 0 errors, 0 warnings, and 0 notes (clean on 2026-11-20 with R 4.6.2 on Windows). Build with `--compact-vignettes=qpdf` (e.g., `devtools::build(args = "--compact-vignettes=qpdf")`), or the vignette PDF gives a size NOTE. Issues fixed from the earlier 2026-11-20 check:
  - [x] Add `sandwich` to Suggests (the `vcov = "HC3"` example and two robust-SE tests fail without it) and guard those with `requireNamespace()` / `skip_if_not_installed()`
  - [x] Add `withr` to Suggests (used in tests)
  - [x] Replace non-ASCII characters in code with `\u` escapes in `R/formatting.R`, `R/tables.R`, and `R/anova.R`
  - [x] Document `include_names` in `matrix_display()`
  - [x] Remove `xfun` from Imports or use it
  - [x] Declare globals: `:=` in `apa_format_columns()` (replaced with `[[<-`), `Model` in `apa_parameters.list()`
  - [x] Shorten the long file path under `vignettes/apa7_files/libs/` (the build-ignore pattern is now `^vignettes/.*_files$`)
  - [x] Move packages used unconditionally in package code (`cli`, `knitr`, `snakecase`, `tidyselect`) from Suggests to Imports, and replace `R.utils::insert()` with a base helper
- [ ] Run `devtools::check_win_devel()` and `rhub` checks
- [x] Run `spelling::spell_check_package()` and `urlchecker::url_check()` (2026-11-20: no typos; terms added to `inst/WORDLIST`; redundant `doi.apa.org` URLs removed from `refs.bib`, whose DOIs remain)
- [ ] Rebuild the pkgdown site and vignettes
- [ ] Update `cran-comments.md`

## easystats tables

### 1. ANOVA tables

- [x] `aov` method: source table with *SS*, *df*, *MS*, *F*, *p*
- [x] Effect sizes from `effectsize`: η², partial η², generalized η², ω², ε², with optional two-sided CIs
- [x] `car::Anova` (Type II/III) support, also via `apa_anova(lm_fit, type = 2)`
- [x] `afex` (`afex_aov`) support for within-subjects and mixed designs, including sphericity corrections
- [x] Repeated-measures `aov` with `Error()` strata (Between/Within subjects heading rows)
- [x] `anova(m1, m2)` model comparison objects: give an error pointing to `apa_performance_comparison()`
- [x] Tests (`tests/testthat/test-anova.R`)
- [x] Vignette article (`vignettes/articles/anova.qmd`)
- [ ] `lmerTest` ANOVA tables for mixed models (F tests with Satterthwaite/Kenward-Roger df)
- [x] Table note helper: type of SS and sphericity correction (e.g., "Greenhouse–Geisser corrected"). See **Table notes** below.
- [ ] Revisit generalized η² CIs if `effectsize` fixes them (they can now exclude the point estimate, so they're hidden)

### 2. htest objects

- [ ] Single `htest` table via `parameters::model_parameters()` (`t.test`, `wilcox.test`, `cor.test`, `chisq.test`)
- [ ] Effect sizes via `effectsize` (Cohen's *d*, Hedges' *g*, rank-biserial *r*, Cramér's V)
- [ ] List method that stacks several tests into one table
- [ ] Group-comparison table: *M* and *SD* by group, plus *t*, *df*, *p*, *d*
- [ ] Tests
- [ ] Vignette article

### 3. Factor analysis (`apa_loadings()`)

- [x] Return an `apa7_tbl` like the other table functions
- [x] Support `psych::principal` in addition to `psych::fa`
- [x] Optional rows for variance explained (SS loadings, proportion, cumulative)
- [x] Optional factor correlation table for oblique rotations (`apa_factor_cor()`)
- [x] Tests (`tests/testthat/test-loadings.R`) and vignette examples (`vignettes/apa7.qmd`)
- [x] Vignette article (`vignettes/articles/factor-analysis.qmd`)
- [x] Rule above the variance rows in the flextable, so they stand apart from the loadings
- [x] Bold the largest loading in each row (optional, `bold_max = TRUE`)
- [x] Support `parameters::factor_analysis()` / `principal_components()` output directly

### 4. More mixed-model classes

- [ ] `glmmTMB` methods for `apa_parameters()` and `apa_performance()`
- [ ] `nlme::lme` methods
- [ ] Tests

### 5. After CRAN

- [ ] lavaan CFA/SEM tables (loadings, fit indices)
- [ ] Bayesian models (`brms`, `rstanarm`) via `bayestestR`
- [ ] Consider moving `apa_cor()` to the easystats `correlation` package

## Table notes

Tables record how they were made in an `apa_note` attribute, and `apa_note(tbl)` turns that record into note text in APA order (general note, then specific notes, then probability notes). The text is a starting point that authors can edit or add to.

- [x] ANOVA: `apa_anova()` records SS type, sphericity correction (afex), effect size (partial/generalized), and CI level; `apa_note()` writes the note
- [x] Check that `#| apa-note: !expr apa7::apa_note(tbl)` works with apaquarto's `apanote` filter (the table must be made in an earlier chunk)
- [x] Sphericity wording: afex corrects only within-subjects effects with more than two levels, so say "where applicable" or name the effects
- [x] Factor analysis: `apa_loadings()` records extraction method, rotation, `min_loading`, and `bold_max` (also `apa_factor_cor()`)
- [x] Regression: `apa_parameters()` records robust SE type (`vcov`), standardized coefficients, and star thresholds (reuse `apa_p_star_note()`)
- [x] `apa_flextable(x, note = TRUE)` adds the generated note as a footer line for plain Quarto, R Markdown, and Word (or `note = "text"` for custom text), like `apa_cor()`'s `note` argument
- [x] Column-format definitions: an `abbreviation` slot in `column_format()` (e.g., `*MSE*` = mean square error) so `apa_note()` defines every abbreviation used in a table's headers, for any table type
- [x] Give a clear message when the `apa_note` attribute has been lost (e.g., after `bind_rows()` or a join)

## Ideas / backlog

<!-- New ideas go here until they get a priority -->

- [ ]

## Done

<!-- Move finished items here at release time -->
