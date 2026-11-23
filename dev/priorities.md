# apa7 Development Priorities

Working checklist for apa7 development. Check items off (`- [x]`) as they are finished, and add new items under the right section. Keep finished items in place until the next release, then move them to the **Done** section (and to NEWS.md).

Last reviewed: 2026-11-23

## Before CRAN submission

- [x] Commit the current changes in `R/tables.R`, `vignettes/articles/multilevel.qmd`, and `vignettes/articles/regression.qmd`
- [x] Make the version numbers match (both are 0.2.0)
- [x] Make `apa_parameters()` handle `aov` objects correctly (they now return the `apa_anova()` table)
- [x] Put new model packages (e.g., `afex`, `car`, `glmmTMB`) in Suggests and guard them with `requireNamespace()` in examples and tests (`afex`, `car`, `glmmTMB`, `lme4`, `lmerTest`, `MASS`, `carData`, `ordinal`, and `sandwich` done)
- [x] Update NEWS.md with the new functions and methods
- [x] Run `R CMD check --as-cran` locally with 0 errors, 0 warnings, and 0 notes (clean on 2026-11-20 with R 4.6.2 on Windows). (An earlier size NOTE about `inst/doc/apa7.pdf` came from a stale `vignettes/apa7.pdf`, now deleted; the vignette is HTML only.) Issues fixed from the earlier 2026-11-20 check:
  - [x] Add `sandwich` to Suggests (the `vcov = "HC3"` example and two robust-SE tests fail without it) and guard those with `requireNamespace()` / `skip_if_not_installed()`
  - [x] Add `withr` to Suggests (used in tests)
  - [x] Replace non-ASCII characters in code with `\u` escapes in `R/formatting.R`, `R/tables.R`, and `R/anova.R`
  - [x] Document `include_names` in `matrix_display()`
  - [x] Remove `xfun` from Imports or use it
  - [x] Declare globals: `:=` in `apa_format_columns()` (replaced with `[[<-`), `Model` in `apa_parameters.list()`
  - [x] Shorten the long file path under `vignettes/apa7_files/libs/` (the build-ignore pattern is now `^vignettes/.*_files$`)
  - [x] Move packages used unconditionally in package code (`cli`, `knitr`, `snakecase`, `tidyselect`) from Suggests to Imports, and replace `R.utils::insert()` with a base helper
- [x] Run `devtools::check_win_devel()` and `rhub` checks (2026-11-21: rhub Linux and macOS arm64 pass; rhub Windows fails only because its runner's Quarto R session cannot find the temporary library, while win-builder builds the vignette fine; win-builder resubmitted after fixing the PDF manual and example timing)
- [x] Run `spelling::spell_check_package()` and `urlchecker::url_check()` (2026-11-20: no typos; terms added to `inst/WORDLIST`; redundant `doi.apa.org` URLs removed from `refs.bib`, whose DOIs remain)
- [x] Rebuild the pkgdown site and vignettes (deployed from `4b7c1ff` by the pkgdown workflow on 2026-11-21)
- [x] Update `cran-comments.md` (confirm the win-builder resubmission is clean before submitting)

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
- [x] `lmerTest` ANOVA tables for mixed models (F tests with Kenward-Roger (default) or Satterthwaite df; Type III by default; partial effect sizes from *F* only on request)
- [x] Table note helper: type of SS and sphericity correction (e.g., "Greenhouse–Geisser corrected"). See **Table notes** below.
- [ ] Revisit generalized η² CIs if `effectsize` fixes them (they can now exclude the point estimate, so they're hidden)

### 2. htest objects

Decision (2026-11-21): `apa_htest(data, formula, test)` runs the tests itself, because `htest` objects don't keep their data, so `effectsize` can't reliably find it (the formula interface never works, and Cohen's *d* silently falls back to an approximation from *t*).

- [x] `apa_htest()` for *t* tests (Welch, Student, one-sample), Wilcoxon (rank-sum, signed-rank), correlation (Pearson, Spearman, Kendall), and chi-square tests of independence
- [x] Effect sizes computed from the data with `effectsize` (Cohen's *d*, Hedges' *g*, rank-biserial *r*, Cramér's *V*, *φ*, Cohen's *w*), with two-sided CIs
- [x] Several outcomes stacked into one table (`y1 + y2 ~ group`)
- [x] Group-comparison table: *M* and *SD* by group (group spanners), plus *t*, *df*, *p*, *d*
- [x] Table note via `apa_note()`
- [x] Tests (`tests/testthat/test-htest.R`)
- [x] Paired *t* tests and Wilcoxon signed-rank tests of paired variables, with `Pair(pre, post) ~ 1` (*d*~av~ by default; *d*~z~, *d*~rm~, and *g* versions)
- [x] Accept a plain `htest` object (or a list of them), with effect sizes computed from what the object keeps (and the note saying so)
- [ ] Stack different kinds of tests in one table (list method)
- [x] Vignette article (`vignettes/articles/htest.qmd`)

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

- [x] `glmmTMB` methods for `apa_parameters()` and `apa_performance()` (zero-inflation and dispersion parts under their own headings; only the conditional model exponentiated)
- [x] `nlme::lme` methods (nlme's df; residual correlation and variance structures as rows, named in the note)
- [x] Tests (`test-glmmtmb.R`, `test-nlme.R`)

### 5. After CRAN

- [x] lavaan CFA/SEM tables (arrows in path labels; standardized estimates by default)
  - [x] Fit indices with `apa_performance()` (*χ*², CFI, TLI, RMSEA [90% CI], SRMR; scaled tests and robust indices for MLR/WLSMV; note with *N*, estimator, FIML)
  - [x] Parameter estimates with `apa_parameters()` (loadings, regressions, correlations, defined parameters under headings; standardized by default)
  - [x] Multiple-group models in `apa_parameters()` (groups side by side by default; stacked with `groups = "rows"`)
  - [x] CFA loading matrix with `apa_loadings()` (and latent correlations with `apa_factor_cor()`)
  - [x] Nested-model and measurement-invariance comparisons with `apa_performance_comparison()` (each model against the previous one)
- [x] Let `apa_cor()` accept output from `correlation::correlation()` (not yet: grouped, two-set, or Bayesian correlations)

### 6. Other analyses reported by easystats

Roughly in order of how often they appear in APA-style papers. `apa_parameters()` and `apa_performance()` have no default method, so other model classes give errors until they are supported. Bayesian models wait until the rest are more or less complete.

- [x] Descriptive and sample tables ("Table 1") with `apa_descriptives()`: *M* (*SD*) or *Mdn* (*IQR*) and *n* (%), a column for each group and the total, and optional tests of group differences (computed in apa7 rather than with `report::report_sample()`, so the layout and notes match the other tables)
  - [ ] Possible additions: ranges (min–max), a missing-values row or column, and Fisher's exact test for sparse tables
- [x] Estimated marginal means, contrasts, and simple slopes (post hoc tests, moderation) with `apa_means()`, `apa_contrasts()`, and `apa_slopes()`, from `modelbased::estimate_means()`, `estimate_contrasts()`, and `estimate_slopes()`: means with CIs, differences with adjusted *p*-values, heading rows for other `by` variables
  - [x] Cohen's *d* [CI] for contrasts (residual *SD* of `lm`/`aov` models, or `sd` for mixed models)
  - [ ] Possible additions: joint and omnibus tests, and Johnson–Neyman intervals
- [x] Reliability and item analysis with `apa_reliability()`: item *M*, *SD*, *r*~it~, and α if deleted; scale mean, α [CI], *ω*~total~, and mean inter-item *r*. Computed with `psych`, which covers what `performance`'s `cronbachs_alpha()`, `item_reliability()`, `item_difficulty()`, `item_discrimination()`, `item_intercor()`, and `item_omega()` report and adds the CI of α.
- [ ] Other regression families via `parameters`
  - [x] Multinomial logistic regression (`nnet::multinom`): one block of coefficients per outcome category, with McFadden's *R*^2^ and likelihood ratio tests for comparisons
  - [x] Zero-inflated and hurdle count models outside mixed models (`pscl::zeroinfl`, `pscl::hurdle`): count and zero models under headings, only the count model exponentiated, LR tests for comparisons
  - [ ] Survival models (`survival::coxph`, `survreg`): hazard ratios
  - [ ] Robust and quantile regression (`MASS::rlm`, `robustbase`, `quantreg`)
  - [ ] Beta regression, GEE, panel and fixed-effects, and instrumental-variable models (`betareg`, `geepack`, `fixest`, `ivreg`), possibly through one generic method
- [ ] Generalized additive models (`mgcv::gam`): parametric terms, and smooth terms with *edf* and *F*
- [x] Collinearity diagnostics with `apa_collinearity()`, from `performance::check_collinearity()`: *VIF* [CI] and tolerance for each predictor, generalized *VIF*s, headings for model parts, bolded large *VIF*s
- [ ] Mediation (`mediation::mediate`): ACME, ADE, and total effect with CIs (beyond `lavaan`'s defined parameters)
- [ ] Factor retention (`parameters::n_factors()`) and cluster analysis (`parameters::cluster_analysis()`)
- [x] Meta-analyses (`metafor::rma()`): study-level effect sizes with weights and the pooled estimate, heterogeneity (*Q*, *I*², *τ*²) with `apa_performance()`, and meta-regressions (computed from the metafor object rather than `parameters`, which gives equal-effects inverse-variance weights rather than percentages and exponentiates only the pooled estimate)
  - [ ] Possible additions: `rma.mv` (multilevel and multivariate models), `rma.mh`/`rma.peto`, subgroup tables, and publication-bias tests (Egger's test, trim and fill)
- [ ] Bayesian models (`brms`, `rstanarm`) via `bayestestR` (on hold until the other major easystats analyses are supported)

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
