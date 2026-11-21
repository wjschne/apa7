# apa7 0.2.0 _2026-10-18_

* New `apa_anova()` makes APA-style ANOVA source tables for `aov` and `lm` models (Type 1 sums of squares, or Types 2 and 3 via `car::Anova()`), repeated-measures `aov` models with `Error()` strata, `afex` models, and tables from `stats::anova()` and `car::Anova()`. Effect sizes (partial, non-partial, or generalized *η*^2^, *ω*^2^, or *ε*^2^) and their two-sided confidence intervals come from `effectsize`.
* New `apa_note()` writes the text of a table note. For `apa_parameters()` tables of regression models (including lists of models and `lme4` models), it says which robust standard errors were used, how mixed-model degrees of freedom were approximated, and that ordinal thresholds are not tested, and it defines the standardized coefficient, exponentiated coefficient (e.g., *OR*), confidence interval, and *R*^2^ change columns. For `apa_loadings()` and `apa_factor_cor()` tables, it gives the extraction method and rotation, says that oblique loadings are pattern coefficients, and explains omitted and bolded loadings and the *h*^2^, *u*^2^, and Complexity columns. For `apa_anova()` tables, it gives the type of sums of squares, which effects have sphericity-corrected degrees of freedom (`afex` models), what the effect size and *MSE* stand for, and what stars or bold values mean. With apaquarto, use it as `#| apa-note: !expr apa_note(tbl)`.
* `apa_parameters()` now returns the ANOVA table for `aov`, repeated-measures `aov`, and `afex` models. Before, `aov` models were treated as `lm` models and gave a coefficients table.
* `apa_loadings()` has been reworked:
  * It now returns an `apa7_tbl`, so it renders as a flextable in Quarto and R Markdown. It supports `psych::principal()` as well as `psych::fa()`, and output from `parameters::factor_analysis()` and `parameters::principal_components()` (including unrotated components from `stats::prcomp()`).
  * New `bold_max` argument bolds the largest loading in each row.
  * New `factor_names` argument relabels the factors.
  * New `variance` argument adds rows for the sum of squared loadings and the proportion and cumulative proportion of variance explained, with a rule above them.
  * New `communality`, `digits`, and `format_variable_names` arguments. `apa_flextable()` no longer reformats item text in the `Variable` column.
  * Small loadings are now blank strings rather than `NA`.
  * The unused `column_formats` argument has been removed.
* New `apa_factor_cor()` makes a table of factor correlations from obliquely rotated `psych::fa()`, `psych::principal()`, and `parameters::factor_analysis()` solutions.
* `apa_flextable()` leaves alone any columns named in a `"no_format_columns"` attribute of the data, and draws a rule above the row named in a `"rule_above"` attribute.
* Partial *η*^2^ columns (`Eta2_partial`) are now headed *η*^2^~p~. New column formats: `Sum_Squares`, `Mean_Square`, `Eta2_generalized`, `Omega2`, `Omega2_partial`, `Epsilon2`, and `Epsilon2_partial`.

* `make_apaquarto()` and `install_apaquarto()` now list missing packages separately and ask a short "Install them now?" question, so the prompt is readable in the console (e.g., R on macOS). New `ask` argument installs missing packages without asking when `FALSE`. If installation is declined, the function stops and prints the `install.packages()` command to run.

# apa7 0.1.2 _2026-05-25_

* Used AI (Claude) and old-fashioned effort to generate a more complete set of unit tests, fix minor bugs, and make documentation more consistent.
* User prompted to install any missing packages when running the `make_apaquarto` shiny app locally.

# apa7 0.1.1 _2025-09-07_

* Fix `align_chr` so that setting `format_numeric_character = FALSE` works. 
* `list_column` gains a `unite` parameter that joins list and text columns. 
* `add_star_column` no longer add stars if the column value is missing.

# apa7 0.1.0 _2025-08-19_

* Initial Cran Release

# apa7 0.0.0.9002

* removed apaquarto installation functions

# apa7 0.0.0.9001

* Added S7 objects for automatic column formats

# apa7 0.0.0.9000

* Initial development version




