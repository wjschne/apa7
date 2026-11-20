# apa7 0.2.0 _2026-10-18_

* New `apa_anova()` makes APA-style ANOVA source tables for `aov` and `lm` models (Type 1 sums of squares, or Types 2 and 3 via `car::Anova()`), repeated-measures `aov` models with `Error()` strata, `afex` models, and tables from `stats::anova()` and `car::Anova()`. Effect sizes (partial, non-partial, or generalized *η*^2^, *ω*^2^, or *ε*^2^) and their two-sided confidence intervals come from `effectsize`.
* `apa_parameters()` now returns the ANOVA table for `aov`, repeated-measures `aov`, and `afex` models. Before, `aov` models were treated as `lm` models and gave a coefficients table.
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




