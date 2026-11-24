## Submission

This is an update of apa7 from 0.2.0 to 0.3.0. It adds APA-style tables for
descriptive statistics (`apa_descriptives()`), reliability
(`apa_reliability()`), hypothesis tests (`apa_htest()`), estimated marginal
means, contrasts, and simple slopes (`apa_means()`, `apa_contrasts()`,
`apa_slopes()`), collinearity (`apa_collinearity()`), and meta-analyses
(`apa_subgroups()`, `apa_publication_bias()`, and methods for 'metafor'
models), and support for more models ('lavaan', 'glmmTMB', 'nlme',
multinomial, zero-inflated and hurdle, and generalized additive models).
Table notes can cite the articles that describe a method, with the
references in `inst/apa7.bib`. See NEWS.md for details, including the changes
that can affect existing tables.

## Test environments

* Local: Windows 11, R 4.6.2 (`R CMD check --as-cran`)
* win-builder: R-release and R-devel (TO DO)
* R-hub: Linux (Ubuntu, R-devel), macOS arm64 (R-devel), and Windows (R-devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

* New packages in Suggests ('marginaleffects', 'metadat', 'metafor', 'mgcv',
  'modelbased', 'nnet', 'pscl') are used conditionally: examples check
  `requireNamespace()` and tests call `skip_if_not_installed()`.

## Reverse dependencies

There are no reverse dependencies on CRAN.
