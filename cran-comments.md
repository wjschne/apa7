## Submission

This is an update of apa7 from 0.1.3 to 0.2.0. It adds APA-style tables for
ANOVA (`apa_anova()`), factor analysis (`apa_loadings()`, `apa_factor_cor()`),
and more regression models, and a function that writes table notes
(`apa_note()`). See NEWS.md for details.

## CRAN check results for 0.1.3

The r-devel Linux flavors warn "replacing previous import 'S7:::=' by
'rlang:::='", because S7 1.0.0 now exports `:=`. This version no longer
imports `:=` from rlang. It installs and loads without warnings, and its
tests pass, with S7 1.0.0.

## Test environments

* Local: Windows 11, R 4.6.2 (`R CMD check --as-cran`, including the PDF manual)
* win-builder: R-release (R 4.6.2) and R-devel (Windows Server 2022)
* R-hub: Linux (Ubuntu, R-devel), macOS arm64 (R-devel), and Windows (R-devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

* The first win-builder run reported LaTeX errors in the PDF manual (Unicode
  Greek letters in the Rd files) and a NOTE that the `apa_parameters` examples
  took more than 10 seconds. Both are fixed: the Rd files use `\eqn{}` and are
  ASCII, and the two slowest examples are in `\donttest{}`.
* win-builder R-devel now reports one WARNING: "Located more than one 'weave'
  output file ... 'apa7.html', 'apa7.pdf' (372482 bytes)". The tarball has no
  PDF: `inst/doc` holds only `apa7.R`, `apa7.html`, and `apa7.qmd`, and
  `build/vignette.rds` lists `apa7.html` as the only output. An `apa7.pdf` was
  in an earlier upload to win-builder R-devel that day and seems to have been
  left in its check directory: it has the same size in every run, while the
  HTML size changes. The same tarball gets Status OK on win-builder R-release,
  with no vignette warning.

## Reverse dependencies

There are no reverse dependencies on CRAN.
