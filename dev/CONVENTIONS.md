# apa7 Conventions

The rules apa7 follows, so that new functions look and behave like the
existing ones. Each rule has an ID (e.g., ARG-4) that commits, NEWS
entries, and reviews can refer to.

**Keeping this document current**

* A change that sets or changes a convention updates this file in the same
  commit.
* Rules marked *Checked* are enforced by `tests/testthat/test-conventions.R`.
  Others rely on review. Prefer adding a check over adding prose.
* Before each CRAN release, review this file against the code (see
  `dev/priorities.md`).
* Known departures from a rule are listed under [Open
  inconsistencies](#open-inconsistencies) until they are fixed or accepted
  as exceptions.

## New-function checklist

For a new table function (or a new method of an existing one):

- [ ] Arguments follow [ARG](#arguments): first argument, shared names, defaults
- [ ] Returns an `apa7_tbl` with a note record ([RET](#return-values))
- [ ] Column names have entries in `column_formats()` where the column is new ([TAB-5](#tables))
- [ ] Note sentences cite methods through `inst/apa7.bib` ([NOTE](#table-notes-and-citations)), and the help page lists them under `@references` ([DOC-5](#documentation))
- [ ] Help page: description, Details for anything with several steps, examples that run ([DOC](#documentation))
- [ ] Listed in the `apa7_tbl` help page, in `apa_note()`'s error message, and in `_pkgdown.yml`
- [ ] Tests, including note text and missing-package skips ([TEST](#tests))
- [ ] NEWS entry under the right heading ([NEWS](#news))

---

## Arguments

**ARG-1: The first argument names what the function takes.**
`fit` for a fitted model, a list of models, or the output of an analysis
(e.g., `parameters::n_factors()`); `data` for a data frame of raw data;
`x` for a vector, a flextable, an `apa7_tbl`, or a matrix. Table functions
take `fit` or `data`, never `x`, even when they also accept other input
(e.g., `apa_n_factors()` runs `parameters::n_factors()` on a data frame).
*Why:* the name tells users what to pipe in. *Checked.*

**ARG-2: The same idea has the same name, and the same default, everywhere.**

| Idea | Name | Default |
|---|---|---|
| Columns that get significance stars | `starred` | `NA` (none) |
| Columns that are bolded when *p* ≤ .05 | `bolded` | `NA` (none) |
| *p*-value thresholds for stars, largest first | `alpha` | `c(.05, .01, .001)` |
| Confidence level | `ci` | `.95` |
| Decimals shown by a table function | `digits` | `2` |
| Rounding of a formatter (e.g., `.01`) | `accuracy` | `NULL` (the setting in `apa7_defaults()`) |
| Column formats | `column_formats` | `NULL` (the setting in `apa7_defaults()`) |
| Table width | `table_width` | `NULL` (chunk option, then `apa7_defaults()`) |
| Grouping variable | `by` | `NULL` |
| Reformat snake_case variable names for display | `format_variable_names` | `TRUE` |
| Flag values past a cutoff | `flag` | `NULL` (no flag) |
| Model fit statistics to show | `metrics` | per model class |

*Why:* 0.3.0 renamed `p_value`, `x`, and `d` because the same idea had
different names. *Checked* (banned names).

**ARG-3: `digits` versus `accuracy`.**
Table functions that round their own output take `digits` (a count of
decimals). Formatters and functions that pass a value to `signs::signs()`
take `accuracy` (a rounding unit). A function does not take both.

**ARG-4: `NULL` means "use the setting", and `NA` means "none".**
An argument whose default comes from `apa7_defaults()` (fonts, sizes,
widths, column formats, accuracy) defaults to `NULL`. An argument that
selects columns defaults to `NA` when none is the default.

**ARG-5: Choices are character vectors resolved with `match.arg()`.**
The first element is the default (e.g., `effect_size = c("eta", "omega",
"epsilon")`). Logical arguments are reserved for on/off switches.

**ARG-6: Arguments after `...`.**
Arguments come after `...` only when `...` holds a tidyselect selection
(e.g., `add_star_column()`), a set of models (`apa_performance_comparison()`),
or arguments passed to another package (e.g., `apa_means()`), or when the
argument is deprecated (ARG-7).

**ARG-7: Renaming an argument.**
The old name moves after `...` (or to the end, if the function has no
`...`), keeps working for at least one release, and
warns with `warn_renamed_arg(old, new, fn)` (shown regularly, not every
call). The rename is listed in NEWS under "Changes that can affect existing
tables", and tested. *Checked.*

**ARG-8: Argument names are snake_case.**
Exceptions: `.before` and `.data` (tidyverse conventions),
`launch.browser` (passed to shiny), and `NA_value` (as R spells `NA`). *Checked.*

**ARG-9: Column arguments take tidyselect selections.**
Unquoted names, quoted names, and helpers all work, and named character
vectors are accepted with their names ignored.

## Return values

**RET-1: Table functions return an `apa7_tbl`.**
An `apa7_tbl` is a tibble that prints as a tibble in the console and is
rendered by `apa_flextable()` in knitr and Quarto. Functions that build a
flextable directly are the exception, not the model (see the open
inconsistencies). *Checked* (documented return value).

**RET-2: The note record travels with the table.**
Each table function stores what its note needs in the `"apa_note"`
attribute (a list with a `table` element naming the kind of table).
`apa_note()` turns the record into sentences, and `apa_flextable(note =
TRUE)` places them. Table functions do not write note text themselves.

## Tables

**TAB-1: Underscores make header levels.**
`Model 1_b` becomes the column spanner "Model 1" over the deckered head
"b". Column names should not contain underscores for any other reason
once a table reaches `apa_flextable()`.

**TAB-2: Helper columns are marked by name.**
Names starting with `apa7breakcolumn` are break columns; names ending in
`apa7starcolumn` and `apa7listcolumn` are star and list columns.
`apa_style()` and `pretty_widths()` find them by these patterns.

**TAB-3: The first column holds row labels.**
It is left-aligned, and the columns of numbers are centered. `Variable` and
`Predictor` are the preferred names.

**TAB-4: Structure is passed in attributes.**
`"no_format_columns"` (columns that `apa_format_columns()` leaves alone)
and `"rule_above"` (rows that get a rule above them).

**TAB-5: Every standard statistic has a column format.**
A new statistic column gets an entry in `column_formats()` with a markdown
header, a LaTeX header, a formatter, and, if the header is an
abbreviation, an `abbreviation` that `apa_note()` defines.

**TAB-6: Model-specific design** (decided when the methods were built):

* *lavaan*: path labels use arrows ("Visual → x1", "Speed ←
  Visual", "Visual ↔ Textual"), not lavaan operators. Parameter
  tables show standardized estimates by default (*&beta;*, *SE*, *p*), with
  unstandardized *B* on request. Fit is reported with `apa_performance()`,
  parameters with `apa_parameters()` and component headings, CFA loadings
  with `apa_loadings()`, and nested comparisons with
  `apa_performance_comparison()`.
* *Mixed models*: "Fixed effects" and "Random effects" heading rows, random
  effects as *SD*s and correlations by default, one subheading per cluster.

## Formatting values

**FMT-1: Numbers are decimal-aligned.**
Formatters use `align_chr()`, which pads with figure spaces (or, by
default, to equal rendered width in the table's font).

**FMT-2: Negative numbers use a true minus sign** (U+2212), from
`signs::signs()`.

**FMT-3: No leading zero for statistics that cannot exceed 1** (*p*,
correlations, standardized coefficients, proportions of variance), following
APA 7 section 6.36. Their formats use `the$trim_leading_zero`.

**FMT-4: *p*-values** are formatted by `apa_p()`: three decimals at most,
no leading zero, and "<.001" below .001.

**FMT-5: A result is significant when *p* ≤ α.** Stars mark *p* ≤ each
value of `alpha`, as superscripts, and bold marks *p* ≤ .05. Every note
that states a threshold says ≤ (e.g., the star note from
`apa_p_star_note()` and the Johnson–Neyman sentence).
*Why:* *p* = .05 is significant at α = .05, and a note must describe
exactly what the table marks.

**FMT-6: Whole numbers get no decimals** (e.g., degrees of freedom and
sample sizes), unless the column's format says otherwise.

**FMT-7: Headers are markdown.** Statistical symbols are italic (`*p*`,
`*SE*`), Greek letters use HTML entities (`*&beta;*`), and superscripts and
subscripts use `^` and `~`.

## Table notes and citations

**NOTE-1: Notes are written from the record, in full sentences.** Each
kind of table has a `*_note_sentences()` function in `R/notes.R`.

**NOTE-2: Methods are cited with `cite(key)`**, where `key` is an entry in
`inst/apa7.bib`. Citations become Pandoc citations or text depending on the
document (see `apa7_bib()`).

**NOTE-3: Abbreviations are defined from the column formats** (TAB-5), not
written into each note.

**NOTE-4: Notes are single-spaced**, even in double-spaced tables.

## Documentation

**DOC-1: Every argument is documented** with its accepted values, units
(points for sizes, inches for page widths), and default.

**DOC-2: The description says what the function does in a sentence or
two.** Behavior with several steps goes in a Details section as a numbered
list of steps in the order the code runs them (see `apa_flextable()` and
`apa_style()`).

**DOC-3: Examples run without `library()` calls**, use `pkg::fun()` for
other packages, and are guarded with `requireNamespace()` for Suggests.
They use realistic labels, not placeholders. *Checked* (every exported
function has examples).

**DOC-4: Non-ASCII characters are written as `\u` escapes in R code**
(R CMD check warns otherwise). Roxygen comments may contain UTF-8.

**DOC-5: A function whose notes cite a method lists that reference under
`@references`**, in APA style with a DOI where one exists. *Checked.*

**DOC-6: Articles and help pages agree.** When an article describes a
function's behavior step by step (e.g., *Building Tables by Hand*), the
help page's Details follow the same steps.

## Tests

**TEST-1: Each argument and each note sentence has a test.**

**TEST-2: Suggested packages are skipped when missing**
(`skip_if_not_installed()`).

**TEST-3: Font-dependent assertions are skipped when the font is missing**
(`skip_if_not(apa7:::font_available("Times New Roman"))`, or an `if` around
the single assertion). Linux CI runners do not have Times New Roman.

**TEST-4: Check cadence.** Targeted tests after each change; the full
`R CMD check --as-cran` before a push or a CRAN submission.

## Code

**CODE-1:** tidyverse style, roxygen2 with markdown.

**CODE-2:** Errors and warnings use `cli::cli_abort()` and
`cli::cli_warn()`, with `{.arg}` and `{.fn}` markup.

**CODE-3:** Packages used unconditionally are in Imports; model packages
are in Suggests and checked with `rlang::check_installed()` or
`requireNamespace()`.

## NEWS

**NEWS-1:** Entries go under these headings, in this order: "Changes that
can affect existing tables", "New table functions", "New models", "Table
notes and citations", "Settings", "Other improvements and bug fixes".

**NEWS-2:** An entry says what changed, what happened before, and how to
get the old behavior if it changed.

---

## Open inconsistencies

Found while drafting this document (2026-11-26). Each needs a decision:
fix it, or record it as an exception to the rule.

| # | Rule | Departure | Suggested resolution |
|---|---|---|---|
| 2 | RET-1 | `apa_cor()` returns a flextable by default (`output = "flextable"`), takes `note` and styling arguments, and writes its own note. | Make it return an `apa7_tbl` with a note record, keeping `output = "flextable"` for one release |
| 3 | ARG-2 | `apa_cor()` uses `star_significant` and `bold_significant` instead of `starred` and `bolded`. | Exception: they are switches over every correlation, not column selections |
| 4 | ARG-2 | Variable relabeling is `labels` (`apa_chisq()`, `apa_descriptives()`), `variable_labels` (`column_formats()`), and `factor_names` (`apa_loadings()`). | Decide whether `factor_names` is a different idea; align the other two |
| 5 | ARG-5 | `test` is logical in `apa_descriptives()` but a choice in `apa_chisq()` and `apa_htest()`. | Exception, or accept `test = "auto"` style values |
| 7 | ARG-6 | `pivot_wider_name_first()` accepts `names_prefix`, `names_sep`, and `names_vary` but ignores them. | Remove them (with deprecation) or honor them |
| 8 | TAB-1 | `apa_format_columns()` looks for `Model 1_low`/`Model 1_high` rather than `Model 1_CI_low`/`Model 1_CI_high` for prefixed intervals (untested). | Test, then fix |
| 9 | CODE-2 | 28 `stop()` and 3 `warning()` calls remain beside 125 `cli::cli_abort()` calls. | Convert when the code is next touched |
