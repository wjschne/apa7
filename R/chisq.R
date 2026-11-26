#' Contingency table with a test of independence
#'
#' Makes an APA-style contingency table of two categorical variables: a row
#' for each level of `variable` and a column (or a pair of columns, with
#' percentages) for each level of `by`, with the counts in each cell. The
#' variables are tested for independence, and [apa_note()] writes the note,
#' with the result of the test and its effect size.
#'
#' Like the other apa7 tables, the result is a tibble that is displayed as an
#' APA-style table in Quarto and R Markdown documents (see [apa7_tbl]). Pass
#' it to [apa_flextable()] to change its appearance, or use
#' `apa_flextable(note = TRUE)` to put the note below it.
#'
#' Percentages are of each column's total (`percent = "column"`, the
#' default), so each group of `by` can be described on its own; of each row's
#' total (`"row"`); or of the whole sample (`"total"`). With `total = TRUE`
#' (default), a "Total" row and column give the marginal counts.
#'
#' The test is the chi-square test of independence ([stats::chisq.test()])
#' or Fisher's exact test ([stats::fisher.test()]). As in
#' [stats::chisq.test()], 2 × 2 tables use Yates's continuity correction
#' unless `correct = FALSE`. When an expected count is below 5, the
#' chi-square approximation can be poor, and the note says so. The effect
#' size is \eqn{\phi}{phi} for 2 × 2 tables and Cramér's *V* otherwise
#' (without the small-sample adjustment), from the effectsize package.
#'
#' Rows with a missing value of either variable are left out, and the note
#' gives their number.
#'
#' @param data data frame
#' @param variable variable whose levels are the rows of the table, unquoted
#'   or quoted (e.g., `smoking` or `"smoking"`)
#' @param by variable whose levels are the columns of the table, unquoted or
#'   quoted. If `data` has exactly two columns, `variable` and `by` can be
#'   left out: the first column is then `by`, and the second is `variable`
#'   (as in earlier versions of apa7).
#' @param percent percentages shown next to the counts: `"column"` (default)
#'   for percentages of each column's total, `"row"` for percentages of each
#'   row's total, `"total"` for percentages of the whole sample, or `"none"`
#'   for counts only
#' @param total if `TRUE` (default), a "Total" row and column are added
#' @param test `"chisq"` (default) for the chi-square test of independence,
#'   `"fisher"` for Fisher's exact test, or `"none"` for no test
#' @param correct if `TRUE` (default), the chi-square test of a 2 × 2 table
#'   uses Yates's continuity correction
#' @param effect_size `"auto"` (default) for \eqn{\phi}{phi} in 2 × 2 tables
#'   and Cramér's *V* otherwise, `"cramers_v"`, `"phi"` (2 × 2 tables only),
#'   or `"none"`
#' @param percent_digits number of decimal places of percentages
#' @param labels named character vector of labels for `variable` and `by`
#'   (e.g., `c(smoke = "Smoking")`). Variables without a label are named by
#'   their formatted names.
#' @param format_variable_names format the names of variables that have no
#'   label with the `Variable` column format
#' @param ... deprecated arguments of earlier versions of `apa_chisq()`
#'   (`note`, `suppress_warnings`, and the styling arguments of
#'   [apa_flextable()]). If any is given, a flextable is returned, as before,
#'   with a warning.
#'
#' @returns `apa7_tbl` (tibble)
#' @export
#' @examples
#' d <- data.frame(
#'   Sex = factor(psych::sat.act$gender, levels = 1:2, labels = c("Men", "Women")),
#'   Education = factor(psych::sat.act$education)
#' )
#' tbl <- apa_chisq(d, Education, by = Sex)
#' tbl
#' apa_note(tbl)
#'
#' # Percentages of each row, and no totals
#' apa_chisq(d, Education, by = Sex, percent = "row", total = FALSE)
#'
#' # Fisher's exact test instead of the chi-square test
#' apa_chisq(d[d$Education %in% c("0", "5"), ], Education, by = Sex, test = "fisher")
apa_chisq <- function(
  data,
  variable = NULL,
  by = NULL,
  percent = c("column", "row", "total", "none"),
  total = TRUE,
  test = c("chisq", "fisher", "none"),
  correct = TRUE,
  effect_size = c("auto", "cramers_v", "phi", "none"),
  percent_digits = 1,
  labels = NULL,
  format_variable_names = TRUE,
  ...
) {
  percent <- rlang::arg_match(percent)
  test <- rlang::arg_match(test)
  effect_size <- rlang::arg_match(effect_size)
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame, not {.obj_type_friendly {data}}.")
  }
  data <- as.data.frame(data)
  pick <- function(quo, arg) {
    out <- tryCatch(
      select_names(quo, data),
      error = function(e) {
        cli::cli_abort("{.arg {arg}} must be the name of a column of {.arg data}.", parent = e, call = NULL)
      }
    )
    if (length(out) > 1) {
      cli::cli_abort("{.arg {arg}} must be the name of one column of {.arg data}, not {length(out)}.", call = NULL)
    }
    out
  }
  variable <- pick(rlang::enquo(variable), "variable")
  by <- pick(rlang::enquo(by), "by")
  if (length(variable) == 0 && length(by) == 0) {
    if (ncol(data) != 2) {
      cli::cli_abort(c(
        "Give the two variables with {.arg variable} and {.arg by}.",
        i = "For example, {.code apa_chisq(data, smoking, by = sex)}. Without them, {.arg data} must have exactly 2 columns, not {ncol(data)}."
      ))
    }
    by <- colnames(data)[1]
    variable <- colnames(data)[2]
  } else if (length(variable) == 0 || length(by) == 0) {
    cli::cli_abort("Give both {.arg variable} (the rows) and {.arg by} (the columns).")
  }
  if (identical(variable, by)) {
    cli::cli_abort("{.arg variable} and {.arg by} must be different columns.")
  }

  label_of <- function(v) {
    if (!is.null(labels) && v %in% names(labels)) {
      return(labels[[v]])
    }
    if (format_variable_names) the$column_formats$Variable@formatter(v) else v
  }
  levels_of <- function(x) {
    if (is.factor(x)) levels(droplevels(x)) else sort(unique(as.character(x[!is.na(x)])))
  }

  x <- data[[variable]]
  g <- data[[by]]
  ok <- !is.na(x) & !is.na(g)
  n_dropped <- sum(!ok)
  row_levels <- levels_of(x[ok])
  col_levels <- levels_of(g[ok])
  if (length(row_levels) < 2 || length(col_levels) < 2) {
    cli::cli_abort(
      "{.var {variable}} and {.var {by}} each need at least 2 observed values to make a contingency table."
    )
  }
  tab <- table(
    factor(as.character(x[ok]), levels = row_levels),
    factor(as.character(g[ok]), levels = col_levels)
  )
  n_total <- sum(tab)

  # Test of independence
  is_2x2 <- all(dim(tab) == 2)
  result <- list(test = test, n = n_total)
  expected <- outer(rowSums(tab), colSums(tab)) / n_total
  if (test == "chisq") {
    ch <- suppressWarnings(stats::chisq.test(tab, correct = correct))
    result$statistic <- unname(ch$statistic)
    result$df <- unname(ch$parameter)
    result$p <- ch$p.value
    result$yates <- is_2x2 && isTRUE(correct)
    result$min_expected <- min(expected)
  } else if (test == "fisher") {
    result$p <- stats::fisher.test(tab, workspace = 2e7)$p.value
  }
  if (effect_size == "auto") {
    effect_size <- if (is_2x2) "phi" else "cramers_v"
  }
  if (effect_size == "phi" && !is_2x2) {
    cli::cli_abort(
      "{.code effect_size = \"phi\"} needs a 2 {cli::symbol$times} 2 table, not a {nrow(tab)} {cli::symbol$times} {ncol(tab)} table. Use {.val cramers_v} instead."
    )
  }
  if (effect_size != "none") {
    es <- switch(
      effect_size,
      cramers_v = effectsize::cramers_v(tab, adjust = FALSE, ci = NULL),
      phi = effectsize::phi(tab, adjust = FALSE, ci = NULL)
    )
    result$effect_size <- effect_size
    result$effect_size_value <- unname(es[[1]][1])
  }

  # Counts and percentages
  counts <- unclass(tab)
  dimnames(counts) <- list(row_levels, col_levels)
  if (total) {
    counts <- rbind(counts, Total = colSums(counts))
    counts <- cbind(counts, Total = rowSums(counts))
  }
  n_rows <- nrow(counts)
  n_cols <- ncol(counts)
  # Totals of the inner table, extended to the margins
  row_totals <- rowSums(counts[seq_along(row_levels), seq_along(col_levels), drop = FALSE])
  col_totals <- colSums(counts[seq_along(row_levels), seq_along(col_levels), drop = FALSE])
  if (total) {
    row_totals <- c(row_totals, Total = n_total)
    col_totals <- c(col_totals, Total = n_total)
  }
  percents <- switch(
    percent,
    column = 100 * sweep(counts, 2, col_totals, "/"),
    row = 100 * sweep(counts, 1, row_totals, "/"),
    total = 100 * counts / n_total,
    none = NULL
  )

  d <- tibble::tibble(.rows = n_rows)
  d[[label_of(variable)]] <- rownames(counts)
  for (j in seq_len(n_cols)) {
    group <- gsub("_", " ", colnames(counts)[j], fixed = TRUE)
    if (is.null(percents)) {
      d[[group]] <- as.integer(counts[, j])
    } else {
      d[[paste0(group, "_n")]] <- as.integer(counts[, j])
      # Aligned on the decimal point, with the same number of decimals
      d[[paste0(group, "_%")]] <- align_chr(unname(percents[, j]), accuracy = .1^percent_digits, format_integers = TRUE)
    }
  }

  attr(d, "no_format_columns") <- label_of(variable)
  if (total) {
    # A rule sets the Total row apart from the levels
    attr(d, "rule_above") <- "Total"
  }
  attr(d, "apa_note") <- list(
    table = "chisq",
    variable = label_of(variable),
    by = label_of(by),
    percent = percent,
    total = total,
    result = result,
    n_dropped = n_dropped
  )
  d <- as_apa7_tbl(d)

  # Earlier versions returned a flextable with the note below it
  dots <- list(...)
  if (length(dots) > 0) {
    d <- chisq_deprecated_flextable(d, dots)
  }
  d
}

#' Flextable for the deprecated arguments of apa_chisq()
#' @keywords internal
#' @noRd
chisq_deprecated_flextable <- function(d, dots, call = rlang::caller_env()) {
  if (is.null(names(dots)) || any(names(dots) == "")) {
    cli::cli_abort("Arguments in {.arg ...} must be named.", call = call)
  }
  old_args <- c(
    "note", "suppress_warnings", "row_title_column", "row_title_prefix",
    "row_title_sep", "row_title_align", "row_title_border",
    "left_column_padding", "cwidth", "cheight", "separate_headers",
    "apa_style", "font_family", "font_size", "text_color", "border_color",
    "border_width", "line_spacing", "horizontal_padding", "table_align",
    "layout", "table_width", "markdown", "markdown_header", "markdown_body",
    "auto_format_columns", "column_formats", "pretty_widths"
  )
  unknown <- setdiff(names(dots), old_args)
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown argument{?s} to {.fn apa_chisq}: {.arg {unknown}}.", call = call)
  }
  deprecated_args <- names(dots)
  cli::cli_warn(
    c(
      "{.fn apa_chisq} now returns a table that is displayed automatically, and its note comes from {.fn apa_note}. These arguments are deprecated: {.arg {deprecated_args}}.",
      i = "Pass the table to {.fn apa_flextable} to style it, with {.code note = TRUE} for the note."
    ),
    call = call,
    .frequency = "regularly",
    .frequency_id = "apa7_chisq_deprecated"
  )
  note <- if ("note" %in% names(dots)) dots$note else TRUE
  if (length(note) == 1 && is.na(note)) {
    note <- NULL
  }
  styling <- dots[setdiff(names(dots), c("note", "suppress_warnings"))]
  do.call(apa_flextable, c(list(d, note = note), styling))
}

#' @keywords internal
#' @noRd
chisq_note_sentences <- function(record) {
  result <- record$result
  sentences <- switch(
    record$percent,
    column = paste0("Values are counts (*n*) and percentages of each column (each group of ", record$by, ")."),
    row = paste0("Values are counts (*n*) and percentages of each row (each level of ", record$variable, ")."),
    total = paste0(
      "Values are counts (*n*) and percentages of the whole sample (*N* = ",
      format(result$n, big.mark = ","), ")."
    ),
    none = "Values are counts."
  )
  es <- NULL
  if (!is.null(result$effect_size)) {
    symbol <- if (result$effect_size == "phi") "*&phi;*" else "Cram\u00e9r's *V*"
    es <- paste0(
      symbol, " = ",
      signs::signs(result$effect_size_value, accuracy = .01, trim_leading_zeros = TRUE)
    )
  }
  if (result$test == "chisq") {
    sentences <- c(
      sentences,
      paste0(
        "Chi-square test of independence: *&chi;*^2^(",
        result$df, ", *N* = ", format(result$n, big.mark = ","), ") = ",
        scales::number(result$statistic, accuracy = .01, big.mark = ","), ", ",
        apa_p(result$p, inline = TRUE),
        if (!is.null(es)) paste0(", ", es),
        "."
      ),
      if (isTRUE(result$yates)) "The test uses Yates's continuity correction.",
      if (isTRUE(result$min_expected < 5)) {
        paste0(
          "The smallest expected count is ",
          scales::number(result$min_expected, accuracy = .01),
          ", below 5, so the chi-square approximation may be inaccurate."
        )
      }
    )
  } else if (result$test == "fisher") {
    sentences <- c(
      sentences,
      paste0(
        "Fisher's exact test: ",
        apa_p(result$p, inline = TRUE),
        if (!is.null(es)) paste0(", ", es),
        "."
      )
    )
  } else if (!is.null(es)) {
    sentences <- c(sentences, paste0(es, "."))
  }
  if (isTRUE(record$n_dropped > 0)) {
    sentences <- c(
      sentences,
      paste0(
        "Cases with a missing value of ", record$variable, " or ", record$by,
        " (*n* = ", format(record$n_dropped, big.mark = ","), ") are not included."
      )
    )
  }
  sentences
}
