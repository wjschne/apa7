#' Table of hypothesis tests from htest objects
#'
#' @param x an `htest` object or a list of them
#' @keywords internal
#' @noRd
htest_from_objects <- function(
  x,
  effect_size = NULL,
  ci = 0.95,
  starred = NA,
  bolded = NA,
  column_formats = NULL
) {
  tests <- if (inherits(x, "htest")) list(x) else x
  kinds <- purrr::map_chr(tests, htest_kind)
  if (length(unique(kinds)) > 1) {
    cli::cli_abort(c(
      "All tests in a table must be of the same kind.",
      i = "The tests include {.and {gsub('*', '', htest_kind_labels[unique(kinds)], fixed = TRUE)}}."
    ))
  }
  kind <- kinds[1]
  test <- sub("_.*", "", kind)
  effect_size <- htest_object_effect_size(kind, effect_size)
  if (!is.numeric(ci) || length(ci) != 1 || is.na(ci) || ci <= 0 || ci >= 1) {
    cli::cli_abort("{.arg ci} must be a number between 0 and 1.")
  }
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }

  labels <- unname(purrr::map_chr(tests, "data.name"))
  if (!is.null(names(tests))) {
    named <- !is.na(names(tests)) & names(tests) != ""
    labels[named] <- unname(names(tests)[named])
  }
  rows <- purrr::map(tests, \(h) htest_object_row(h, kind, effect_size, ci))
  d <- dplyr::bind_rows(rows)
  d <- tibble::add_column(d, Parameter = labels, .before = 1)

  formatted <- htest_format(
    d,
    test = test,
    ci = ci,
    descriptives = TRUE,
    starred = starred,
    bolded = bolded,
    column_formats = column_formats
  )
  d_new <- formatted$data
  record <- attr(d_new, "apa_note", exact = TRUE)
  null_values <- unique(purrr::map_dbl(tests, \(h) {
    if (is.null(h$null.value)) NA_real_ else unname(h$null.value[1])
  }))
  attr(d_new, "apa_note") <- htest_note_record(
    test = test,
    n_tests = length(tests),
    one_sample = kind == "t_one",
    paired = kind == "t_paired",
    method_names = unique(purrr::map_chr(tests, "method")),
    alternative = unique(purrr::map_chr(tests, \(h) {
      if (is.null(h$alternative)) "two.sided" else h$alternative
    })),
    effect_size = formatted$es_col,
    effect_size_header = formatted$es_header,
    abbreviations = record$abbreviations,
    starred = record$starred,
    bolded = record$bolded,
    from_object = TRUE,
    kind = kind,
    null_values = null_values[!is.na(null_values)]
  )
  as_apa7_tbl(labels_formatted(d_new))
}

#' Is x a list of htest objects?
#' @keywords internal
#' @noRd
is_htest_list <- function(x) {
  is.list(x) &&
    !is.data.frame(x) &&
    !inherits(x, "htest") &&
    length(x) > 0 &&
    all(purrr::map_lgl(x, \(h) inherits(h, "htest")))
}

#' The kind of test an htest object holds, from its method
#' @keywords internal
#' @noRd
htest_kind <- function(x) {
  m <- x$method
  kind <- if (grepl("Paired t-test", m, fixed = TRUE)) {
    "t_paired"
  } else if (grepl("One Sample t-test", m, fixed = TRUE)) {
    "t_one"
  } else if (grepl("Welch Two Sample t-test", m, fixed = TRUE)) {
    "t_welch"
  } else if (grepl("Two Sample t-test", m, fixed = TRUE)) {
    "t_student"
  } else if (grepl("Wilcoxon rank sum", m, fixed = TRUE)) {
    "wilcoxon_ranksum"
  } else if (grepl("Wilcoxon signed rank", m, fixed = TRUE)) {
    "wilcoxon_signed"
  } else if (grepl("Pearson's product-moment correlation", m, fixed = TRUE)) {
    "cor_pearson"
  } else if (grepl("Spearman's rank correlation", m, fixed = TRUE)) {
    "cor_spearman"
  } else if (grepl("Kendall's rank correlation", m, fixed = TRUE)) {
    "cor_kendall"
  } else if (grepl("Chi-squared test for given probabilities", m, fixed = TRUE)) {
    "chisq_gof"
  } else if (grepl("Pearson's Chi-squared test", m, fixed = TRUE)) {
    "chisq_independence"
  } else {
    NA_character_
  }
  if (is.na(kind)) {
    cli::cli_abort(c(
      "{.fn apa_htest} does not support this test: {.val {trimws(m)}}.",
      i = "Supported tests are from {.fn stats::t.test}, {.fn stats::wilcox.test}, {.fn stats::cor.test}, and {.fn stats::chisq.test}."
    ))
  }
  kind
}

#' Check and resolve effect_size for htest objects
#'
#' Effect sizes are computed from what the object keeps: the test statistic
#' and its degrees of freedom (t tests) or the observed table (chi-square
#' tests of independence). Wilcoxon tests keep neither the group sizes nor
#' the data, so they have no effect size.
#' @keywords internal
#' @noRd
htest_object_effect_size <- function(kind, effect_size) {
  allowed <- switch(
    kind,
    t_welch = ,
    t_student = ,
    t_one = "cohens_d",
    t_paired = c("d_z", "cohens_d"),
    chisq_independence = c("cramers_v", "phi", "cohens_w"),
    character(0)
  )
  if (is.null(effect_size)) {
    return(if (length(allowed) > 0) allowed[1] else "none")
  }
  if (!is.character(effect_size) || length(effect_size) != 1) {
    cli::cli_abort("{.arg effect_size} must be a single string or {.code NULL}.")
  }
  if (effect_size == "none") {
    return("none")
  }
  kind_label <- gsub("*", "", htest_kind_labels[[kind]], fixed = TRUE)
  if (length(allowed) == 0) {
    cli::cli_abort(
      "No effect size can be computed from the results of a {kind_label}. Use {.code effect_size = \"none\"}, or give {.fn apa_htest} the data."
    )
  }
  if (!(effect_size %in% allowed)) {
    cli::cli_abort(
      "For the results of a {kind_label}, {.arg effect_size} must be {.or {.val {c(allowed, 'none')}}}, not {.val {effect_size}}."
    )
  }
  if (kind == "t_paired") "d_z" else effect_size
}

#' One row of a table of htest objects
#' @keywords internal
#' @noRd
htest_object_row <- function(x, kind, effect_size, ci) {
  stat <- unname(x$statistic)
  df <- if (is.null(x$parameter)) NA_real_ else unname(x$parameter[1])
  row <- switch(
    sub("_.*", "", kind),
    t = tibble::tibble(t = stat, df_error = df, p = x$p.value),
    wilcoxon = {
      r <- tibble::tibble(p = x$p.value)
      r[[if (kind == "wilcoxon_ranksum") "W" else "V"]] <- stat
      r[c(2, 1)]
    },
    cor = {
      est_col <- switch(kind, cor_pearson = "r", cor_spearman = "rho", cor_kendall = "tau")
      r <- tibble::tibble(.rows = 1)
      if (kind == "cor_pearson") {
        r$n <- df + 2
      }
      r[[est_col]] <- unname(x$estimate)
      if (!is.null(x$conf.int)) {
        r$CI_low <- x$conf.int[1]
        r$CI_high <- x$conf.int[2]
      }
      if (kind == "cor_pearson") {
        r$t <- stat
        r$df_error <- df
      }
      r$p <- x$p.value
      r
    },
    chisq = tibble::tibble(
      N = sum(x$observed),
      Chi2 = stat,
      df = df,
      p = x$p.value
    )
  )
  if (effect_size == "none") {
    return(row)
  }

  es <- if (kind %in% c("t_one", "t_paired")) {
    # d = t / sqrt(n) for one sample (or the differences of paired
    # variables). effectsize divides by sqrt(df), so rescale to sqrt(n).
    e <- as.data.frame(effectsize::t_to_d(
      stat, df,
      paired = TRUE, ci = ci, alternative = "two.sided"
    ))
    scale <- sqrt(df / (df + 1))
    tibble::tibble(d = e$d * scale, CI_low = e$CI_low * scale, CI_high = e$CI_high * scale)
  } else if (startsWith(kind, "t_")) {
    # Two groups: d = 2t / sqrt(df), which assumes equal group sizes
    e <- as.data.frame(effectsize::t_to_d(stat, df, ci = ci, alternative = "two.sided"))
    tibble::tibble(d = e$d, CI_low = e$CI_low, CI_high = e$CI_high)
  } else {
    tab <- x$observed
    switch(
      effect_size,
      cramers_v = htest_es(effectsize::cramers_v, tab, adjust = FALSE, ci = ci),
      phi = {
        if (!all(dim(tab) == 2)) {
          cli::cli_abort(
            "{.code effect_size = \"phi\"} needs 2 {cli::symbol$times} 2 tables. Use {.val cramers_v} instead."
          )
        }
        htest_es(effectsize::phi, tab, adjust = FALSE, ci = ci)
      },
      cohens_w = htest_es(effectsize::cohens_w, tab, ci = ci)
    )
  }
  if ("d" %in% colnames(es)) {
    colnames(es)[1] <- if (kind == "t_paired") "d_z" else "Cohens_d"
  }
  dplyr::bind_cols(row, es[setdiff(colnames(es), "CI")])
}

#' Names of the kinds of tests
#' @keywords internal
#' @noRd
htest_kind_labels <- c(
  t_welch = "Welch's *t* test",
  t_student = "Student's *t* test",
  t_one = "one-sample *t* test",
  t_paired = "paired *t* test",
  wilcoxon_ranksum = "Wilcoxon rank-sum test",
  wilcoxon_signed = "Wilcoxon signed-rank test",
  cor_pearson = "Pearson correlation test",
  cor_spearman = "Spearman rank correlation test",
  cor_kendall = "Kendall rank correlation test",
  chisq_independence = "chi-square test of independence",
  chisq_gof = "chi-square goodness-of-fit test"
)

#' Note sentences for tables made from htest objects
#' @keywords internal
#' @noRd
htest_object_sentences <- function(record) {
  one <- isTRUE(record$n_tests == 1)
  label <- htest_kind_labels[[record$kind]]
  possessive <- grepl("'s ", label, fixed = TRUE)
  sentences <- if (one) {
    paste0("The result is from ", if (!possessive) "a ", label, ".")
  } else {
    plural <- if (record$kind == "chisq_independence") {
      "chi-square tests of independence"
    } else {
      paste0(label, "s")
    }
    paste0("Results are from ", plural, ".")
  }
  if (any(grepl("continuity correction", record$method_names, fixed = TRUE))) {
    sentences <- c(sentences, "2 \u00D7 2 tables use Yates's continuity correction.")
  }

  # Hypothesized values other than 0
  nulls <- record$null_values
  if (startsWith(record$kind, "t_") || startsWith(record$kind, "wilcoxon_")) {
    nulls <- nulls[nulls != 0]
    if (length(nulls) > 0) {
      what <- if (record$kind %in% c("t_one")) {
        "mean"
      } else if (record$kind == "wilcoxon_signed") {
        "location"
      } else {
        "difference"
      }
      values <- sub("-", "\u2212", format(nulls, trim = TRUE), fixed = TRUE)
      sentences <- c(
        sentences,
        paste0("The hypothesized ", what, " is ", or_list(values), ".")
      )
    }
  }

  alternative <- setdiff(record$alternative, "two.sided")
  if (length(alternative) > 0) {
    sentences <- c(
      sentences,
      paste0(
        if (one) "The *p*-value is one-sided (alternative: " else "*p*-values are one-sided (alternative: ",
        paste(gsub(".", " ", alternative, fixed = TRUE), collapse = ", "),
        ")."
      )
    )
  }

  es <- record$effect_size_header
  if (length(es) > 0) {
    how <- switch(
      record$kind,
      t_welch = ,
      t_student = paste0(
        es,
        " is approximated from *t* and its degrees of freedom (2*t*/&radic;*df*), which assumes groups of equal size."
      ),
      t_one = paste0(es, " is computed from *t* as *t*/&radic;*n*."),
      t_paired = paste0(
        es,
        " is computed from *t* as *t*/&radic;*n*: the mean difference divided by the standard deviation of the differences."
      ),
      NULL
    )
    sentences <- c(sentences, how)
  }
  if (startsWith(record$kind, "wilcoxon_")) {
    sentences <- c(
      sentences,
      "No effect size is shown, because the test results do not include the sample sizes."
    )
  }

  sentences <- c(sentences, abbreviation_sentence(record$abbreviations))
  c(sentences, probability_note_sentences(record$starred, record$bolded))
}
