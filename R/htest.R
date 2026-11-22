#' Format hypothesis tests in APA style
#'
#' Runs *t* tests, Wilcoxon tests, correlation tests, or chi-square tests of
#' independence on the variables in a data frame and makes a table with one row
#' per outcome variable: descriptive statistics, the test statistic, its
#' degrees of freedom, the *p*-value, and an effect size with its confidence
#' interval. Tests come from the stats package (e.g., [stats::t.test()]), and
#' effect sizes are computed from the data with the effectsize package (e.g.,
#' [effectsize::cohens_d()]).
#'
#' @param data data frame, or test results already computed: an `htest`
#'   object from [stats::t.test()], [stats::wilcox.test()], [stats::cor.test()],
#'   or [stats::chisq.test()], or a list of such objects of the same kind
#'   (named lists label the rows). See **Test results**.
#' @param formula two-sided formula with outcome variables on the left and a
#'   grouping or predictor variable on the right. Several outcomes are joined
#'   with `+` (e.g., `mpg + hp ~ am`), and each gets its own row. For one-sample
#'   *t* tests and Wilcoxon signed-rank tests, use `1` on the right (e.g.,
#'   `mpg ~ 1`). For paired tests, put each pair of variables in `Pair()` with
#'   `1` on the right (e.g., `Pair(pre, post) ~ 1`, or
#'   `Pair(anx1, anx2) + Pair(dep1, dep2) ~ 1` for several pairs).
#' @param test type of test:
#'
#'   * `"t"`: *t* test comparing the means of two groups (Welch's test unless
#'     `var_equal = TRUE`), a paired *t* test, or a one-sample *t* test
#'     comparing a mean with `mu`
#'   * `"wilcoxon"`: Wilcoxon rank-sum test comparing two groups, or a Wilcoxon
#'     signed-rank test of paired variables or of a variable compared with `mu`
#'   * `"cor"`: correlation test of each outcome with the variable on the right
#'   * `"chisq"`: chi-square test of independence of each outcome with the
#'     variable on the right
#' @param effect_size effect size to display. If `NULL` (default), the effect
#'   size is Cohen's *d* (`"cohens_d"`) for *t* tests, the rank-biserial
#'   correlation (`"rank_biserial"`) for Wilcoxon tests, and Cramér's *V*
#'   (`"cramers_v"`) for chi-square tests. *t* tests can also show Hedges' *g*
#'   (`"hedges_g"`), and chi-square tests can show \eqn{\phi}{phi} (`"phi"`, for
#'   2 \eqn{\times}{x} 2 tables) or Cohen's *w* (`"cohens_w"`). Use `"none"` for
#'   no effect size. Correlation tests show the correlation itself.
#'
#'   For paired *t* tests, Cohen's *d* is *d*~av~ (`"d_av"`), which divides the
#'   mean difference by the average of the two variables' standard deviations,
#'   so it is comparable to *d* for two independent groups. Other choices are
#'   *d*~z~ (`"d_z"`, divided by the standard deviation of the differences) and
#'   *d*~rm~ (`"d_rm"`, *d*~z~ corrected for the correlation between the
#'   variables), and Hedges' *g* versions with a small-sample correction
#'   (`"g_av"`, `"g_z"`, `"g_rm"`; `"hedges_g"` is `"g_av"`). These come from
#'   [effectsize::repeated_measures_d()].
#' @param ci confidence level of the effect size interval (for correlation
#'   tests, the interval of Pearson's *r*). Intervals are two-sided.
#' @param var_equal if `TRUE`, two-group *t* tests assume equal variances
#'   (Student's *t* test), and Cohen's *d* uses the pooled standard deviation.
#'   If `FALSE` (default), Welch's *t* test is used, and Cohen's *d* uses the
#'   average of the two groups' variances.
#' @param mu value that one-sample tests compare each variable with (for
#'   two-group tests, the hypothesized difference in location)
#' @param method correlation coefficient: `"pearson"` (default), `"spearman"`,
#'   or `"kendall"`
#' @param descriptives if `TRUE` (default), the table shows each group's mean
#'   and standard deviation (*t* tests) or median (Wilcoxon tests), or each
#'   variable's sample size (correlation and chi-square tests).
#' @param pair_labels for paired tests, labels of the first and second
#'   variable of each pair, used as the headings above their descriptive
#'   statistics (e.g., `c("Pretest", "Posttest")`). If `NULL` (default), the
#'   headings are the variable names when there is one pair, and "First" and
#'   "Second" otherwise.
#' @param starred columns to star with significant *p*-values (e.g., `"t"`)
#' @param bolded columns to bold, if significant
#' @param column_formats column_formats object to format columns. If NULL, the
#'   default column_formats is used.
#' @param ... further arguments passed to the test function (e.g.,
#'   `alternative = "greater"`, or `correct = FALSE` for [stats::chisq.test()])
#'
#' @details
#' Each test uses the rows in which both the outcome and the grouping (or
#' predictor) variable are observed. For two-group tests, the grouping
#' variable must have exactly two observed values, and differences are the
#' first group (in factor level order) minus the second. Paired tests use the
#' rows in which both variables of the pair are observed, and differences are
#' the first variable minus the second.
#'
#' Effect sizes are computed from the data rather than from the test
#' statistic, so Cohen's *d* for Welch's test is exact rather than
#' approximated from *t*.
#'
#' Use [apa_note()] to write a table note that says which tests were done, how
#' the effect size was computed, and what the abbreviations stand for.
#'
#' @section Test results:
#' `htest` objects keep the test statistic, its degrees of freedom, and the
#' *p*-value, but not the data, so `formula` and the arguments that control
#' the tests are not used, and effect sizes are computed from what the
#' objects keep:
#'
#' * One-sample and paired *t* tests: Cohen's *d* (*d*~z~ for paired tests)
#'   is exactly \eqn{t/\sqrt{n}}{t / sqrt(n)}, with an exact interval from the
#'   noncentral *t* distribution.
#' * Two-sample *t* tests: Cohen's *d* is approximated as
#'   \eqn{2t/\sqrt{df}}{2t / sqrt(df)} ([effectsize::t_to_d()]), which assumes
#'   groups of equal size, and the note says so. For exact values, give
#'   `apa_htest()` the data instead.
#' * Correlation tests: the correlation and, for Pearson's *r*, its interval
#'   and the sample size.
#' * Chi-square tests of independence: Cramér's *V*, \eqn{\phi}{phi}, or
#'   Cohen's *w*, computed from the observed table that the object keeps.
#' * Wilcoxon tests and chi-square goodness-of-fit tests: no effect size,
#'   because the objects do not keep the sample sizes.
#'
#' The rows are labeled by the list's names or, if unnamed, by the objects'
#' data names (e.g., "mpg by am"). The table has no descriptive statistics.
#'
#' @return `apa7_tbl` (tibble)
#' @export
#'
#' @examples
#' # Welch's t tests with Cohen's d
#' d <- mtcars
#' d$am <- factor(d$am, labels = c("Automatic", "Manual"))
#' apa_htest(d, mpg + hp + qsec ~ am)
#'
#' # Student's t test with Hedges' g
#' apa_htest(d, mpg ~ am, var_equal = TRUE, effect_size = "hedges_g")
#'
#' # One-sample t test
#' apa_htest(d, mpg ~ 1, mu = 20)
#'
#' # Paired t test with d_av
#' d_sleep <- data.frame(drug1 = sleep$extra[1:10], drug2 = sleep$extra[11:20])
#' apa_htest(d_sleep, Pair(drug1, drug2) ~ 1)
#'
#' # Wilcoxon rank-sum tests with rank-biserial correlations
#' apa_htest(d, mpg + hp ~ am, test = "wilcoxon", exact = FALSE)
#'
#' # Correlation tests
#' apa_htest(d, mpg + hp + qsec ~ wt, test = "cor")
#'
#' # Chi-square tests of independence with Cramer's V
#' d$cyl <- factor(d$cyl)
#' d$vs <- factor(d$vs, labels = c("V-shaped", "Straight"))
#' tbl <- apa_htest(d, cyl + vs ~ am, test = "chisq")
#' tbl
#' apa_note(tbl)
#'
#' # Test results already computed
#' apa_htest(t.test(mpg ~ am, data = d))
#' apa_htest(list(
#'   Weight = cor.test(d$mpg, d$wt),
#'   Displacement = cor.test(d$mpg, d$disp)
#' ))
apa_htest <- function(
  data,
  formula,
  test = c("t", "wilcoxon", "cor", "chisq"),
  effect_size = NULL,
  ci = 0.95,
  var_equal = FALSE,
  mu = 0,
  method = c("pearson", "spearman", "kendall"),
  descriptives = TRUE,
  pair_labels = NULL,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  ...
) {
  # Test results that were already computed
  if (inherits(data, "htest") || is_htest_list(data)) {
    return(htest_from_objects(
      data,
      effect_size = effect_size,
      ci = ci,
      starred = starred,
      bolded = bolded,
      column_formats = column_formats
    ))
  }
  test <- match.arg(test)
  method <- match.arg(method)

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame, not {.obj_type_friendly {data}}.")
  }
  if (!inherits(formula, "formula") || length(formula) != 3) {
    cli::cli_abort(c(
      "{.arg formula} must be a two-sided formula.",
      i = "For example, {.code mpg ~ am}, {.code mpg + hp ~ am}, or {.code mpg ~ 1}."
    ))
  }
  if (!is.numeric(ci) || length(ci) != 1 || is.na(ci) || ci <= 0 || ci >= 1) {
    cli::cli_abort("{.arg ci} must be a number between 0 and 1.")
  }

  outcomes <- htest_outcomes(formula[[2]])
  predictor <- all.vars(formula[[3]])
  if (length(predictor) > 1) {
    cli::cli_abort(c(
      "{.arg formula} can have only one variable on the right.",
      i = "To test several outcomes, put them on the left (e.g., {.code y1 + y2 ~ group})."
    ))
  }
  paired <- is.list(outcomes)
  if (paired) {
    if (length(predictor) > 0) {
      cli::cli_abort(c(
        "Paired tests need {.code 1} on the right side of {.arg formula}.",
        i = "For example, {.code Pair(pre, post) ~ 1}."
      ))
    }
    if (!(test %in% c("t", "wilcoxon"))) {
      cli::cli_abort(
        "{.fn Pair} works only with {.code test = \"t\"} or {.code test = \"wilcoxon\"}."
      )
    }
  }
  one_sample <- !paired && length(predictor) == 0
  if (one_sample && !(test %in% c("t", "wilcoxon"))) {
    cli::cli_abort(
      "{.code test = \"{test}\"} needs a variable on the right side of {.arg formula}."
    )
  }
  missing_vars <- setdiff(c(unlist(outcomes), predictor), colnames(data))
  if (length(missing_vars) > 0) {
    cli::cli_abort("{.arg data} has no {cli::qty(missing_vars)} column{?s} {.val {missing_vars}}.")
  }

  effect_size <- htest_effect_size(test, effect_size, paired)

  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }

  # Two-group tests share the groups of the grouping variable, and paired
  # tests label the first and second variable of each pair
  name_formatter <- column_formats$Parameter@formatter
  groups <- NULL
  if (paired) {
    groups <- htest_pair_labels(pair_labels, outcomes, name_formatter)
  } else if (!one_sample && test %in% c("t", "wilcoxon")) {
    groups <- htest_groups(data[[predictor]], predictor)
  }

  rows <- purrr::map(outcomes, \(v) {
    if (paired) {
      row <- switch(
        test,
        t = htest_t_paired(data[[v[1]]], data[[v[2]]], v, mu, effect_size, ci, ...),
        wilcoxon = htest_wilcoxon_paired(data[[v[1]]], data[[v[2]]], v, mu, effect_size, ci, ...)
      )
      row$Parameter <- paste(name_formatter(v[1]), "vs.", name_formatter(v[2]))
      return(row)
    }
    y <- data[[v]]
    x <- if (one_sample) NULL else data[[predictor]]
    row <- switch(
      test,
      t = htest_t(y, x, groups, v, var_equal, mu, effect_size, ci, ...),
      wilcoxon = htest_wilcoxon(y, x, groups, v, mu, effect_size, ci, ...),
      cor = htest_cor(y, x, v, method, ci, ...),
      chisq = htest_chisq(y, x, v, effect_size, ci, ...)
    )
    row$Parameter <- name_formatter(v)
    row
  })
  method_names <- unique(purrr::map_chr(rows, \(r) attr(r, "method")))
  alternative <- unique(purrr::map_chr(rows, \(r) attr(r, "alternative")))
  d <- dplyr::bind_rows(rows)
  d <- dplyr::relocate(d, "Parameter")

  formatted <- htest_format(
    d,
    test = test,
    ci = ci,
    descriptives = descriptives,
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    groups = groups
  )
  d_new <- formatted$data
  es_col <- formatted$es_col
  cf <- formatted$cf

  record <- attr(d_new, "apa_note", exact = TRUE)
  attr(d_new, "apa_note") <- htest_note_record(
    test = test,
    n_tests = nrow(d),
    one_sample = one_sample,
    paired = paired,
    var_equal = var_equal,
    mu = mu,
    method = method,
    method_names = method_names,
    alternative = alternative,
    effect_size = es_col,
    effect_size_header = formatted$es_header,
    predictor = if (one_sample || paired) NULL else name_formatter(predictor),
    pair = if (paired && length(outcomes) == 1) name_formatter(outcomes[[1]]) else NULL,
    abbreviations = record$abbreviations,
    starred = record$starred,
    bolded = record$bolded
  )
  as_apa7_tbl(labels_formatted(d_new))
}

#' Formats and columns of a hypothesis test table
#'
#' @param d one row per test, with `Parameter` first
#' @returns list with the formatted table (`data`), the column formats
#'   (`cf`), the effect size column (`es_col`, or NULL), and its header
#'   (`es_header`, or NULL)
#' @keywords internal
#' @noRd
htest_format <- function(
  d,
  test,
  ci,
  descriptives,
  starred,
  bolded,
  column_formats,
  groups = NULL
) {
  # Columns and their formats
  es_col <- intersect(colnames(d), htest_es_columns)[1]
  cf <- column_formats
  cf <- htest_group_formats(cf, groups)
  # Row labels are already formatted (e.g., "Pre vs. Post")
  cf$Parameter@formatter <- \(x, ...) x

  if (test == "cor") {
    es_col <- intersect(c("r", "rho", "tau"), colnames(d))[1]
    if (!is.null(cf$tau)) {
      cf$tau@abbreviation <- "Kendall's rank correlation"
    }
  }
  # Bounded effect sizes have no leading zero, and exact values such as -1
  # keep their decimals (-1.00)
  if (!is.na(es_col) && es_col %in% htest_bounded_es && !is.null(cf[[es_col]])) {
    cf[[es_col]]@formatter <- \(x, accuracy = the$accuracy, ...) {
      align_chr(x, accuracy = accuracy, trim_leading_zeros = TRUE, format_integers = TRUE)
    }
  }

  if (!is.na(es_col) && "CI_low" %in% colnames(d)) {
    d$CI <- ci
    if (!is.null(cf$CI)) {
      # Bounded effect sizes have no leading zero
      if (es_col %in% htest_bounded_es) {
        cf <- es_ci_format(cf)
      }
      es_header <- if (is.null(cf[[es_col]])) es_col else cf[[es_col]]@header
      cf$CI@abbreviation <- c(CI = paste0("confidence interval for ", es_header))
    }
  }

  descriptive_cols <- grep("^(M|SD|Mdn)(_[12])?$|^n$|^N$", colnames(d), value = TRUE)
  if (!isTRUE(descriptives)) {
    descriptive_cols <- character(0)
  }
  stat_cols <- intersect(c("t", "W", "V", "Chi2"), colnames(d))
  df_cols <- intersect(c("df_error", "df"), colnames(d))
  htest_parameters <- c(
    "Parameter",
    descriptive_cols,
    if (test == "cor") c(es_col, "CI"),
    stat_cols,
    df_cols,
    "p",
    if (test != "cor" && !is.na(es_col)) c(es_col, "CI")
  )
  htest_parameters <- unique(htest_parameters[!is.na(htest_parameters)])

  d_new <- format_apa_parameters(
    d,
    predictor_parameters = htest_parameters,
    starred = starred,
    bolded = bolded,
    column_formats = cf,
    t_with_df = TRUE
  )

  list(
    data = d_new,
    cf = cf,
    es_col = if (is.na(es_col)) NULL else es_col,
    es_header = if (is.na(es_col) || is.null(cf[[es_col]])) NULL else cf[[es_col]]@header
  )
}

#' Effect sizes between -1 and 1
#' @keywords internal
#' @noRd
htest_bounded_es <- c("r", "rho", "tau", "r_rank_biserial", "Cramers_v", "phi")

#' Effect size columns from effectsize
#' @keywords internal
#' @noRd
htest_es_columns <- c(
  "Cohens_d",
  "Hedges_g",
  "d_av",
  "d_z",
  "d_rm",
  "g_av",
  "g_z",
  "g_rm",
  "r_rank_biserial",
  "Cramers_v",
  "phi",
  "Cohens_w"
)

#' Check and resolve the effect_size argument of apa_htest()
#' @keywords internal
#' @noRd
htest_effect_size <- function(test, effect_size, paired = FALSE) {
  allowed <- switch(
    test,
    t = if (paired) {
      c("d_av", "d_z", "d_rm", "g_av", "g_z", "g_rm", "cohens_d", "hedges_g")
    } else {
      c("cohens_d", "hedges_g")
    },
    wilcoxon = "rank_biserial",
    cor = character(0),
    chisq = c("cramers_v", "phi", "cohens_w")
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
  if (test == "cor") {
    cli::cli_abort(c(
      "Correlation tests show the correlation as the effect size.",
      i = "Use {.code effect_size = NULL} or {.code effect_size = \"none\"}."
    ))
  }
  if (!(effect_size %in% allowed)) {
    cli::cli_abort(
      "For {.code test = \"{test}\"}, {.arg effect_size} must be {.or {.val {c(allowed, 'none')}}}, not {.val {effect_size}}."
    )
  }
  if (paired) {
    effect_size <- switch(effect_size, cohens_d = "d_av", hedges_g = "g_av", effect_size)
  }
  effect_size
}

#' Outcomes on the left side of an apa_htest() formula
#'
#' @returns character vector of outcome names, or a list of length-2
#'   character vectors for `Pair()` terms
#' @keywords internal
#' @noRd
htest_outcomes <- function(lhs) {
  split_plus <- function(e) {
    if (is.call(e) && identical(e[[1]], as.name("+")) && length(e) == 3) {
      c(split_plus(e[[2]]), list(e[[3]]))
    } else {
      list(e)
    }
  }
  terms <- split_plus(lhs)
  is_pair <- purrr::map_lgl(terms, \(e) is.call(e) && identical(e[[1]], as.name("Pair")))
  if (any(is_pair) && !all(is_pair)) {
    cli::cli_abort(c(
      "{.arg formula} can't mix {.fn Pair} terms with single outcomes.",
      i = "Make one table of paired tests and another of the other tests."
    ))
  }
  if (all(is_pair)) {
    pair_error <- "{.fn Pair} takes two variable names, as in {.code Pair(pre, post)}."
    return(purrr::map(terms, \(e) {
      args <- as.list(e)[-1]
      if (length(args) != 2 || !all(purrr::map_lgl(args, is.name))) {
        cli::cli_abort(pair_error)
      }
      purrr::map_chr(args, as.character)
    }))
  }
  outcomes <- all.vars(lhs)
  if (length(outcomes) == 0) {
    cli::cli_abort("{.arg formula} needs at least one outcome variable on the left.")
  }
  outcomes
}

#' Headings for the first and second variables of paired tests
#' @keywords internal
#' @noRd
htest_pair_labels <- function(pair_labels, pairs, formatter) {
  if (!is.null(pair_labels)) {
    if (!is.character(pair_labels) || length(pair_labels) != 2) {
      cli::cli_abort("{.arg pair_labels} must be a character vector of length 2.")
    }
    return(pair_labels)
  }
  if (length(pairs) == 1) {
    return(formatter(pairs[[1]]))
  }
  c("First", "Second")
}

#' The two groups of a grouping variable, in factor level order
#' @keywords internal
#' @noRd
htest_groups <- function(g, name) {
  if (is.factor(g)) {
    groups <- levels(droplevels(g[!is.na(g)]))
  } else {
    groups <- sort(unique(g[!is.na(g)]))
  }
  if (length(groups) != 2) {
    cli::cli_abort(c(
      "The grouping variable {.var {name}} must have exactly 2 groups, not {length(groups)}.",
      i = "For more than two groups, use {.fn apa_anova}."
    ))
  }
  groups
}

#' Column formats for group descriptives, with the group as a spanner
#' (e.g., "Manual_*M*")
#' @keywords internal
#' @noRd
htest_group_formats <- function(cf, groups) {
  base <- c("M", "SD", "Mdn")
  if (is.null(groups)) {
    return(cf)
  }
  labels <- gsub("_", " ", as.character(groups), fixed = TRUE)
  for (b in base) {
    if (is.null(cf[[b]])) {
      next # nocov
    }
    for (i in 1:2) {
      new_cf <- cf[[b]]
      new_cf@name <- paste0(b, "_", i)
      new_cf@header <- paste0(labels[i], "_", cf[[b]]@header)
      new_cf@latex <- paste0(labels[i], "_", cf[[b]]@latex)
      cf[[new_cf@name]] <- new_cf
    }
  }
  cf
}

#' Split an outcome into two groups, dropping missing values
#' @keywords internal
#' @noRd
htest_split <- function(y, x, groups) {
  ok <- !is.na(y) & !is.na(x)
  list(
    y[ok & x == groups[1]],
    y[ok & x == groups[2]]
  )
}

#' Two-sided interval of an effect size computed from the data
#' @keywords internal
#' @noRd
htest_es <- function(fun, ...) {
  tibble::as_tibble(as.data.frame(fun(..., alternative = "two.sided")))[
    1,
    ,
    drop = FALSE
  ]
}

#' Attach the test method and alternative to a row
#' @keywords internal
#' @noRd
htest_row <- function(row, test_result) {
  attr(row, "method") <- test_result$method
  attr(row, "alternative") <- if (is.null(test_result$alternative)) {
    "two.sided"
  } else {
    test_result$alternative
  }
  row
}

#' @keywords internal
#' @noRd
htest_t <- function(y, x, groups, name, var_equal, mu, effect_size, ci, ...) {
  check_numeric_outcome(y, name, "t")
  es_fun <- switch(
    effect_size,
    cohens_d = effectsize::cohens_d,
    hedges_g = effectsize::hedges_g,
    none = NULL
  )
  if (is.null(x)) {
    y <- y[!is.na(y)]
    tt <- stats::t.test(y, mu = mu, conf.level = ci, ...)
    row <- tibble::tibble(M = mean(y), SD = stats::sd(y))
    if (!is.null(es_fun)) {
      es <- htest_es(es_fun, y, mu = mu, ci = ci)
    }
  } else {
    xy <- htest_split(y, x, groups)
    tt <- stats::t.test(
      xy[[1]],
      xy[[2]],
      var.equal = var_equal,
      mu = mu,
      conf.level = ci,
      ...
    )
    row <- tibble::tibble(
      M_1 = mean(xy[[1]]),
      SD_1 = stats::sd(xy[[1]]),
      M_2 = mean(xy[[2]]),
      SD_2 = stats::sd(xy[[2]])
    )
    if (!is.null(es_fun)) {
      es <- htest_es(es_fun, xy[[1]], xy[[2]], mu = mu, pooled_sd = var_equal, ci = ci)
    }
  }
  row$t <- unname(tt$statistic)
  row$df_error <- unname(tt$parameter)
  row$p <- tt$p.value
  if (!is.null(es_fun)) {
    row <- dplyr::bind_cols(row, es[setdiff(colnames(es), "CI")])
  }
  htest_row(row, tt)
}

#' @keywords internal
#' @noRd
htest_wilcoxon <- function(y, x, groups, name, mu, effect_size, ci, ...) {
  check_numeric_outcome(y, name, "wilcoxon")
  if (is.null(x)) {
    y <- y[!is.na(y)]
    wt <- stats::wilcox.test(y, mu = mu, ...)
    row <- tibble::tibble(Mdn = stats::median(y), V = unname(wt$statistic))
    if (effect_size != "none") {
      es <- htest_es(effectsize::rank_biserial, y, mu = mu, ci = ci)
    }
  } else {
    xy <- htest_split(y, x, groups)
    wt <- stats::wilcox.test(xy[[1]], xy[[2]], mu = mu, ...)
    row <- tibble::tibble(
      Mdn_1 = stats::median(xy[[1]]),
      Mdn_2 = stats::median(xy[[2]]),
      W = unname(wt$statistic)
    )
    if (effect_size != "none") {
      es <- htest_es(effectsize::rank_biserial, xy[[1]], xy[[2]], mu = mu, ci = ci)
    }
  }
  row$p <- wt$p.value
  if (effect_size != "none") {
    row <- dplyr::bind_cols(row, es[setdiff(colnames(es), "CI")])
  }
  htest_row(row, wt)
}

#' Complete pairs of two paired variables
#' @keywords internal
#' @noRd
htest_pairs <- function(y1, y2, vars, test) {
  check_numeric_outcome(y1, vars[1], test)
  check_numeric_outcome(y2, vars[2], test)
  ok <- !is.na(y1) & !is.na(y2)
  list(y1[ok], y2[ok])
}

#' @keywords internal
#' @noRd
htest_t_paired <- function(y1, y2, vars, mu, effect_size, ci, ...) {
  xy <- htest_pairs(y1, y2, vars, "t")
  tt <- stats::t.test(xy[[1]], xy[[2]], paired = TRUE, mu = mu, conf.level = ci, ...)
  row <- tibble::tibble(
    M_1 = mean(xy[[1]]),
    SD_1 = stats::sd(xy[[1]]),
    M_2 = mean(xy[[2]]),
    SD_2 = stats::sd(xy[[2]]),
    t = unname(tt$statistic),
    df_error = unname(tt$parameter),
    p = tt$p.value
  )
  if (effect_size != "none") {
    es <- htest_es(
      effectsize::repeated_measures_d,
      xy[[1]],
      xy[[2]],
      mu = mu,
      method = sub("^[dg]_", "", effect_size),
      adjust = startsWith(effect_size, "g_"),
      ci = ci
    )
    # effectsize names the column d_* even with the small-sample correction
    colnames(es)[1] <- effect_size
    row <- dplyr::bind_cols(row, es[setdiff(colnames(es), "CI")])
  }
  htest_row(row, tt)
}

#' @keywords internal
#' @noRd
htest_wilcoxon_paired <- function(y1, y2, vars, mu, effect_size, ci, ...) {
  xy <- htest_pairs(y1, y2, vars, "wilcoxon")
  wt <- stats::wilcox.test(xy[[1]], xy[[2]], paired = TRUE, mu = mu, ...)
  row <- tibble::tibble(
    Mdn_1 = stats::median(xy[[1]]),
    Mdn_2 = stats::median(xy[[2]]),
    V = unname(wt$statistic),
    p = wt$p.value
  )
  if (effect_size != "none") {
    es <- htest_es(
      effectsize::rank_biserial,
      xy[[1]],
      xy[[2]],
      paired = TRUE,
      mu = mu,
      ci = ci
    )
    row <- dplyr::bind_cols(row, es[setdiff(colnames(es), "CI")])
  }
  htest_row(row, wt)
}

#' @keywords internal
#' @noRd
htest_cor <- function(y, x, name, method, ci, ...) {
  check_numeric_outcome(y, name, "cor")
  ok <- !is.na(y) & !is.na(x)
  ct <- stats::cor.test(y[ok], x[ok], method = method, conf.level = ci, ...)
  est_col <- switch(method, pearson = "r", spearman = "rho", kendall = "tau")
  row <- tibble::tibble(n = sum(ok))
  row[[est_col]] <- unname(ct$estimate)
  if (!is.null(ct$conf.int)) {
    row$CI_low <- ct$conf.int[1]
    row$CI_high <- ct$conf.int[2]
  }
  if (method == "pearson") {
    row$t <- unname(ct$statistic)
    row$df_error <- unname(ct$parameter)
  }
  row$p <- ct$p.value
  htest_row(row, ct)
}

#' @keywords internal
#' @noRd
htest_chisq <- function(y, x, name, effect_size, ci, ...) {
  ok <- !is.na(y) & !is.na(x)
  tab <- table(y[ok], x[ok])
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (any(dim(tab) < 2)) {
    cli::cli_abort(
      "{.var {name}} and the variable it is tested with each need at least 2 observed values."
    )
  }
  ch <- stats::chisq.test(tab, ...)
  row <- tibble::tibble(
    N = sum(tab),
    Chi2 = unname(ch$statistic),
    df = unname(ch$parameter),
    p = ch$p.value
  )
  if (effect_size != "none") {
    if (effect_size == "phi" && !all(dim(tab) == 2)) {
      cli::cli_abort(
        "{.code effect_size = \"phi\"} needs 2 {cli::symbol$times} 2 tables, but {.var {name}} has a {nrow(tab)} {cli::symbol$times} {ncol(tab)} table. Use {.val cramers_v} instead."
      )
    }
    es <- switch(
      effect_size,
      cramers_v = htest_es(effectsize::cramers_v, tab, adjust = FALSE, ci = ci),
      phi = htest_es(effectsize::phi, tab, adjust = FALSE, ci = ci),
      cohens_w = htest_es(effectsize::cohens_w, tab, ci = ci)
    )
    row <- dplyr::bind_cols(row, es[setdiff(colnames(es), "CI")])
  }
  htest_row(row, ch)
}

#' @keywords internal
#' @noRd
check_numeric_outcome <- function(y, name, test) {
  if (!is.numeric(y)) {
    cli::cli_abort(
      "{.code test = \"{test}\"} needs a numeric outcome, but {.var {name}} is {.obj_type_friendly {y}}."
    )
  }
}

#' Record what a hypothesis test table's note needs
#' @keywords internal
#' @noRd
htest_note_record <- function(
  test,
  n_tests = 1L,
  one_sample = FALSE,
  paired = FALSE,
  var_equal = FALSE,
  mu = 0,
  method = "pearson",
  method_names = character(0),
  alternative = "two.sided",
  effect_size = NULL,
  effect_size_header = NULL,
  predictor = NULL,
  pair = NULL,
  abbreviations = character(0),
  starred = FALSE,
  bolded = FALSE,
  from_object = FALSE,
  kind = NULL,
  null_values = NULL
) {
  list(
    table = "htest",
    from_object = from_object,
    kind = kind,
    null_values = null_values,
    test = test,
    n_tests = n_tests,
    one_sample = one_sample,
    paired = isTRUE(paired),
    var_equal = var_equal,
    mu = mu,
    method = method,
    method_names = method_names,
    alternative = alternative,
    effect_size = effect_size,
    effect_size_header = effect_size_header,
    predictor = predictor,
    pair = pair,
    abbreviations = abbreviations,
    starred = isTRUE(starred),
    bolded = isTRUE(bolded)
  )
}

#' @keywords internal
#' @noRd
htest_note_sentences <- function(record) {
  if (isTRUE(record$from_object)) {
    return(htest_object_sentences(record))
  }
  sentences <- character(0)
  mu <- sub("-", "\u2212", format(record$mu, trim = TRUE), fixed = TRUE)

  # Which tests, in the singular for one test
  one <- isTRUE(record$n_tests == 1)
  tests <- if (one) " test " else " tests "
  verb <- if (one) "compares " else "compare "
  each <- if (one) "the " else "each "
  # Paired tests compare the variables of each pair (named if just one pair)
  pair_text <- if (length(record$pair) == 2) {
    paste0(record$pair[1], " with ", record$pair[2])
  } else {
    "the two variables in each row"
  }
  test_sentence <- switch(
    record$test,
    t = if (isTRUE(record$paired)) {
      paste0("Paired *t*", tests, verb, pair_text, ".")
    } else if (record$one_sample) {
      paste0("One-sample *t*", tests, verb, each, "mean with ", mu, ".")
    } else if (isTRUE(record$var_equal)) {
      paste0(
        "Student's *t*", tests, verb, "the groups of ",
        record$predictor,
        ", assuming equal variances."
      )
    } else {
      paste0(
        "Welch's *t*", tests, verb, "the groups of ",
        record$predictor,
        " without assuming equal variances."
      )
    },
    wilcoxon = if (isTRUE(record$paired)) {
      paste0("Wilcoxon signed-rank", tests, verb, pair_text, ".")
    } else if (record$one_sample) {
      paste0("Wilcoxon signed-rank", tests, verb, each, "variable with ", mu, ".")
    } else {
      paste0("Wilcoxon rank-sum", tests, verb, "the groups of ", record$predictor, ".")
    },
    cor = paste0(
      switch(
        record$method,
        pearson = "Pearson",
        spearman = "Spearman rank",
        kendall = "Kendall rank"
      ),
      if (one) " correlation with " else " correlations with ",
      record$predictor,
      "."
    ),
    chisq = c(
      paste0(
        "Chi-square",
        tests,
        "of independence with ",
        record$predictor,
        "."
      ),
      if (any(grepl("continuity correction", record$method_names, fixed = TRUE))) {
        "2 \u00D7 2 tables use Yates's continuity correction."
      }
    )
  )
  sentences <- c(sentences, test_sentence)

  # Hypothesized difference for two-group tests
  if (!record$one_sample && record$test %in% c("t", "wilcoxon") && record$mu != 0) {
    sentences <- c(
      sentences,
      paste0(
        "The hypothesized difference between ",
        if (isTRUE(record$paired)) "variables" else "groups",
        " is ",
        mu,
        "."
      )
    )
  }

  # One-sided tests
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

  # How the effect size was computed
  if (
    record$test == "t" &&
      !record$one_sample &&
      isTRUE(record$effect_size %in% c("Cohens_d", "Hedges_g"))
  ) {
    sentences <- c(
      sentences,
      paste0(
        record$effect_size_header,
        if (isTRUE(record$var_equal)) {
          " uses the pooled standard deviation."
        } else {
          " uses the average of the two groups' variances."
        }
      )
    )
  }

  if (isTRUE(record$paired) && isTRUE(record$effect_size %in% htest_paired_es)) {
    how <- switch(
      sub("^[dg]_", "", record$effect_size),
      av = " divides the mean difference by the average of the two variables' standard deviations",
      z = " divides the mean difference by the standard deviation of the differences",
      rm = " divides the mean difference by the standard deviation of the differences, corrected for the correlation between the variables"
    )
    sentences <- c(
      sentences,
      paste0(
        record$effect_size_header,
        how,
        if (startsWith(record$effect_size, "g_")) ", with a small-sample correction",
        "."
      )
    )
  }

  sentences <- c(sentences, abbreviation_sentence(record$abbreviations))
  c(sentences, probability_note_sentences(record$starred, record$bolded))
}

#' Paired effect sizes from effectsize::repeated_measures_d()
#' @keywords internal
#' @noRd
htest_paired_es <- c("d_av", "d_z", "d_rm", "g_av", "g_z", "g_rm")

#' Probability notes for stars and bold values
#' @keywords internal
#' @noRd
probability_note_sentences <- function(starred = FALSE, bolded = FALSE) {
  sentences <- character(0)
  if (isTRUE(starred)) {
    sentences <- c(sentences, paste0(apa_p_star_note(), "."))
  }
  if (isTRUE(bolded)) {
    sentences <- c(
      sentences,
      paste0(
        "Bold values are statistically significant at *p*",
        intToUtf8(c(160, 8804, 160)),
        ".05."
      )
    )
  }
  sentences
}
