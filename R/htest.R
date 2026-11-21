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
#' @param data data frame
#' @param formula two-sided formula with outcome variables on the left and a
#'   grouping or predictor variable on the right. Several outcomes are joined
#'   with `+` (e.g., `mpg + hp ~ am`), and each gets its own row. For one-sample
#'   *t* tests and Wilcoxon signed-rank tests, use `1` on the right (e.g.,
#'   `mpg ~ 1`).
#' @param test type of test:
#'
#'   * `"t"`: *t* test comparing the means of two groups (Welch's test unless
#'     `var_equal = TRUE`), or a one-sample *t* test comparing a mean with `mu`
#'   * `"wilcoxon"`: Wilcoxon rank-sum test comparing two groups, or a Wilcoxon
#'     signed-rank test comparing a variable with `mu`
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
#' first group (in factor level order) minus the second.
#'
#' Effect sizes are computed from the data rather than from the test
#' statistic, so Cohen's *d* for Welch's test is exact rather than
#' approximated from *t*.
#'
#' Use [apa_note()] to write a table note that says which tests were done, how
#' the effect size was computed, and what the abbreviations stand for.
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
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  ...
) {
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

  outcomes <- all.vars(formula[[2]])
  predictor <- all.vars(formula[[3]])
  if (length(outcomes) == 0) {
    cli::cli_abort("{.arg formula} needs at least one outcome variable on the left.")
  }
  if (length(predictor) > 1) {
    cli::cli_abort(c(
      "{.arg formula} can have only one variable on the right.",
      i = "To test several outcomes, put them on the left (e.g., {.code y1 + y2 ~ group})."
    ))
  }
  one_sample <- length(predictor) == 0
  if (one_sample && !(test %in% c("t", "wilcoxon"))) {
    cli::cli_abort(
      "{.code test = \"{test}\"} needs a variable on the right side of {.arg formula}."
    )
  }
  missing_vars <- setdiff(c(outcomes, predictor), colnames(data))
  if (length(missing_vars) > 0) {
    cli::cli_abort("{.arg data} has no {cli::qty(missing_vars)} column{?s} {.val {missing_vars}}.")
  }

  effect_size <- htest_effect_size(test, effect_size)

  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }

  # Two-group tests share the groups of the grouping variable
  groups <- NULL
  if (!one_sample && test %in% c("t", "wilcoxon")) {
    groups <- htest_groups(data[[predictor]], predictor)
  }

  rows <- purrr::map(outcomes, \(v) {
    y <- data[[v]]
    x <- if (one_sample) NULL else data[[predictor]]
    row <- switch(
      test,
      t = htest_t(y, x, groups, v, var_equal, mu, effect_size, ci, ...),
      wilcoxon = htest_wilcoxon(y, x, groups, v, mu, effect_size, ci, ...),
      cor = htest_cor(y, x, v, method, ci, ...),
      chisq = htest_chisq(y, x, v, effect_size, ci, ...)
    )
    row$Parameter <- v
    row
  })
  method_names <- unique(purrr::map_chr(rows, \(r) attr(r, "method")))
  alternative <- unique(purrr::map_chr(rows, \(r) attr(r, "alternative")))
  d <- dplyr::bind_rows(rows)
  d <- dplyr::relocate(d, "Parameter")

  # Columns and their formats
  es_col <- intersect(colnames(d), htest_es_columns)[1]
  cf <- column_formats
  cf <- htest_group_formats(cf, groups)

  if (test == "cor") {
    es_col <- intersect(c("r", "rho", "tau"), colnames(d))[1]
    if (!is.null(cf$tau)) {
      cf$tau@formatter <- the$trim_leading_zero
      cf$tau@abbreviation <- "Kendall's rank correlation"
    }
  }

  if (!is.na(es_col) && "CI_low" %in% colnames(d)) {
    d$CI <- ci
    if (!is.null(cf$CI)) {
      # Bounded effect sizes have no leading zero
      if (es_col %in% c("r", "r_rank_biserial", "Cramers_v", "phi", "rho")) {
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

  record <- attr(d_new, "apa_note", exact = TRUE)
  attr(d_new, "apa_note") <- htest_note_record(
    test = test,
    n_tests = nrow(d),
    one_sample = one_sample,
    var_equal = var_equal,
    mu = mu,
    method = method,
    method_names = method_names,
    alternative = alternative,
    effect_size = if (is.na(es_col)) NULL else es_col,
    effect_size_header = if (is.na(es_col) || is.null(cf[[es_col]])) NULL else cf[[es_col]]@header,
    predictor = if (one_sample) NULL else cf$Parameter@formatter(predictor),
    abbreviations = record$abbreviations,
    starred = record$starred,
    bolded = record$bolded
  )
  as_apa7_tbl(d_new)
}

#' Effect size columns from effectsize
#' @keywords internal
#' @noRd
htest_es_columns <- c(
  "Cohens_d",
  "Hedges_g",
  "r_rank_biserial",
  "Cramers_v",
  "phi",
  "Cohens_w"
)

#' Check and resolve the effect_size argument of apa_htest()
#' @keywords internal
#' @noRd
htest_effect_size <- function(test, effect_size) {
  allowed <- switch(
    test,
    t = c("cohens_d", "hedges_g"),
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
  effect_size
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
  var_equal = FALSE,
  mu = 0,
  method = "pearson",
  method_names = character(0),
  alternative = "two.sided",
  effect_size = NULL,
  effect_size_header = NULL,
  predictor = NULL,
  abbreviations = character(0),
  starred = FALSE,
  bolded = FALSE
) {
  list(
    table = "htest",
    test = test,
    n_tests = n_tests,
    one_sample = one_sample,
    var_equal = var_equal,
    mu = mu,
    method = method,
    method_names = method_names,
    alternative = alternative,
    effect_size = effect_size,
    effect_size_header = effect_size_header,
    predictor = predictor,
    abbreviations = abbreviations,
    starred = isTRUE(starred),
    bolded = isTRUE(bolded)
  )
}

#' @keywords internal
#' @noRd
htest_note_sentences <- function(record) {
  sentences <- character(0)
  mu <- format(record$mu, trim = TRUE)

  # Which tests, in the singular for one test
  one <- isTRUE(record$n_tests == 1)
  tests <- if (one) " test " else " tests "
  verb <- if (one) "compares " else "compare "
  each <- if (one) "the " else "each "
  test_sentence <- switch(
    record$test,
    t = if (record$one_sample) {
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
    wilcoxon = if (record$one_sample) {
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
      paste0("The hypothesized difference between groups is ", mu, ".")
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

  sentences <- c(sentences, abbreviation_sentence(record$abbreviations))
  c(sentences, probability_note_sentences(record$starred, record$bolded))
}

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
