#' Descriptive statistics table ("Table 1")
#'
#' Makes an APA-style table describing a sample: for each continuous
#' variable, its mean and standard deviation (or median and interquartile
#' range), and for each categorical variable, the number and percentage of
#' respondents at each level. With `by`, there is a column for each group and
#' one for the whole sample, and group differences can be tested.
#'
#' Numeric variables are continuous, and factors, character vectors, and
#' logical vectors are categorical (use `categorical` to treat numeric codes
#' as categories). Each categorical variable is introduced by a heading row,
#' with its levels indented below it, in factor level order (character
#' vectors are sorted).
#'
#' Percentages are of the nonmissing responses, and [apa_note()] lists the
#' number of missing values of each variable that has any. Rows whose `by`
#' value is missing are left out.
#'
#' With `test = TRUE`, a *p* column tests group differences: Welch's *t* test
#' (two groups) or Welch's one-way ANOVA ([stats::oneway.test()], more groups)
#' for variables described by their mean, the Kruskal-Wallis test for
#' variables described by their median, and the chi-square test of
#' independence (without continuity correction) for categorical variables.
#' Many journals discourage testing baseline differences in randomized
#' trials, so tests are not shown by default.
#'
#' @param data data frame
#' @param variables names of the variables to describe, in order. If `NULL`
#'   (default), all columns except `by`.
#' @param by name of a grouping variable. If given, each group gets a column.
#' @param total if `TRUE` (default), a column for the whole sample is added
#'   when there are groups
#' @param continuous how continuous variables are described: `"mean_sd"`
#'   (default) for *M* (*SD*), or `"median_iqr"` for *Mdn* (*IQR*). A named
#'   vector sets it for particular variables (e.g., `c(income =
#'   "median_iqr")`), and other variables use `"mean_sd"`.
#' @param categorical names of numeric variables to treat as categorical
#' @param labels named character vector of variable labels (e.g., `c(age =
#'   "Age (years)")`). Variables without a label are named by their formatted
#'   names.
#' @param test if `TRUE`, a column of *p*-values tests the differences
#'   between groups (requires `by`)
#' @param digits number of decimal places of means, standard deviations,
#'   medians, and interquartile ranges
#' @param percent_digits number of decimal places of percentages
#' @param format_variable_names format variable names that have no label with
#'   the `Variable` column format
#'
#' @returns `apa7_tbl` (tibble)
#' @export
#' @examples
#' d <- mtcars
#' d$am <- factor(d$am, labels = c("Automatic", "Manual"))
#' d$cyl <- factor(d$cyl)
#' apa_descriptives(d, variables = c("mpg", "hp", "cyl"), by = "am")
#'
#' # Medians for skewed variables, and tests of group differences
#' apa_descriptives(
#'   d,
#'   variables = c("mpg", "hp", "cyl"),
#'   by = "am",
#'   continuous = c(hp = "median_iqr"),
#'   test = TRUE
#' )
apa_descriptives <- function(
  data,
  variables = NULL,
  by = NULL,
  total = TRUE,
  continuous = "mean_sd",
  categorical = NULL,
  labels = NULL,
  test = FALSE,
  digits = 2,
  percent_digits = 1,
  format_variable_names = TRUE
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame, not {.obj_type_friendly {data}}.")
  }
  data <- as.data.frame(data)
  if (!is.null(by)) {
    if (!is.character(by) || length(by) != 1 || !(by %in% colnames(data))) {
      cli::cli_abort("{.arg by} must be the name of a column of {.arg data}.")
    }
  }
  if (is.null(variables)) {
    variables <- setdiff(colnames(data), by)
  }
  unknown <- setdiff(c(variables, categorical), colnames(data))
  if (length(unknown) > 0) {
    cli::cli_abort("{.arg data} has no {cli::qty(unknown)} column{?s} {.val {unknown}}.")
  }
  if (isTRUE(test) && is.null(by)) {
    cli::cli_abort("{.code test = TRUE} needs groups to compare. Give {.arg by}.")
  }
  methods <- descriptives_methods(continuous, variables)

  # Groups (rows with a missing group are left out)
  n_dropped <- 0L
  groups <- NULL
  if (!is.null(by)) {
    missing_group <- is.na(data[[by]])
    n_dropped <- sum(missing_group)
    data <- data[!missing_group, , drop = FALSE]
    g <- data[[by]]
    groups <- if (is.factor(g)) levels(droplevels(g)) else sort(unique(as.character(g)))
    if (length(groups) < 2) {
      cli::cli_abort("{.arg by} must have at least 2 groups.")
    }
  }

  # Column subsets: each group, then the whole sample
  subsets <- list()
  if (!is.null(groups)) {
    for (gr in groups) {
      subsets[[gr]] <- as.character(data[[by]]) == gr
    }
    if (total) {
      subsets[["Total"]] <- rep(TRUE, nrow(data))
    }
  } else {
    subsets[["Total"]] <- rep(TRUE, nrow(data))
  }

  label_of <- function(v) {
    if (!is.null(labels) && v %in% names(labels)) {
      return(labels[[v]])
    }
    if (format_variable_names) the$column_formats$Variable@formatter(v) else v
  }
  # Values inside combined cells such as "17.15 (3.83)" are not padded
  pct <- function(x) signs::signs(x, accuracy = .1^percent_digits)
  num <- function(x) signs::signs(x, accuracy = .1^digits)

  rows <- list()
  missing_counts <- integer(0)
  for (v in variables) {
    x <- data[[v]]
    is_categorical <- v %in% categorical || is.factor(x) || is.character(x) || is.logical(x)
    if (!is_categorical && !is.numeric(x)) {
      cli::cli_abort("{.var {v}} is {.obj_type_friendly {x}}, which cannot be described.")
    }
    if (any(is.na(x))) {
      missing_counts[[label_of(v)]] <- sum(is.na(x))
    }
    if (is_categorical) {
      levels_v <- if (is.factor(x)) levels(x) else sort(unique(as.character(x[!is.na(x)])))
      xc <- as.character(x)
      block <- tibble::tibble(Variable = c(paste0(label_of(v), ", *n* (%)"), paste0("\u2003", levels_v)))
      for (s in names(subsets)) {
        xs <- xc[subsets[[s]] & !is.na(xc)]
        counts <- vapply(levels_v, \(l) sum(xs == l), integer(1))
        percents <- if (length(xs) > 0) 100 * counts / length(xs) else rep(NA_real_, length(counts))
        block[[s]] <- c(NA_character_, paste0(counts, " (", pct(percents), ")"))
      }
      if (test) {
        block$p <- c(descriptives_test(xc, data[[by]], "categorical"), rep(NA_real_, length(levels_v)))
      }
    } else {
      method <- methods[[v]]
      suffix <- if (method == "median_iqr") ", *Mdn* (*IQR*)" else ", *M* (*SD*)"
      block <- tibble::tibble(Variable = paste0(label_of(v), suffix))
      for (s in names(subsets)) {
        xs <- x[subsets[[s]] & !is.na(x)]
        block[[s]] <- if (length(xs) == 0) {
          NA_character_
        } else if (method == "median_iqr") {
          paste0(num(stats::median(xs)), " (", num(stats::IQR(xs)), ")")
        } else {
          paste0(num(mean(xs)), " (", num(stats::sd(xs)), ")")
        }
      }
      if (test) {
        block$p <- descriptives_test(x, data[[by]], method)
      }
    }
    rows[[v]] <- block
  }
  d <- dplyr::bind_rows(rows)

  if (test) {
    d$p <- the$column_formats$p@formatter(d$p)
    d$p[is.na(d$p) | trimws(d$p) == "NA"] <- ""
  }
  d <- dplyr::mutate(d, dplyr::across(dplyr::everything(), \(v) ifelse(is.na(v), "", v)))

  # Group sizes in the headers
  n_of <- vapply(subsets, sum, integer(1))
  headers <- purrr::map2_chr(names(subsets), n_of, \(s, n) {
    if (s == "Total") {
      paste0("Total (*N* = ", format(n, big.mark = ","), ")")
    } else {
      paste0(gsub("_", " ", s, fixed = TRUE), " (*n* = ", format(n, big.mark = ","), ")")
    }
  })
  colnames(d)[match(names(subsets), colnames(d))] <- headers
  if (test) {
    colnames(d)[colnames(d) == "p"] <- "*p*"
  }

  attr(d, "no_format_columns") <- "Variable"
  attr(d, "apa_note") <- list(
    table = "descriptives",
    methods = unique(unname(methods[variables[!(variables %in% categorical) &
      !purrr::map_lgl(data[variables], \(x) is.factor(x) || is.character(x) || is.logical(x))]])),
    any_categorical = any(variables %in% categorical) ||
      any(purrr::map_lgl(data[variables], \(x) is.factor(x) || is.character(x) || is.logical(x))),
    missing = missing_counts,
    n_dropped = n_dropped,
    by = if (is.null(by)) NULL else label_of(by),
    test = isTRUE(test),
    n_groups = length(groups)
  )
  as_apa7_tbl(d)
}

#' How each continuous variable is described
#' @keywords internal
#' @noRd
descriptives_methods <- function(continuous, variables) {
  allowed <- c("mean_sd", "median_iqr")
  bad <- setdiff(unname(continuous), allowed)
  if (length(bad) > 0) {
    cli::cli_abort("{.arg continuous} must be {.or {.val {allowed}}}, not {.val {bad}}.")
  }
  methods <- stats::setNames(rep("mean_sd", length(variables)), variables)
  if (is.null(names(continuous))) {
    methods[] <- continuous[1]
  } else {
    unknown <- setdiff(names(continuous), variables)
    if (length(unknown) > 0) {
      cli::cli_abort("{.arg continuous} names unknown variables: {.val {unknown}}.")
    }
    methods[names(continuous)] <- continuous
  }
  methods
}

#' p-value of a test of group differences
#' @keywords internal
#' @noRd
descriptives_test <- function(x, g, method) {
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]
  g <- droplevels(as.factor(g[ok]))
  result <- tryCatch(
    suppressWarnings(switch(
      method,
      categorical = stats::chisq.test(table(x, g), correct = FALSE)$p.value,
      median_iqr = stats::kruskal.test(x, g)$p.value,
      mean_sd = if (nlevels(g) == 2) {
        stats::t.test(x ~ g)$p.value
      } else {
        stats::oneway.test(x ~ g)$p.value
      }
    )),
    error = function(e) NA_real_
  )
  result
}

#' @keywords internal
#' @noRd
descriptives_note_sentences <- function(record) {
  sentences <- character(0)
  described <- c(
    if ("mean_sd" %in% record$methods) "*M* (*SD*)",
    if ("median_iqr" %in% record$methods) "*Mdn* (*IQR*)"
  )
  parts <- c(
    if (length(described) > 0) paste0(paste(described, collapse = " or "), " for continuous variables"),
    if (isTRUE(record$any_categorical)) "*n* (%) for categorical variables"
  )
  if (length(parts) > 0) {
    sentences <- c(sentences, paste0("Values are ", paste(parts, collapse = " and "), "."))
  }
  if (isTRUE(record$any_categorical)) {
    sentences <- c(sentences, "Percentages are of nonmissing responses.")
  }
  if (length(record$missing) > 0) {
    sentences <- c(
      sentences,
      paste0(
        "Missing values: ",
        and_list(paste0(format(record$missing, big.mark = ",", trim = TRUE), " for ", names(record$missing))),
        "."
      )
    )
  }
  if (isTRUE(record$n_dropped > 0)) {
    sentences <- c(
      sentences,
      paste0(
        format(record$n_dropped, big.mark = ","),
        if (record$n_dropped == 1) " respondent" else " respondents",
        " with a missing value of ", record$by,
        if (record$n_dropped == 1) " is" else " are",
        " not included."
      )
    )
  }
  if (isTRUE(record$test)) {
    tests <- c(
      if ("mean_sd" %in% record$methods) {
        paste0(
          if (isTRUE(record$n_groups == 2)) "Welch's *t* tests" else "Welch's one-way ANOVAs",
          " (", if (length(described) > 1) "variables described by *M* (*SD*)" else "continuous variables", ")"
        )
      },
      if ("median_iqr" %in% record$methods) {
        paste0(
          "Kruskal", intToUtf8(8211), "Wallis tests (",
          if (length(described) > 1) "variables described by *Mdn* (*IQR*)" else "continuous variables", ")"
        )
      },
      if (isTRUE(record$any_categorical)) "chi-square tests of independence (categorical variables)"
    )
    sentences <- c(
      sentences,
      paste0("*p*-values are from ", and_list(tests), " comparing the groups of ", record$by, ".")
    )
  }
  abbreviations <- c(
    if ("median_iqr" %in% record$methods) c("*Mdn*" = "median", "*IQR*" = "interquartile range")
  )
  c(sentences, abbreviation_sentence(abbreviations))
}
