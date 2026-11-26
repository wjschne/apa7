#' Reliability and item analysis table
#'
#' Makes an APA-style table of the reliability of one or more scales: for each
#' item, its mean, standard deviation, corrected item-total correlation, and
#' Cronbach's \eqn{\alpha}{alpha} if the item were deleted, followed for each
#' scale by the scale score's mean and standard deviation, Cronbach's
#' \eqn{\alpha}{alpha} with its confidence interval, McDonald's
#' \eqn{\omega_{\text{total}}}{omega total}, and the mean inter-item
#' correlation.
#'
#' The corrected item-total correlation (*r*~it~) is the item's correlation
#' with the sum of the other items in its scale. Items with low values (often,
#' below .30) discriminate poorly. \eqn{\alpha}{Alpha} if deleted is the
#' scale's \eqn{\alpha}{alpha} without the item; values above the scale's
#' \eqn{\alpha}{alpha} mean that the item lowers reliability.
#'
#' Cronbach's \eqn{\alpha}{alpha}, the item statistics, and the mean
#' inter-item correlation come from [psych::alpha()]. The confidence interval
#' of \eqn{\alpha}{alpha} is Feldt's. \eqn{\omega_{\text{total}}}{Omega total}
#' is computed from a one-factor model ([psych::fa()], minimum residual):
#' \eqn{(\sum\lambda)^2 / ((\sum\lambda)^2 + \sum\psi)}{(sum of loadings)^2 /
#' ((sum of loadings)^2 + sum of uniquenesses)}.
#'
#' Each scale uses the respondents who answered all of its items, and the
#' note gives their number. Scale scores are the means of the items (or their
#' sums, with `score = "sum"`).
#'
#' When all items of a scale are scored 0 and 1 (e.g., incorrect and correct
#' answers), *M* is the item's difficulty (the proportion of 1s), *r*~it~ is
#' the corrected point-biserial correlation, and \eqn{\alpha}{alpha} equals
#' KR-20. The note says so.
#'
#' @param data data frame of item responses
#' @param scales named list of the items of each scale, given as unquoted or
#'   quoted names or tidyselect helpers (e.g., `list(Agreeableness = c(A1, A2,
#'   A3), Conscientiousness = starts_with("C"))`). A named list of character
#'   vectors stored in a variable works too. If `NULL` (default), all columns
#'   of `data` form one scale.
#' @param reverse items to reverse-score before the analysis, selected like
#'   the items in `scales` (e.g., `c(A1, C4, C5)`). Items are reversed within
#'   their scale's range of responses (`item_range`), and they are marked "(R)"
#'   in the table.
#' @param item_range lowest and highest possible responses, used to reverse
#'   items. If `NULL` (default), the lowest and highest responses observed in
#'   each scale are used.
#' @param level `"items"` (default) for item rows followed by each scale's
#'   statistics, or `"scales"` for one row per scale.
#' @param omega if `TRUE` (default), McDonald's
#'   \eqn{\omega_{\text{total}}}{omega total} is shown
#' @param ci confidence level of the interval of \eqn{\alpha}{alpha}
#' @param score `"mean"` (default) or `"sum"`: how scale scores are computed
#' @param flag `NULL` (default) or a number: item-total correlations below
#'   it, and \eqn{\alpha}{alpha}-if-deleted values above the scale's
#'   \eqn{\alpha}{alpha}, are bolded (e.g., `flag = .30`), and the note says
#'   so. `TRUE` is the same as `.30`. This works like `flag` in
#'   [apa_collinearity()].
#' @param format_variable_names if `TRUE` (default), item and scale names are
#'   lightly formatted: underscores become spaces, and the first letter is
#'   capitalized (item codes such as "A1" stay intact). Set to `FALSE` to keep
#'   names exactly as they are.
#' @param digits number of decimal places (unaffected by
#'   `apa7_defaults(accuracy = )`)
#'
#' @references
#' Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987). Statistical
#' inference for coefficient alpha. *Applied Psychological Measurement,
#' 11*(1), 93--103. \doi{10.1177/014662168701100107}
#'
#' McDonald, R. P. (1999). *Test theory: A unified treatment*. Lawrence
#' Erlbaum. \doi{10.4324/9781410601087}
#' @returns `apa7_tbl` (tibble)
#' @export
#' @examples
#' d <- psych::bfi
#' apa_reliability(
#'   d,
#'   scales = list(
#'     Agreeableness = paste0("A", 1:5),
#'     Conscientiousness = paste0("C", 1:5)
#'   ),
#'   reverse = c("A1", "C4", "C5"),
#'   item_range = c(1, 6)
#' )
#'
#' # One row per scale, with items selected by tidyselect helpers
#' apa_reliability(
#'   d,
#'   scales = list(
#'     Agreeableness = A1:A5,
#'     Conscientiousness = starts_with("C")
#'   ),
#'   reverse = c(A1, C4, C5),
#'   item_range = c(1, 6),
#'   level = "scales"
#' )
apa_reliability <- function(
  data,
  scales = NULL,
  reverse = NULL,
  item_range = NULL,
  level = c("items", "scales"),
  omega = TRUE,
  ci = 0.95,
  score = c("mean", "sum"),
  flag = NULL,
  format_variable_names = TRUE,
  digits = 2
) {
  level <- match.arg(level)
  score <- match.arg(score)
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame, not {.obj_type_friendly {data}}.")
  }
  if (!is.numeric(ci) || length(ci) != 1 || is.na(ci) || ci <= 0 || ci >= 1) {
    cli::cli_abort("{.arg ci} must be a number between 0 and 1.")
  }
  if (isTRUE(flag)) {
    flag <- 0.30
  } else if (isFALSE(flag)) {
    flag <- NULL
  }
  if (!is.null(flag) && (!is.numeric(flag) || length(flag) != 1 || is.na(flag))) {
    cli::cli_abort("{.arg flag} must be {.code NULL} or a number (e.g., {.code flag = .30}).")
  }
  # Items can be given unquoted, quoted, or with tidyselect helpers
  scales <- reliability_scales(rlang::enquo(scales), data)
  one_scale <- is.null(scales)
  if (one_scale) {
    scales <- list(Scale = colnames(data))
  }
  reverse <- select_names(rlang::enquo(reverse), data)
  items <- unlist(scales, use.names = FALSE)
  not_numeric <- items[!purrr::map_lgl(data[items], is.numeric)]
  if (length(not_numeric) > 0) {
    cli::cli_abort("Items must be numeric: {.val {not_numeric}} {?is/are} not.")
  }
  too_short <- names(scales)[lengths(scales) < 2]
  if (length(too_short) > 0) {
    cli::cli_abort("Each scale needs at least 2 items: {.val {too_short}} {?has/have} fewer.")
  }

  results <- purrr::imap(scales, \(vars, name) {
    reliability_scale(
      data[vars],
      reverse = intersect(reverse, vars),
      item_range = item_range,
      omega = omega,
      ci = ci,
      score = score
    )
  })

  # Light formatting keeps item codes such as "A1" intact
  label <- function(x) {
    if (format_variable_names) lavaan_label(x) else x
  }
  num <- function(x) align_chr(x, accuracy = .1^digits)
  bounded <- function(x) {
    align_chr(x, accuracy = .1^digits, trim_leading_zeros = TRUE, format_integers = TRUE)
  }
  ci_header <- paste0(round(ci * 100), "% CI")
  scale_labels <- label(names(scales))

  if (level == "scales") {
    rows <- purrr::map(results, \(r) {
      tibble::tibble(
        k = r$k,
        M = r$scale_mean,
        SD = r$scale_sd,
        alpha = r$alpha,
        CI_low = r$alpha_ci[1],
        CI_high = r$alpha_ci[2],
        omega = r$omega,
        mean_r = r$mean_r
      )
    }) |>
      dplyr::bind_rows()
    d <- tibble::tibble(Scale = scale_labels)
    d[["*k*"]] <- as.character(rows$k)
    d[["*M*"]] <- num(rows$M)
    d[["*SD*"]] <- num(rows$SD)
    d[["&alpha;"]] <- bounded(rows$alpha)
    d[[ci_header]] <- ci_md(bounded(rows$CI_low), bounded(rows$CI_high))
    if (omega) {
      d[["*&omega;*~total~"]] <- bounded(rows$omega)
    }
    d[["Mean *r*"]] <- bounded(rows$mean_r)
    first_column <- "Scale"
  } else {
    pieces <- purrr::map2(results, scale_labels, \(r, scale_label) {
      item_label <- label(r$items)
      item_label[r$reversed] <- paste0(item_label[r$reversed], " (R)")
      rit <- bounded(r$item_total)
      drop <- bounded(r$alpha_drop)
      if (!is.null(flag)) {
        low <- r$item_total < flag
        rit[low] <- bold_padded(rit[low])
        high <- r$alpha_drop > r$alpha
        drop[high] <- bold_padded(drop[high])
      }
      item_rows <- tibble::tibble(
        Item = item_label,
        M = num(r$item_mean),
        SD = num(r$item_sd),
        rit = rit,
        drop = drop
      )
      alpha_label <- if (r$binary) "&alpha; (KR-20)" else "&alpha;"
      summary_rows <- tibble::tibble(
        Item = c(
          paste0("Scale ", if (score == "sum") "sum" else "mean", " (*k* = ", r$k, ")"),
          paste0(alpha_label, " [", ci_header, "]"),
          if (omega) "*&omega;*~total~",
          "Mean inter-item *r*"
        ),
        M = c(
          num(r$scale_mean),
          paste0(bounded(r$alpha), " ", ci_md(bounded(r$alpha_ci[1]), bounded(r$alpha_ci[2]))),
          if (omega) bounded(r$omega),
          bounded(r$mean_r)
        ),
        SD = c(num(r$scale_sd), rep(NA_character_, 2 + omega))
      )
      block <- dplyr::bind_rows(item_rows, summary_rows)
      if (!one_scale) {
        block$Item <- paste0("\u2003", block$Item)
        block <- dplyr::bind_rows(tibble::tibble(Item = scale_label), block)
      }
      block
    })
    d <- dplyr::bind_rows(pieces)
    d <- dplyr::mutate(d, dplyr::across(dplyr::everything(), \(v) ifelse(is.na(v), "", v)))
    colnames(d) <- c("Item", "*M*", "*SD*", "*r*~it~", "&alpha; if deleted")
    first_column <- "Item"
  }

  attr(d, "no_format_columns") <- first_column
  attr(d, "apa_note") <- list(
    table = "reliability",
    level = level,
    n = purrr::map_int(results, "n"),
    scales = scale_labels,
    one_scale = one_scale,
    reversed = length(reverse) > 0,
    binary = purrr::map_lgl(results, "binary"),
    omega = omega,
    ci = ci,
    score = score,
    flag = !is.null(flag),
    min_item_total = flag
  )
  as_apa7_tbl(d)
}

#' Item names of each scale
#'
#' `scales` is either a call to `list()` whose elements are selections (e.g.,
#' `list(A = starts_with("A"), B = c(B1, B2))`), evaluated one at a time with
#' tidyselect, or a named list of character vectors stored in a variable.
#' @param quo quosure of `scales`
#' @param data data frame
#' @returns named list of character vectors, or `NULL` if `scales` is `NULL`
#' @keywords internal
#' @noRd
reliability_scales <- function(quo, data, call = rlang::caller_env()) {
  if (rlang::quo_is_null(quo)) {
    return(NULL)
  }
  expr <- rlang::quo_get_expr(quo)
  env <- rlang::quo_get_env(quo)
  if (rlang::is_call(expr, "list", ns = c("", "base"))) {
    selections <- purrr::map(rlang::call_args(expr), \(e) rlang::new_quosure(e, env))
  } else {
    value <- rlang::eval_tidy(quo)
    if (!is.list(value)) {
      value <- NA
    }
    selections <- purrr::map(value, \(v) rlang::quo(!!v))
  }
  if (length(selections) == 0 || is.null(names(selections)) || any(names(selections) == "")) {
    cli::cli_abort(
      "{.arg scales} must be a named list of items, e.g., {.code list(Anxiety = c(a1, a2, a3))} or {.code list(Anxiety = starts_with(\"a\"))}.",
      call = call
    )
  }
  purrr::map(selections, \(s) select_names(s, data, call = call))
}

#' Reliability statistics of one scale
#' @param x data frame of the scale's items
#' @keywords internal
#' @noRd
reliability_scale <- function(x, reverse = character(0), item_range = NULL, omega = TRUE, ci = 0.95, score = "mean") {
  x <- as.data.frame(x)
  # Respondents who answered every item
  x <- x[stats::complete.cases(x), , drop = FALSE]
  n <- nrow(x)
  k <- ncol(x)
  if (n < 3) {
    cli::cli_abort("A scale needs at least 3 respondents with complete responses, not {n}.")
  }

  if (length(reverse) > 0) {
    rng <- if (is.null(item_range)) range(unlist(x), na.rm = TRUE) else item_range
    for (v in reverse) {
      x[[v]] <- rng[1] + rng[2] - x[[v]]
    }
  }

  a <- suppressWarnings(suppressMessages(
    psych::alpha(x, check.keys = FALSE, warnings = FALSE)
  ))
  alpha <- a$total$raw_alpha

  # Feldt's interval
  p <- 1 - ci
  df1 <- n - 1
  df2 <- (n - 1) * (k - 1)
  alpha_ci <- c(
    1 - (1 - alpha) * stats::qf(1 - p / 2, df1, df2),
    1 - (1 - alpha) * stats::qf(p / 2, df1, df2)
  )

  omega_total <- NA_real_
  if (omega) {
    f <- tryCatch(
      suppressWarnings(suppressMessages(psych::fa(x, nfactors = 1, fm = "minres"))),
      error = function(e) NULL
    )
    if (!is.null(f)) {
      loadings <- as.numeric(f$loadings)
      omega_total <- sum(loadings)^2 / (sum(loadings)^2 + sum(f$uniquenesses))
    }
  }

  scores <- if (score == "sum") rowSums(x) else rowMeans(x)
  list(
    items = colnames(x),
    reversed = colnames(x) %in% reverse,
    n = n,
    k = k,
    item_mean = colMeans(x),
    item_sd = vapply(x, stats::sd, numeric(1)),
    item_total = a$item.stats$r.drop,
    alpha_drop = a$alpha.drop$raw_alpha,
    alpha = alpha,
    alpha_ci = alpha_ci,
    omega = omega_total,
    mean_r = a$total$average_r,
    scale_mean = mean(scores),
    scale_sd = stats::sd(scores),
    binary = all(unlist(x) %in% c(0, 1))
  )
}

#' @keywords internal
#' @noRd
reliability_note_sentences <- function(record) {
  sentences <- character(0)
  n <- record$n
  if (length(unique(n)) == 1) {
    sentences <- c(
      sentences,
      paste0(
        "*N* = ", format(n[1], big.mark = ","),
        if (record$one_scale) " respondents with complete responses." else " respondents with complete responses on each scale."
      )
    )
  } else {
    sentences <- c(
      sentences,
      paste0(
        "Each scale uses the respondents with complete responses on its items (",
        paste0(record$scales, ": *n* = ", format(n, big.mark = ",", trim = TRUE), collapse = "; "),
        ")."
      )
    )
  }
  if (isTRUE(record$reversed) && record$level == "items") {
    sentences <- c(sentences, "Items marked (R) were reverse-scored.")
  } else if (isTRUE(record$reversed)) {
    sentences <- c(sentences, "Reverse-keyed items were reverse-scored.")
  }
  sentences <- c(
    sentences,
    paste0(
      "Scale scores are the ",
      if (record$score == "sum") "sums" else "means",
      " of the items."
    )
  )
  if (any(record$binary)) {
    sentences <- c(
      sentences,
      paste0(
        if (all(record$binary)) "Items are" else "Some items are",
        " scored 0 or 1: for these, ",
        if (record$level == "items") "*M* is item difficulty (the proportion of 1s), *r*~it~ is the corrected point-biserial correlation, and " else "",
        "&alpha; equals KR-20."
      )
    )
  }
  ci <- paste0(round(record$ci * 100), "%")
  defs <- c(
    if (record$level == "items") c(
      "*r*~it~" = paste0("corrected item", intToUtf8(8211), "total correlation (the correlation of the item with the sum of the other items)"),
      "&alpha; if deleted" = "Cronbach's &alpha; without the item"
    ),
    "&alpha;" = "Cronbach's &alpha;",
    "CI" = paste0(ci, " confidence interval ", cite("feldtStatisticalInferenceCoefficient1987")),
    if (isTRUE(record$omega)) c("*&omega;*~total~" = paste0("McDonald's &omega;~total~ ", cite("mcdonaldTestTheoryUnified1999"), " from a one-factor model")),
    if (record$level == "scales") c("*k*" = "number of items", "Mean *r*" = "mean inter-item correlation")
  )
  sentences <- c(sentences, abbreviation_sentence(defs))
  if (isTRUE(record$flag) && record$level == "items") {
    cutoff <- sub("^0", "", format(record$min_item_total, nsmall = 2))
    sentences <- c(
      sentences,
      paste0(
        "Bold values are item", intToUtf8(8211), "total correlations below ", cutoff,
        " and values of &alpha; if deleted above the scale's &alpha;."
      )
    )
  }
  sentences
}
