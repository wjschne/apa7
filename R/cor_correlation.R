#' What apa_cor() needs from correlation::correlation() output
#'
#' @param x an `easycorrelation` object
#' @returns list with `ct` (`r` and `p` matrices, like [psych::corTest()]),
#'   `vars` (variable names in table order), `data` (the data the
#'   correlations were computed from, for the summary columns, or NULL), and
#'   `sentences` (general note sentences: the kind of correlation and the
#'   *p*-value adjustment)
#' @keywords internal
#' @noRd
correlation_parts <- function(x) {
  if ("Group" %in% colnames(x)) {
    cli::cli_abort(c(
      "{.fn apa_cor} does not yet support correlations computed by group.",
      i = "Make one table for each group, e.g., with {.fn correlation::correlation} on each group's data."
    ))
  }
  if (!("p" %in% colnames(x))) {
    cli::cli_abort(
      "{.fn apa_cor} needs correlations with {.emph p}-values. Bayesian correlations are not yet supported."
    )
  }
  estimate <- attr(x, "coefficient_name")
  if (is.null(estimate) || !(estimate %in% colnames(x))) {
    estimate <- intersect(c("r", "rho", "tau"), colnames(x))[1]
  }
  if (is.na(estimate)) {
    cli::cli_abort("Cannot find the correlations in {.arg data}.") # nocov
  }

  vars <- unique(c(x$Parameter1, x$Parameter2))
  k <- length(vars)
  if (nrow(x) != k * (k - 1) / 2) {
    cli::cli_abort(c(
      "{.fn apa_cor} needs the correlations among every pair of variables.",
      i = "Correlations between two sets of variables (e.g., {.code correlation(x, y)}) are not supported."
    ))
  }
  i <- match(x$Parameter1, vars)
  j <- match(x$Parameter2, vars)
  fill <- function(values, diagonal) {
    m <- matrix(NA_real_, k, k, dimnames = list(vars, vars))
    m[cbind(i, j)] <- values
    m[cbind(j, i)] <- values
    diag(m) <- diagonal
    m
  }
  ct <- list(r = fill(x[[estimate]], 1), p = fill(x$p, 0))

  data <- attr(x, "data")
  if (!is.null(data) && all(vars %in% colnames(data))) {
    data <- tibble::as_tibble(data)[vars]
  } else {
    data <- NULL
  }

  list(
    ct = ct,
    vars = vars,
    data = data,
    sentences = correlation_note_sentences(
      method = attr(x, "method"),
      method_label = unique(x$Method),
      partial = isTRUE(attr(x, "partial")),
      p_adjust = attr(x, "p_adjust")
    )
  )
}

#' General note sentences for correlation::correlation() output
#' @keywords internal
#' @noRd
correlation_note_sentences <- function(
  method = "pearson",
  method_label = NULL,
  partial = FALSE,
  p_adjust = "none"
) {
  sentences <- character(0)
  is_pearson <- identical(tolower(method), "pearson")
  if (partial || !is_pearson) {
    label <- if (length(method_label) == 1) method_label else "correlation"
    sentences <- c(
      sentences,
      paste0(
        "Values are ",
        if (partial) "partial ",
        label,
        "s",
        if (partial) ", each controlling for all other variables in the table",
        "."
      )
    )
  }
  if (length(p_adjust) == 1 && !is.na(p_adjust) && !(p_adjust %in% c("none", ""))) {
    adjustment <- switch(
      p_adjust,
      holm = paste0("Holm's method ", cite("holmSimpleSequentiallyRejective1979")),
      hochberg = "Hochberg's method",
      hommel = "Hommel's method",
      bonferroni = "the Bonferroni correction",
      BH = ,
      fdr = paste0("the Benjamini", intToUtf8(8211), "Hochberg false discovery rate method ", cite("benjaminiControllingFalseDiscovery1995")),
      BY = paste0("the Benjamini", intToUtf8(8211), "Yekutieli method"),
      p_adjust
    )
    sentences <- c(
      sentences,
      paste0("*p*-values are adjusted for multiple comparisons with ", adjustment, ".")
    )
  }
  sentences
}
