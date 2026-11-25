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

#' Correlations of a data frame, computed by correlation::correlation()
#'
#' The columns get internal names while the correlations are computed, so
#' variable names with spaces or symbols work for every kind of correlation
#' (partial and multilevel correlations fit models with formulas, which
#' cannot use such names). The original names are restored in the result.
#' @param data data frame
#' @param method,p_adjust,partial passed to [correlation::correlation()]
#' @param ... passed to [correlation::correlation()]
#' @returns an `easycorrelation` object with the original variable names
#' @keywords internal
#' @noRd
correlation_from_data <- function(
  data,
  method = "pearson",
  p_adjust = "none",
  partial = FALSE,
  ...,
  call = rlang::caller_env()
) {
  dots <- list(...)
  # Arguments of psych::corTest(), which apa_cor() used in apa7 0.2.0
  if ("adjust" %in% names(dots)) {
    cli::cli_warn(c(
      "{.arg adjust} is deprecated in {.fn apa_cor}; use {.arg p_adjust} instead.",
      i = "{.fn apa_cor} now computes correlations with {.fn correlation::correlation}."
    ))
    p_adjust <- dots$adjust
    dots$adjust <- NULL
  }
  if ("use" %in% names(dots)) {
    use <- dots$use
    dots$use <- NULL
    if (is.character(use) && grepl("^complete", use)) {
      data <- stats::na.omit(data)
    }
    cli::cli_warn(c(
      "{.arg use} is deprecated in {.fn apa_cor}.",
      i = "Correlations use all pairwise complete observations. For listwise deletion, remove incomplete rows first (e.g., with {.fn stats::na.omit})."
    ))
  }
  if ("y" %in% names(dots)) {
    cli::cli_abort(
      "Correlations between two sets of variables ({.arg y}) are not supported.",
      call = call
    )
  }

  data <- as.data.frame(dplyr::ungroup(tibble::as_tibble(data)))
  if (ncol(data) < 2) {
    cli::cli_abort("{.arg data} needs at least two variables to correlate.", call = call)
  }
  logical_columns <- vapply(data, is.logical, logical(1))
  data[logical_columns] <- lapply(data[logical_columns], as.numeric)

  original <- colnames(data)
  internal <- paste0("apa7var", seq_along(original))
  colnames(data) <- internal
  lookup <- stats::setNames(original, internal)
  rename <- function(x) unname(ifelse(x %in% internal, lookup[x], x))

  x <- do.call(
    correlation::correlation,
    c(list(data, method = method, p_adjust = p_adjust, partial = partial), dots)
  )
  x$Parameter1 <- rename(x$Parameter1)
  x$Parameter2 <- rename(x$Parameter2)
  d <- attr(x, "data")
  if (!is.null(d)) {
    colnames(d) <- rename(colnames(d))
    attr(x, "data") <- d
  }
  x
}

#' Pad correlations so that they line up on the decimal point
#'
#' Without star columns, each correlation column is one column of text. Its
#' values are padded on the left (as [align_chr()] does) so that the decimal
#' points share one position, and then on the right so that every cell has
#' the same width, so that the column can be centered (as the summary columns
#' are). Markdown bold markers are added after the left padding, and the
#' diagonal's dash starts where the decimal points are.
#' @param R character matrix of formatted correlations, with `NA` above the
#'   diagonal and a dash on it
#' @keywords internal
#' @noRd
pad_correlations <- function(R) {
  font <- if (isTRUE(the$width_aware_padding)) the$font_family else NA
  for (j in seq_len(ncol(R))) {
    i <- which(!is.na(R[, j]) & seq_len(nrow(R)) != j)
    if (length(i) == 0) {
      next
    }
    plain <- c(gsub("**", "", R[i, j], fixed = TRUE), ".")
    padded <- align_chr(plain, side = "left")
    prefix <- substr(padded, 1, nchar(padded) - nchar(plain))
    cells <- paste0(prefix, c(R[i, j], R[j, j]))
    cells <- num_pad(cells, pad_left = FALSE, padding_character = "\u2007", font_family = font)
    R[i, j] <- cells[seq_along(i)]
    R[j, j] <- cells[length(cells)]
  }
  R
}
