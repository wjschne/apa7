#' Collinearity diagnostics table
#'
#' Makes an APA-style table of the variance inflation factor (*VIF*) and
#' tolerance of each predictor in a regression model, as computed by
#' [performance::check_collinearity()].
#'
#' The *VIF* of a predictor is the factor by which the variance of its
#' coefficient is inflated by its correlations with the other predictors.
#' Tolerance is 1/*VIF*, the proportion of the predictor's variance that the
#' other predictors do not explain. Common rules of thumb treat *VIF*s above 5
#' or 10 as signs of problematic collinearity; `flag` bolds them.
#'
#' For terms with more than 1 degree of freedom (e.g., factors with more than
#' two levels), *VIF* is the generalized *VIF* (Fox & Monette, 1992), and
#' `"SE_factor"` is \eqn{GVIF^{1/(2df)}}{GVIF^(1/(2df))}, which is comparable
#' across terms with different degrees of freedom. The note names these terms.
#'
#' In models with interactions, the *VIF*s of the interaction and its
#' component terms are high in part because product terms are correlated with
#' their components. This kind of collinearity does not affect the test of the
#' interaction, and centering the predictors reduces it. When any of these
#' *VIF*s is at or above `flag` (or 5, if `flag` is `NULL`), the note says so.
#'
#' Models with several parts (e.g., the count and zero-inflation models of
#' `pscl::zeroinfl()` or `glmmTMB::glmmTMB()`) get a heading row for each
#' part.
#'
#' @param fit a regression model with at least two predictors (any model that
#'   [performance::check_collinearity()] accepts, e.g., `lm`, `glm`, `lme4`,
#'   `glmmTMB`, or `pscl` models), or the output of
#'   [performance::check_collinearity()]
#' @param columns columns to display, in order: any of `"VIF"`, `"VIF_CI"`
#'   (confidence interval of *VIF*), `"SE_factor"` (\eqn{\sqrt{VIF}}{sqrt(VIF)},
#'   the factor by which the standard error is inflated), `"Tolerance"`, and
#'   `"Tolerance_CI"`
#' @param flag `NULL` (default) or a number: *VIF*s at or above it are bolded
#'   (e.g., `flag = 5` or `flag = 10`), and the note says so
#' @param ci confidence level of the intervals
#' @param format_variable_names if `TRUE` (default), term names are formatted
#'   as in [apa_parameters()] (e.g., "factor(cyl)" becomes "Cyl", and "wt:hp"
#'   becomes "Wt \eqn{\times}{x} Hp"). Set to `FALSE` to keep the names exactly as
#'   [performance::check_collinearity()] gives them.
#' @param digits number of decimal places (unaffected by
#'   `apa7_defaults(accuracy = )`)
#' @param ... arguments passed to [performance::check_collinearity()] (e.g.,
#'   `component`)
#'
#' @returns `apa7_tbl` (tibble)
#' @references Fox, J., & Monette, G. (1992). Generalized collinearity
#'   diagnostics. *Journal of the American Statistical Association, 87*(417),
#'   178--183. \doi{10.1080/01621459.1992.10475190}
#' @export
#' @examples
#' fit <- lm(mpg ~ wt + hp + disp + factor(cyl), data = mtcars)
#' apa_collinearity(fit)
#'
#' # Bold VIFs of 10 or more, and show the note
#' apa_collinearity(fit, flag = 10) |>
#'   apa_note()
apa_collinearity <- function(
  fit,
  columns = c("VIF", "VIF_CI", "Tolerance"),
  flag = NULL,
  ci = 0.95,
  format_variable_names = TRUE,
  digits = 2,
  ...
) {
  allowed <- c("VIF", "VIF_CI", "SE_factor", "Tolerance", "Tolerance_CI")
  unknown <- setdiff(columns, allowed)
  if (length(columns) == 0 || length(unknown) > 0) {
    cli::cli_abort(
      "{.arg columns} must contain only {.or {.val {allowed}}}{if (length(unknown) > 0) paste0(', not ', paste0('\"', unknown, '\"', collapse = ', ')) else ''}."
    )
  }
  if (!is.null(flag) && !(is.numeric(flag) && length(flag) == 1 && !is.na(flag))) {
    cli::cli_abort("{.arg flag} must be {.code NULL} or a single number (e.g., {.code flag = 10}).")
  }

  if (inherits(fit, "check_collinearity")) {
    cc <- fit
    fit <- NULL
  } else {
    cc <- tryCatch(
      suppressMessages(performance::check_collinearity(fit, ci = ci, ...)),
      warning = function(w) {
        cli::cli_abort(c(
          "Collinearity diagnostics could not be computed.",
          i = conditionMessage(w)
        ), call = rlang::caller_env(4))
      }
    )
    if (is.null(cc) || nrow(cc) == 0) {
      cli::cli_abort("Collinearity diagnostics need a model with at least two predictors.")
    }
  }
  ci <- attr(cc, "ci", exact = TRUE) %||% ci

  d_cc <- as.data.frame(cc)
  # Degrees of freedom of each term (for generalized VIFs)
  term_df <- attr(cc, "data", exact = TRUE)$df
  if (is.null(term_df) || length(term_df) != nrow(d_cc)) {
    term_df <- rep(1, nrow(d_cc))
  }
  if (!("Component" %in% colnames(d_cc))) {
    d_cc$Component <- "conditional"
  }

  labels <- d_cc$Term
  if (format_variable_names) {
    labels <- collinearity_term_labels(labels)
  }

  num <- function(v) align_chr(v, accuracy = .1^digits)
  bounded <- function(v) {
    align_chr(v, accuracy = .1^digits, trim_leading_zeros = TRUE, format_integers = TRUE)
  }
  ci_header <- paste0(round(ci * 100), "% CI")

  vif <- num(d_cc$VIF)
  flagged <- if (is.null(flag)) rep(FALSE, nrow(d_cc)) else d_cc$VIF >= flag
  vif[flagged] <- bold_padded(vif[flagged])

  d <- tibble::tibble(Variable = labels)
  headers <- c(
    VIF = "*VIF*",
    VIF_CI = paste0(ci_header, " for *VIF*"),
    SE_factor = if (any(term_df > 1)) "*VIF*^1/(2*df*)^" else "&radic;*VIF*",
    Tolerance = "Tolerance",
    Tolerance_CI = paste0(ci_header, " for tolerance")
  )
  for (col in columns) {
    d[[headers[[col]]]] <- switch(
      col,
      VIF = vif,
      VIF_CI = ci_md(num(d_cc$VIF_CI_low), num(d_cc$VIF_CI_high)),
      SE_factor = num(d_cc$SE_factor),
      Tolerance = bounded(d_cc$Tolerance),
      Tolerance_CI = ci_md(bounded(d_cc$Tolerance_CI_low), bounded(d_cc$Tolerance_CI_high))
    )
  }

  # A heading row for each part of models with several parts
  components <- unique(d_cc$Component)
  if (length(components) > 1) {
    counts <- !is.null(fit) && inherits(fit, c("zeroinfl", "hurdle"))
    headings <- c(
      conditional = if (counts) "Count model" else "Conditional model",
      count = "Count model",
      `zero inflated` = if (!is.null(fit) && inherits(fit, "hurdle")) "Zero hurdle model" else "Zero-inflation model",
      zero_inflated = if (!is.null(fit) && inherits(fit, "hurdle")) "Zero hurdle model" else "Zero-inflation model",
      dispersion = "Dispersion model"
    )
    pieces <- purrr::map(components, \(component) {
      block <- d[d_cc$Component == component, ]
      block$Variable <- paste0("\u2003", block$Variable)
      heading <- if (component %in% names(headings)) headings[[component]] else component
      dplyr::bind_rows(tibble::tibble(Variable = heading), block)
    })
    d <- dplyr::bind_rows(pieces)
    d <- dplyr::mutate(d, dplyr::across(dplyr::everything(), \(v) ifelse(is.na(v), "", v)))
  }

  attr(d, "no_format_columns") <- "Variable"
  attr(d, "apa_note") <- list(
    table = "collinearity",
    columns = columns,
    ci = ci,
    flag = if (any(flagged)) flag,
    generalized = unique(labels[term_df > 1]),
    interaction = interaction_inflated(d_cc$Term, d_cc$VIF, flag %||% 5)
  )
  as_apa7_tbl(d)
}

#' Format the term names of check_collinearity()
#'
#' Removes `factor()`, `as.factor()`, `ordered()`, and `I()` wrappers and
#' writes interactions with a multiplication sign.
#' @keywords internal
#' @noRd
collinearity_term_labels <- function(terms) {
  formatter <- the$column_formats$Parameter@formatter
  purrr::map_chr(terms, \(term) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    parts <- sub("^(as\\.)?(factor|ordered)\\((.+)\\)$", "\\3", parts)
    parts <- sub("^I\\((.+)\\)$", "\\1", parts)
    paste(formatter(parts), collapse = " \u00d7 ")
  })
}

#' Are interaction terms or their components inflated?
#'
#' True when a VIF of an interaction term, or of a term in an interaction, is
#' at or above `cutoff`. With centered predictors, these VIFs are usually
#' small, and the note need not explain them.
#' @keywords internal
#' @noRd
interaction_inflated <- function(terms, vif, cutoff) {
  interactions <- terms[grepl(":", terms, fixed = TRUE)]
  if (length(interactions) == 0) {
    return(FALSE)
  }
  components <- unique(unlist(strsplit(interactions, ":", fixed = TRUE)))
  involved <- terms %in% c(interactions, components)
  any(vif[involved] >= cutoff, na.rm = TRUE)
}

#' @keywords internal
#' @noRd
collinearity_note_sentences <- function(record) {
  sentences <- character(0)
  columns <- record$columns
  if (length(record$generalized) > 0) {
    sentences <- c(
      sentences,
      paste0(
        "For ",
        and_list(record$generalized),
        ", which ",
        if (length(record$generalized) == 1) "has" else "have",
        " more than 1 *df*, *VIF* is the generalized *VIF* ",
        cite("foxGeneralizedCollinearityDiagnostics1992"),
        if ("SE_factor" %in% columns) {
          ", and *VIF*^1/(2*df*)^ is comparable across terms with different *df*"
        } else {
          ""
        },
        "."
      )
    )
  }
  if (isTRUE(record$interaction)) {
    sentences <- c(
      sentences,
      "The *VIF*s of interaction terms and their components are high partly because product terms are correlated with their components; this does not affect the tests of the interactions."
    )
  }
  if (any(c("Tolerance", "Tolerance_CI") %in% columns)) {
    sentences <- c(
      sentences,
      "Tolerance = 1/*VIF*, the proportion of a predictor's variance not explained by the other predictors."
    )
  }
  abbreviations <- c(`*VIF*` = "variance inflation factor")
  if (any(c("VIF_CI", "Tolerance_CI") %in% columns)) {
    abbreviations <- c(abbreviations, CI = "confidence interval")
  }
  sentences <- c(sentences, abbreviation_sentence(abbreviations))
  if (!is.null(record$flag)) {
    sentences <- c(
      sentences,
      paste0("*VIF*s of ", record$flag, " or more are bolded.")
    )
  }
  sentences
}
