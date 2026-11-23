#' @param terms for generalized additive models (`mgcv::gam`), which terms to
#'   show: `"all"` (default) for the parametric terms and the smooth terms
#'   under their own headings, `"parametric"`, or `"smooth"`
#' @details
#' For generalized additive models fit with `mgcv::gam()` or `mgcv::bam()`,
#' the table has the parametric terms (coefficients, standard errors, and
#' their *t* or *z* tests) and the smooth terms (effective degrees of freedom
#' and their approximate *F* or \eqn{\chi^2}{chi-square} tests), as reported
#' by `summary()`, under the headings "Parametric terms" and "Smooth terms".
#' Smooth terms are labeled by their type and variables (e.g., "s(Age)" or
#' "te(Longitude, Latitude)"), and smooths of a factor-by variable are labeled
#' by level (e.g., "s(Age): Group = Treatment"). Coefficients of log and logit
#' links are exponentiated by default, as for [stats::glm()] models.
#' [apa_note()] names the method used to estimate the smoothing parameters.
#' @export
#' @rdname apa_parameters
#' @examples
#' \donttest{
#' if (requireNamespace("mgcv", quietly = TRUE)) {
#'   mgcv::gam(mpg ~ am + s(hp, k = 5) + s(wt, k = 5), data = mtcars,
#'             method = "REML") |>
#'     apa_parameters()
#' }
#' }
apa_parameters.gam <- function(
  fit,
  predictor_parameters = c(
    "Parameter",
    "Coefficient",
    "SE",
    "Exp_Coefficient",
    "t",
    "z",
    "df_error",
    "edf",
    "F",
    "Chi2",
    "p"
  ),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  exponentiate = NULL,
  terms = c("all", "parametric", "smooth"),
  ...
) {
  terms <- rlang::arg_match(terms)
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }
  names(predictor_parameters) <- NULL
  s <- summary(fit)
  name_formatter <- column_formats$Parameter@formatter

  # Parametric terms
  p_table <- s$p.table
  stat_name <- if (any(grepl("^t ", colnames(p_table)))) "t" else "z"
  d_p <- tibble::tibble(
    Parameter = name_formatter(gam_parametric_labels(fit, rownames(p_table))),
    Coefficient = p_table[, 1],
    SE = p_table[, 2]
  )
  d_p[[stat_name]] <- p_table[, 3]
  if (stat_name == "t") {
    d_p$df_error <- s$residual.df
  }
  d_p$p <- p_table[, 4]

  # Smooth terms
  s_table <- s$s.table
  d_s <- NULL
  smooth_stat <- NULL
  if (!is.null(s_table) && nrow(s_table) > 0) {
    smooth_stat <- if ("Chi.sq" %in% colnames(s_table)) "Chi2" else "F"
    d_s <- tibble::tibble(
      Parameter = gam_smooth_labels(fit, rownames(s_table), name_formatter),
      edf = s_table[, "edf"]
    )
    d_s[[smooth_stat]] <- s_table[, if (smooth_stat == "Chi2") "Chi.sq" else "F"]
    d_s$p <- s_table[, ncol(s_table)]
  }

  # Exponentiated coefficients (e.g., OR) for log and logit links
  link <- stats::family(fit)$link
  if (is.null(exponentiate)) {
    exponentiate <- link %in% c("logit", "log")
  }
  if (isTRUE(exponentiate)) {
    d_p <- add_exp_columns(d_p, predictor_parameters)
    column_formats <- label_exp_columns(column_formats, exp_coefficient_label(fit))
  }

  pieces <- list()
  if (terms %in% c("all", "parametric")) {
    pieces$parametric <- d_p
  }
  if (terms %in% c("all", "smooth") && !is.null(d_s)) {
    pieces$smooth <- d_s
  }
  if (length(pieces) == 0) {
    cli::cli_abort("The model has no smooth terms.")
  }
  if (length(pieces) > 1) {
    headings <- c(parametric = "Parametric terms", smooth = "Smooth terms")
    pieces <- purrr::imap(pieces, \(piece, part) {
      piece$Parameter <- paste0("\u2003", piece$Parameter)
      dplyr::bind_rows(tibble::tibble(Parameter = headings[[part]]), piece)
    })
  }
  d_b <- dplyr::bind_rows(pieces)

  # Names were formatted above
  column_formats$Parameter@formatter <- \(x, ...) x

  format_apa_parameters(
    d_b,
    predictor_parameters = predictor_parameters,
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    t_with_df = t_with_df,
    note = list(
      gam = list(
        method = fit$method,
        smooth = !is.null(d_s) && terms != "parametric",
        smooth_stat = smooth_stat
      )
    )
  ) |>
    labels_formatted()
}

#' @export
#' @rdname apa_parameters
apa_parameters.bam <- apa_parameters.gam

#' @details
#' For generalized additive models (`mgcv::gam` and `mgcv::bam`), the
#' metrics are the adjusted *R*^2^ (`"R2_adjusted"`), the percentage of the
#' deviance explained (`"Deviance_explained"`), the number of observations
#' (`"n"`), `"AIC"`, and `"BIC"`, as reported by `summary()`. The defaults are
#' `"R2_adjusted"`, `"Deviance_explained"`, and `"AIC"`.
#' @export
#' @rdname apa_performance
apa_performance.gam <- function(
  fit,
  metrics = c("R2_adjusted", "Deviance_explained", "AIC"),
  column_formats = NULL
) {
  allowed <- c("R2_adjusted", "Deviance_explained", "n", "AIC", "BIC")
  if (any(metrics == "all")) {
    metrics <- allowed
  }
  unknown <- setdiff(metrics, allowed)
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown {cli::qty(unknown)} metric{?s} for GAMs: {.val {unknown}}. Use any of {.val {allowed}}.")
  }
  s <- summary(fit)
  num <- function(v) align_chr(v, accuracy = the$accuracy)
  d <- tibble::tibble(.rows = 1)
  for (m in metrics) {
    switch(
      m,
      R2_adjusted = d[["*R*^2^~adj~"]] <- align_chr(s$r.sq, accuracy = the$accuracy, trim_leading_zeros = TRUE),
      Deviance_explained = d[["Deviance explained (%)"]] <- align_chr(100 * s$dev.expl, accuracy = 0.1),
      n = d[["*n*"]] <- format(s$n, big.mark = ","),
      AIC = d[["*AIC*"]] <- num(stats::AIC(fit)),
      BIC = d[["*BIC*"]] <- num(stats::BIC(fit))
    )
  }
  as_apa7_tbl(d)
}

#' @export
#' @rdname apa_performance
apa_performance.bam <- apa_performance.gam

#' Labels of parametric terms
#'
#' Factor coefficients (e.g., `gb`) are written as `g [b]`, as
#' `parameters::model_parameters()` writes them, so that the name formatter
#' treats them like those of other models.
#' @param fit a gam model
#' @param names names of the parametric coefficients
#' @keywords internal
#' @noRd
gam_parametric_labels <- function(fit, names) {
  data <- fit$model
  factors <- colnames(data)[vapply(data, \(x) is.factor(x) || is.character(x) || is.logical(x), logical(1))]
  vapply(names, \(name) {
    for (f in factors) {
      if (startsWith(name, f)) {
        level <- substring(name, nchar(f) + 1)
        values <- data[[f]]
        lv <- if (is.factor(values)) levels(values) else unique(as.character(values))
        if (level %in% lv) {
          return(paste0(f, " [", level, "]"))
        }
      }
    }
    name
  }, character(1), USE.NAMES = FALSE)
}

#' Labels of smooth terms
#'
#' The smooth's type and its formatted variables (e.g., "s(Age)" or
#' "te(Longitude, Latitude)"), with the level of a factor-by variable (e.g.,
#' "s(Age): Group = Treatment").
#' @param fit a gam model
#' @param smooth_labels row names of the smooth table of `summary(fit)`
#' @param name_formatter function that formats variable names
#' @keywords internal
#' @noRd
gam_smooth_labels <- function(fit, smooth_labels, name_formatter) {
  smooths <- stats::setNames(fit$smooth, vapply(fit$smooth, \(s) s$label, character(1)))
  vapply(smooth_labels, \(label) {
    sm <- smooths[[label]]
    if (is.null(sm)) {
      return(label) # nocov
    }
    type <- sub("\\(.*$", "", label)
    vars <- paste(name_formatter(sm$term), collapse = ", ")
    out <- paste0(type, "(", vars, ")")
    by <- sm$by
    if (!is.null(by) && !identical(by, "NA")) {
      by_label <- name_formatter(by)
      if (!is.null(sm$by.level)) {
        out <- paste0(out, ": ", by_label, " = ", sm$by.level)
      } else {
        out <- paste0(out, " \u00d7 ", by_label)
      }
    }
    out
  }, character(1), USE.NAMES = FALSE)
}

#' Note sentences of GAM tables
#' @keywords internal
#' @noRd
gam_note_sentences <- function(gam) {
  if (length(gam) == 0 || !isTRUE(gam$smooth)) {
    return(character(0))
  }
  methods <- c(
    REML = "restricted maximum likelihood (REML)",
    ML = "maximum likelihood (ML)",
    GCV.Cp = "generalized cross-validation (GCV)",
    GCV = "generalized cross-validation (GCV)",
    GACV.Cp = "generalized approximate cross-validation (GACV)",
    "P-REML" = "penalized quasi-likelihood REML",
    "P-ML" = "penalized quasi-likelihood ML",
    fREML = "fast restricted maximum likelihood (fREML)",
    UBRE = "the unbiased risk estimator (UBRE)"
  )
  method <- gam$method %||% ""
  method_text <- if (method %in% names(methods)) methods[[method]] else method
  stat <- if (identical(gam$smooth_stat, "Chi2")) "*&chi;*^2^" else "*F*"
  c(
    if (nzchar(method_text)) paste0("Smoothing parameters were estimated by ", method_text, "."),
    paste0("Smooth terms are tested with approximate ", stat, " tests.")
  )
}
