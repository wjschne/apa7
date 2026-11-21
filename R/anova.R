#' Format ANOVA tables in APA style
#'
#' Makes an analysis of variance source table with sums of squares, degrees of freedom, mean squares, *F* tests, *p*-values, and effect sizes. Values come from [parameters::model_parameters()], and effect sizes come from the `effectsize` package (e.g., [effectsize::eta_squared()]).
#'
#' @param fit an `aov` or `lm` model, a repeated-measures `aov` model with an `Error()` term (class `aovlist`), an `afex` model (class `afex_aov`), or an ANOVA table from [stats::anova()] or `car::Anova()`.
#' @param anova_parameters columns to display. `"Effect_Size"` stands for whichever effect size `effect_size`, `partial`, and `generalized` select, and `"CI"` adds its confidence interval as \[*LL*, *UL*\]. Columns not in the ANOVA table are silently dropped.
#' @param effect_size effect size to display: `"eta"` (*&eta;*^2^, default), `"omega"` (*&omega;*^2^), or `"epsilon"` (*&epsilon;*^2^). If `NULL`, no effect size is computed.
#' @param partial if `TRUE` (default), partial effect sizes (e.g., *&eta;*^2^~p~) are displayed. For one-way designs, partial and non-partial effect sizes are the same.
#' @param generalized if `TRUE`, generalized *&eta;*^2^ (*&eta;*^2^~G~) is displayed instead, which is comparable across between- and within-subjects designs. Only for `effect_size = "eta"`.
#' @param type type of sums of squares for `aov` and `lm` models: `1` (sequential, default, as in [stats::anova()]), `2`, or `3`. Types 2 and 3 are computed with `car::Anova()`. Type 3 tests are interpretable only when factors have sum-to-zero contrasts (e.g., `contr.sum`) and covariates are centered. Ignored for ANOVA tables, which already have a type.
#' @param ci confidence level for the effect size interval, shown if `anova_parameters` includes `"CI"`. Intervals are two-sided, so a 90% interval excludes 0 when the *F* test is significant at *&alpha;* = .05, and its lower bound is 0 otherwise. Intervals are not shown for generalized *&eta;*^2^, because those computed by `effectsize` can exclude the point estimate.
#' @param residual_label label for residual (error) rows
#' @param starred columns to star with significant *p*-values (e.g., `"F"`)
#' @param bolded columns to bold, if significant
#' @param column_formats column_formats object to format columns. If NULL, the default column_formats is used.
#' @param ... further arguments passed to [parameters::model_parameters()]
#'
#' @details
#' For repeated-measures models fit with `aov(y ~ a * b + Error(id / b))`, each error stratum is introduced by a heading row ("Between subjects" or "Within subjects"), and each stratum's error term follows its effects.
#'
#' Model comparisons (e.g., `anova(m1, m2)`) are not ANOVA source tables. Use [apa_performance_comparison()] for them.
#'
#' @return `apa7_tbl` (tibble)
#' @export
#'
#' @examples
#' d <- mtcars
#' d$cyl <- factor(d$cyl)
#' d$am <- factor(d$am)
#' fit <- aov(mpg ~ cyl * am, data = d)
#' apa_anova(fit)
#'
#' # Omega squared with a 90% confidence interval
#' apa_anova(
#'   fit,
#'   anova_parameters = c(
#'     "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
#'     "Effect_Size", "CI"
#'   ),
#'   effect_size = "omega",
#'   ci = .90
#' )
#'
#' # Type 2 sums of squares (requires the car package)
#' if (requireNamespace("car", quietly = TRUE)) {
#'   apa_anova(fit, type = 2)
#' }
apa_anova <- function(
  fit,
  anova_parameters = c(
    "Parameter",
    "Sum_Squares",
    "df",
    "Mean_Square",
    "F",
    "p",
    "Effect_Size"
  ),
  effect_size = c("eta", "omega", "epsilon"),
  partial = TRUE,
  generalized = FALSE,
  type = NULL,
  ci = 0.95,
  residual_label = "Error",
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  ...
) {
  UseMethod("apa_anova")
}

#' @export
#' @rdname apa_anova
apa_anova.lm <- function(
  fit,
  anova_parameters = c(
    "Parameter",
    "Sum_Squares",
    "df",
    "Mean_Square",
    "F",
    "p",
    "Effect_Size"
  ),
  effect_size = c("eta", "omega", "epsilon"),
  partial = TRUE,
  generalized = FALSE,
  type = NULL,
  ci = 0.95,
  residual_label = "Error",
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  ...
) {
  if (inherits(fit, "glm")) {
    stop(
      "apa_anova() makes tables of F tests for linear models. ",
      "For generalized linear models, use apa_parameters() or apa_performance_comparison()."
    )
  }
  if (inherits(fit, "mlm")) {
    stop("apa_anova() does not support multivariate linear models.")
  }

  type <- anova_ss_type(type)
  if (type == 1) {
    tab <- if (inherits(fit, "aov")) fit else stats::anova(fit)
  } else {
    if (!requireNamespace("car", quietly = TRUE)) {
      stop("Type 2 and 3 sums of squares require the car package. Install it with install.packages(\"car\").") # nocov
    }
    tab <- car::Anova(fit, type = type)
  }

  apa_anova_table(
    tab,
    anova_parameters = anova_parameters,
    effect_size = effect_size,
    partial = partial,
    generalized = generalized,
    ci = ci,
    residual_label = residual_label,
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    note = list(ss_type = type),
    ...
  )
}

#' @export
#' @rdname apa_anova
apa_anova.aovlist <- function(
  fit,
  anova_parameters = c(
    "Parameter",
    "Sum_Squares",
    "df",
    "Mean_Square",
    "F",
    "p",
    "Effect_Size"
  ),
  effect_size = c("eta", "omega", "epsilon"),
  partial = TRUE,
  generalized = FALSE,
  type = NULL,
  ci = 0.95,
  residual_label = "Error",
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  ...
) {
  if (anova_ss_type(type) != 1) {
    stop(
      "Repeated-measures aov models use sequential (Type 1) sums of squares. ",
      "For Type 2 or 3 tests, fit the model with afex::aov_car() or car::Anova()."
    )
  }
  apa_anova_table(
    fit,
    anova_parameters = anova_parameters,
    effect_size = effect_size,
    partial = partial,
    generalized = generalized,
    ci = ci,
    residual_label = residual_label,
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    note = list(ss_type = 1L),
    ...
  )
}

#' @export
#' @rdname apa_anova
apa_anova.anova <- function(
  fit,
  anova_parameters = c(
    "Parameter",
    "Sum_Squares",
    "df",
    "Mean_Square",
    "F",
    "p",
    "Effect_Size"
  ),
  effect_size = c("eta", "omega", "epsilon"),
  partial = TRUE,
  generalized = FALSE,
  type = NULL,
  ci = 0.95,
  residual_label = "Error",
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  ...
) {
  if ("RSS" %in% colnames(fit)) {
    stop(
      "This looks like a model comparison (e.g., anova(m1, m2)). ",
      "Use apa_performance_comparison() for model comparisons."
    )
  }
  if (!any(grepl("^F", colnames(fit)))) {
    stop(
      "apa_anova() makes tables of F tests, but this ANOVA table has none ",
      "(e.g., a deviance table from a generalized linear model)."
    )
  }
  if (!is.null(type)) {
    warning("`type` is ignored for ANOVA tables, which already have a type of sums of squares.")
  }
  apa_anova_table(
    fit,
    anova_parameters = anova_parameters,
    effect_size = effect_size,
    partial = partial,
    generalized = generalized,
    ci = ci,
    residual_label = residual_label,
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    note = list(ss_type = anova_heading_type(fit)),
    ...
  )
}

#' @details
#' For `afex` models (`afex::aov_car()`, `afex::aov_ez()`, `afex::aov_4()`), each effect has its own error term, so the table shows each effect's degrees of freedom (*df*~effect~, *df*~error~), mean squared error (*MSE*), *F*, *p*, and effect size, as in `afex`'s own `nice()` tables. Degrees of freedom and *p*-values reflect the sphericity correction set in `afex` (Greenhouse-Geisser by default), and generalized *&eta;*^2^ is the default effect size, as in `afex`.
#' @export
#' @rdname apa_anova
#' @examples
#' if (requireNamespace("afex", quietly = TRUE)) {
#'   data(obk.long, package = "afex")
#'   fit_afex <- afex::aov_ez(
#'     "id", "value", obk.long,
#'     between = "treatment", within = "phase", fun_aggregate = mean
#'   )
#'   apa_anova(fit_afex)
#' }
apa_anova.afex_aov <- function(
  fit,
  anova_parameters = c(
    "Parameter",
    "df",
    "df_error",
    "Mean_Square",
    "F",
    "p",
    "Effect_Size"
  ),
  effect_size = c("eta", "omega", "epsilon"),
  partial = TRUE,
  generalized = TRUE,
  type = NULL,
  ci = 0.95,
  residual_label = "Error",
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  ...
) {
  # Prevent warnings
  Parameter <- NULL

  if (!is.null(type)) {
    warning("`type` is ignored for afex models. Set it when fitting the model (e.g., afex::aov_car(type = 2)).")
  }
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }

  if (!is.null(effect_size)) {
    effect_size <- match.arg(effect_size)
  }
  if (!identical(effect_size, "eta")) {
    generalized <- FALSE
  }
  anova_parameters <- drop_generalized_ci(anova_parameters, generalized)
  column_formats <- es_ci_format(column_formats)
  show_ci <- "CI" %in% anova_parameters && !is.null(effect_size)

  d <- parameters::model_parameters(fit, ...) |>
    lapply(identity) |>
    tibble::as_tibble() # lapply() drops parameters attributes

  # model_parameters() fixes one-sided intervals for afex models, so effect
  # sizes come from effectsize directly
  if (!is.null(effect_size)) {
    es_fun <- switch(
      effect_size,
      eta = effectsize::eta_squared,
      omega = effectsize::omega_squared,
      epsilon = effectsize::epsilon_squared
    )
    es_args <- list(
      fit,
      partial = partial,
      ci = if (show_ci) ci else NULL,
      alternative = "two.sided",
      verbose = FALSE
    )
    if (isTRUE(generalized)) {
      es_args$generalized <- TRUE
    }
    d_es <- do.call(es_fun, es_args) |>
      tibble::as_tibble()
    es_col <- intersect(colnames(d_es), anova_es_columns)[1]
    keep <- intersect(c("Parameter", es_col, "CI_low", "CI_high"), colnames(d_es))
    d_es <- d_es[keep]
    if (show_ci && all(c("CI_low", "CI_high") %in% colnames(d_es))) {
      d_es$CI <- ci
    }
    d <- dplyr::left_join(d, d_es, by = dplyr::join_by(Parameter))
  }

  es_col <- intersect(colnames(d), anova_es_columns)[1]
  if (is.na(es_col)) {
    anova_parameters <- setdiff(anova_parameters, c("Effect_Size", "CI"))
  } else {
    anova_parameters[anova_parameters == "Effect_Size"] <- es_col
    if (!show_ci || !("CI_low" %in% colnames(d))) {
      anova_parameters <- setdiff(anova_parameters, "CI")
    }
  }

  d$Parameter <- gsub(":", " \u00D7 ", d$Parameter)

  # Each effect has its own error term
  if (column_formats$Parameter@header == "Variable") {
    column_formats$Parameter@header <- "Effect"
    column_formats$Parameter@latex <- "Effect"
  }
  if (!is.null(column_formats$df)) {
    column_formats$df@header <- "*df*~effect~"
    column_formats$df@latex <- "$df_{\\text{effect}}$"
  }
  if (!is.null(column_formats$df_error)) {
    column_formats$df_error@header <- "*df*~error~"
    column_formats$df_error@latex <- "$df_{\\text{error}}$"
  }
  if (!is.null(column_formats$Mean_Square)) {
    column_formats$Mean_Square@header <- "*MSE*"
    column_formats$Mean_Square@latex <- "$MSE$"
    column_formats$Mean_Square@abbreviation <- "mean square error"
  }

  d_new <- format_apa_parameters(
    d,
    predictor_parameters = anova_parameters,
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    t_with_df = FALSE
  )

  attr(d_new, "apa_note") <- anova_note_record(
    ss_type = afex_ss_type(fit),
    abbreviations = attr(d_new, "apa_note", exact = TRUE)$abbreviations,
    starred = anova_marked(starred, anova_parameters),
    bolded = anova_marked(bolded, anova_parameters),
    correction = attr(fit$anova_table, "correction"),
    corrected_factors = afex_corrected_factors(
      fit,
      column_formats$Parameter@formatter
    )
  )

  as_apa7_tbl(d_new)
}

#' Type of sums of squares of an afex model
#' @keywords internal
#' @noRd
afex_ss_type <- function(fit) {
  type <- attr(fit, "type")
  if (is.null(type)) {
    return(NA_integer_) # nocov
  }
  anova_ss_type(type)
}

#' Within-subjects factors whose effects get a sphericity correction (those
#' with more than two levels), formatted like the table's effect names
#' @keywords internal
#' @noRd
afex_corrected_factors <- function(fit, formatter) {
  within <- attr(fit, "within")
  if (length(within) == 0) {
    return(character(0))
  }
  factors <- names(within)[lengths(within) > 2]
  if (length(factors) == 0) {
    return(character(0))
  }
  formatter(factors)
}

#' @export
#' @rdname apa_anova
apa_anova.default <- function(fit, ...) {
  stop(
    "apa_anova() does not support objects of class ",
    paste0("\"", class(fit), "\"", collapse = ", "),
    ". Supported objects are aov and lm models, repeated-measures aov models, ",
    "afex models, and ANOVA tables from stats::anova() or car::Anova()."
  )
}

#' Type of sums of squares as 1, 2, or 3
#' @keywords internal
#' @noRd
anova_ss_type <- function(type) {
  if (is.null(type)) {
    return(1)
  }
  if (is.character(type)) {
    type <- match(toupper(type), c("I", "II", "III", "1", "2", "3"))
    type <- ((type - 1) %% 3) + 1
  }
  if (length(type) != 1 || is.na(type) || !(type %in% 1:3)) {
    stop("`type` must be 1, 2, or 3.")
  }
  as.integer(type)
}

#' Effect size column names from effectsize
#' @keywords internal
#' @noRd
anova_es_columns <- c(
  "Eta2",
  "Eta2_partial",
  "Eta2_generalized",
  "Omega2",
  "Omega2_partial",
  "Epsilon2",
  "Epsilon2_partial"
)

#' Labels for the error strata of repeated-measures models
#'
#' Strata such as "id" and "id:phase" become "Between subjects" and "Within
#' subjects". With more than one within-subjects stratum, the stratum's effect
#' is named (e.g., "Within subjects: Phase").
#' @keywords internal
#' @noRd
anova_stratum_labels <- function(strata, formatter) {
  within <- strata == "Within" | grepl(":", strata)
  labels <- ifelse(within, "Within subjects", "Between subjects")
  within_strata <- unique(strata[within])
  if (length(within_strata) > 1) {
    effect <- sub("^[^:]*:", "", strata[within])
    labels[within] <- paste0(
      "Within subjects: ",
      formatter(gsub(":", " \u00D7 ", effect))
    )
  }
  labels
}

#' Shared by `apa_anova` methods
#' @keywords internal
#' @noRd
apa_anova_table <- function(
  tab,
  anova_parameters,
  effect_size,
  partial,
  generalized,
  ci,
  residual_label,
  starred,
  bolded,
  column_formats,
  note = list(),
  ...
) {
  # Prevent warnings
  Parameter <- Group <- NULL

  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }
  names(anova_parameters) <- NULL

  # Effect size options
  if (!is.null(effect_size)) {
    effect_size <- match.arg(effect_size, c("eta", "omega", "epsilon"))
  }
  if (isTRUE(generalized) && !identical(effect_size, "eta")) {
    stop("Generalized effect sizes are available only for `effect_size = \"eta\"`.")
  }
  anova_parameters <- drop_generalized_ci(anova_parameters, generalized)
  column_formats <- es_ci_format(column_formats)
  show_ci <- "CI" %in% anova_parameters && !is.null(effect_size)

  args <- list(tab, ...)
  if (!is.null(effect_size)) {
    args$es_type <- effect_size
    args$partial <- partial
    args$ci <- if (show_ci) ci else NULL
    if (show_ci) {
      args$alternative <- "two.sided"
    }
    if (isTRUE(generalized)) {
      args$generalized <- TRUE
    }
  }

  d <- do.call(parameters::model_parameters, args) |>
    lapply(identity) |>
    tibble::as_tibble() # lapply() drops parameters attributes

  # Only F-test source tables are supported
  if ("RSS" %in% colnames(d)) {
    stop(
      "This looks like a model comparison (e.g., anova(m1, m2)). ",
      "Use apa_performance_comparison() for model comparisons."
    )
  }
  if (!("F" %in% colnames(d))) {
    stop(
      "apa_anova() makes tables of F tests, but this ANOVA table has none ",
      "(e.g., a deviance table from a generalized linear model)."
    )
  }

  # Error strata of repeated-measures models stay together, error term last
  has_strata <- "Group" %in% colnames(d)
  if (has_strata) {
    d <- d |>
      dplyr::arrange(
        match(Group, unique(Group)),
        Parameter == "Residuals"
      )
  }

  # Effect size and its confidence interval
  es_col <- intersect(colnames(d), anova_es_columns)[1]
  if (is.na(es_col)) {
    anova_parameters <- setdiff(anova_parameters, c("Effect_Size", "CI"))
  } else {
    anova_parameters[anova_parameters == "Effect_Size"] <- es_col
    ci_low <- grep("_CI_low$", colnames(d), value = TRUE)
    ci_high <- grep("_CI_high$", colnames(d), value = TRUE)
    if (show_ci && length(ci_low) == 1 && length(ci_high) == 1) {
      d$CI_low <- d[[ci_low]]
      d$CI_high <- d[[ci_high]]
      d$CI <- ci
    } else {
      anova_parameters <- setdiff(anova_parameters, "CI")
    }
  }

  # Source labels
  d$Parameter <- ifelse(
    d$Parameter == "Residuals",
    residual_label,
    gsub(":", " \u00D7 ", d$Parameter)
  )
  if (column_formats$Parameter@header == "Variable") {
    column_formats$Parameter@header <- "Source"
    column_formats$Parameter@latex <- "Source"
  }

  strata <- if (has_strata) d$Group else NULL

  d_new <- format_apa_parameters(
    d,
    predictor_parameters = anova_parameters,
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    t_with_df = FALSE
  )
  # Stratum rows are added with bind_rows(), which drops attributes
  abbreviations <- attr(d_new, "apa_note", exact = TRUE)$abbreviations

  if (has_strata) {
    d_new <- add_stratum_rows(
      d_new,
      anova_stratum_labels(strata, column_formats$Parameter@formatter)
    )
  }

  attr(d_new, "apa_note") <- anova_note_record(
    ss_type = if (is.null(note$ss_type)) NA else note$ss_type,
    abbreviations = abbreviations,
    starred = anova_marked(starred, anova_parameters),
    bolded = anova_marked(bolded, anova_parameters)
  )

  as_apa7_tbl(d_new)
}

#' Insert a heading row before each error stratum and indent its rows
#' @keywords internal
#' @noRd
add_stratum_rows <- function(d, labels) {
  source_col <- colnames(d)[1]
  d[[source_col]] <- paste0("\u2003", d[[source_col]])
  pieces <- split(seq_len(nrow(d)), factor(labels, levels = unique(labels)))
  rows <- purrr::imap(pieces, \(i, label) {
    heading <- d[1, ]
    heading[1, ] <- NA
    heading[[source_col]] <- label
    dplyr::bind_rows(heading, d[i, ])
  })
  out <- dplyr::bind_rows(rows)
  out[is.na(out)] <- ""
  out
}

#' @details
#' For `aov` models (including repeated-measures models with an `Error()` term) and `afex` models, `apa_parameters()` returns the ANOVA table from [apa_anova()]. `predictor_parameters` and `t_with_df` are ignored, and other arguments in `...` (e.g., `effect_size`, `type`) are passed to [apa_anova()]. To display regression coefficients instead, refit the model with [stats::lm()].
#' @export
#' @rdname apa_parameters
apa_parameters.aov <- function(
  fit,
  predictor_parameters = NULL,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  ...
) {
  apa_anova(
    fit,
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    ...
  )
}

#' @export
#' @rdname apa_parameters
apa_parameters.aovlist <- apa_parameters.aov

#' @export
#' @rdname apa_parameters
apa_parameters.afex_aov <- apa_parameters.aov

#' Format effect size intervals like effect sizes: no leading zero, and
#' decimals kept even when a bound is exactly 0 (e.g., \[.00, .52\])
#' @keywords internal
#' @noRd
es_ci_format <- function(column_formats) {
  if (!is.null(column_formats$CI)) {
    column_formats$CI@formatter <- \(l, u, accuracy = the$accuracy, ...) {
      ci_md(
        align_chr(l, accuracy = accuracy, trim_leading_zeros = TRUE, format_integers = TRUE),
        align_chr(u, accuracy = accuracy, trim_leading_zeros = TRUE, format_integers = TRUE),
        missing = is.na(l) | is.na(u)
      )
    }
  }
  column_formats
}

#' Generalized eta squared intervals from effectsize can exclude the point
#' estimate, so they are not shown
#' @keywords internal
#' @noRd
drop_generalized_ci <- function(anova_parameters, generalized) {
  if (isTRUE(generalized) && "CI" %in% anova_parameters) {
    warning(
      "Confidence intervals are not shown for generalized eta squared, ",
      "because those computed by effectsize can exclude the point estimate. ",
      "Use `generalized = FALSE` for intervals of partial eta squared."
    )
    anova_parameters <- setdiff(anova_parameters, "CI")
  }
  anova_parameters
}
