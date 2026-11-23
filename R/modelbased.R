#' Estimated marginal means, contrasts, and slopes tables
#'
#' Makes APA-style tables of estimated marginal means, contrasts between them
#' (e.g., pairwise comparisons after an ANOVA), and simple slopes (e.g., to
#' probe a moderation effect), computed with the
#' [modelbased](https://easystats.github.io/modelbased/) package.
#'
#' `x` is either a model or the output of [modelbased::estimate_means()],
#' [modelbased::estimate_contrasts()], or [modelbased::estimate_slopes()].
#' With a model, the arguments in `...` are passed to the modelbased function
#' (e.g., `by`, `contrast`, `trend`, and `p_adjust`), so
#' `apa_contrasts(fit, contrast = "group", p_adjust = "holm")` is the same as
#' `apa_contrasts(modelbased::estimate_contrasts(fit, contrast = "group", p_adjust = "holm"))`.
#'
#' Rows are labeled by the levels of the last focal variable (in
#' `apa_means()`), by the levels compared (in `apa_contrasts()`, e.g.,
#' "6 \eqn{-}{-} 4"), or by the levels of the moderator (in `apa_slopes()`).
#' Any other variables in `by` get heading rows, with the rows below them
#' indented. Numeric levels (e.g., values of a continuous moderator) are
#' rounded to 2 decimals.
#'
#' Values set with a modelbased shortcut are named, with the value in
#' parentheses: `"x=[sd]"` (or `"x=[meansd]"`) gives "\eqn{-}{-}1*SD*",
#' "*M*", and "+1*SD*"; `"x=[mad]"` gives "\eqn{-}{-}1*MAD*", "*Mdn*", and
#' "+1*MAD*"; `"x=[fivenum]"` gives "Min", "*Q*~1~", "*Mdn*", "*Q*~3~",
#' and "Max"; `"x=[quartiles]"` gives "*Q*~1~", "*Mdn*", and "*Q*~3~";
#' and `"x=[minmax]"` gives "Min" and "Max". Contrasts between such values
#' (e.g., `contrast = "x=[sd]"`) are labeled by name (e.g.,
#' "+1*SD* \eqn{-}{-} *M*"), and the note gives the values. With a model, a
#' numeric variable named in `by` without values (e.g., `by = "x"`) is set to
#' its mean and 1 *SD* above and below it, as with `by = "x=[sd]"` (rather
#' than the modelbased default of 10 values, which suits plots better than
#' tables).
#'
#' [apa_note()] says what the values are, how the other predictors were held
#' constant, on which scale the estimates are (for models with a link
#' function, such as logistic regression), which level was subtracted from
#' which, and how *p*-values were adjusted for multiple comparisons.
#'
#' Bayesian models are not yet supported.
#'
#' @param x a model, or output of [modelbased::estimate_means()] (for
#'   `apa_means()`), [modelbased::estimate_contrasts()] (for
#'   `apa_contrasts()`), or [modelbased::estimate_slopes()] (for
#'   `apa_slopes()`)
#' @param ... arguments passed to the modelbased function when `x` is a model
#' @param starred columns to which significance stars are added (e.g., `"p"`
#'   or `"Estimate"`)
#' @param bolded columns that are bolded when *p* \eqn{\le}{<=} .05
#' @param column_formats [column_formats()] object. If `NULL` (default), the
#'   defaults are used. The estimate column is named `"Estimate"`.
#' @param format_variable_names if `TRUE` (default), variable names are
#'   formatted (e.g., `"Sepal.Length"` becomes "Sepal Length"). Factor levels
#'   are shown as they are.
#'
#' @returns `apa7_tbl` (tibble)
#' @export
#' @examples
#' if (requireNamespace("modelbased", quietly = TRUE) &&
#'     requireNamespace("marginaleffects", quietly = TRUE)) {
#'   fit <- lm(Sepal.Width ~ Species * Sepal.Length, data = iris)
#'
#'   # Means of each species, with sepal length held at its mean
#'   apa_means(fit, by = "Species")
#'
#'   # Pairwise comparisons, with Holm-adjusted p-values
#'   apa_contrasts(fit, contrast = "Species", p_adjust = "holm")
#'
#'   # Simple slopes of sepal length in each species
#'   apa_slopes(fit, trend = "Sepal.Length", by = "Species")
#'
#'   # The output of modelbased works, too
#'   modelbased::estimate_contrasts(fit, contrast = "Species") |>
#'     apa_contrasts() |>
#'     apa_note()
#' }
apa_means <- function(
  x,
  ...,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  format_variable_names = TRUE
) {
  x <- modelbased_call(x, "estimate_means", ...)
  modelbased_table(
    x,
    kind = "means",
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    format_variable_names = format_variable_names
  )
}

#' @rdname apa_means
#' @export
apa_contrasts <- function(
  x,
  ...,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  format_variable_names = TRUE
) {
  x <- modelbased_call(x, "estimate_contrasts", ...)
  modelbased_table(
    x,
    kind = "contrasts",
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    format_variable_names = format_variable_names
  )
}

#' @rdname apa_means
#' @export
apa_slopes <- function(
  x,
  ...,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  format_variable_names = TRUE
) {
  x <- modelbased_call(x, "estimate_slopes", ...)
  modelbased_table(
    x,
    kind = "slopes",
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    format_variable_names = format_variable_names
  )
}

#' modelbased output, computed from a model if needed, with numeric
#' variables named in `by` without values set to their mean and 1 SD above
#' and below it
#' @keywords internal
#' @noRd
modelbased_call <- function(x, fun, ...) {
  if (inherits(x, "data.frame")) {
    return(modelbased_estimates(x, fun, ...))
  }
  args <- list(...)
  if (!is.null(args$by)) {
    args$by <- modelbased_numeric_by(x, args$by)
  }
  do.call(modelbased_estimates, c(list(x, fun), args))
}

#' modelbased output, computed from a model if needed
#' @param fun name of the modelbased function (also its output class)
#' @keywords internal
#' @noRd
modelbased_estimates <- function(x, fun, ...) {
  if (inherits(x, fun)) {
    if (...length() > 0) {
      cli::cli_warn(
        "Arguments in {.arg ...} are ignored because {.arg x} is already output of {.fn modelbased::{fun}}."
      )
    }
    return(x)
  }
  functions <- c(
    estimate_means = "apa_means",
    estimate_contrasts = "apa_contrasts",
    estimate_slopes = "apa_slopes"
  )
  other <- intersect(names(functions), class(x))
  if (length(other) > 0) {
    cli::cli_abort(
      "{.arg x} is output of {.fn modelbased::{other[1]}}. Use {.fn {functions[[other[1]]]}} instead."
    )
  }
  if (is.data.frame(x)) {
    cli::cli_abort(
      "{.arg x} must be a model or output of {.fn modelbased::{fun}}, not a plain data frame."
    )
  }
  rlang::check_installed(
    c("modelbased", "marginaleffects"),
    reason = "to estimate marginal means, contrasts, and slopes."
  )
  f <- getExportedValue("modelbased", fun)
  f(x, ...)
}

#' Set numeric variables named in by without values (e.g., "x") to their
#' mean and 1 SD above and below it ("x=[sd]")
#' @keywords internal
#' @noRd
modelbased_numeric_by <- function(model, by) {
  data <- tryCatch(
    insight::get_data(model, verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(data) || !is.character(by)) {
    return(by)
  }
  continuous <- vapply(
    by,
    \(v) {
      v %in% colnames(data) &&
        is.numeric(data[[v]]) &&
        length(unique(stats::na.omit(data[[v]]))) > 2
    },
    logical(1)
  )
  by[continuous] <- paste0(by[continuous], "=[sd]")
  by
}

#' Labels of moderator values set with a modelbased shortcut
#'
#' For example, `"x=[sd]"` gives "-1*SD* (12.23)", "*M* (14.23)", and
#' "+1*SD* (16.23)", and `"x=[fivenum]"` gives the minimum, quartiles,
#' median, and maximum.
#' @param v values of the variable
#' @param var name of the variable
#' @param by the `by` attribute of the modelbased output
#' @returns labels, or `NULL` if no shortcut was used
#' @keywords internal
#' @noRd
modelbased_shortcut_labels <- function(v, var, by) {
  if (!is.numeric(v)) {
    return(NULL)
  }
  shortcut <- modelbased_shortcut(var, by)
  names <- modelbased_shortcut_names(shortcut)
  values <- sort(unique(v))
  if (is.null(names) || length(values) != length(names)) {
    return(NULL)
  }
  labels <- paste0(names[match(v, values)], " (", modelbased_values(v), ")")
  attr(labels, "shortcut") <- shortcut
  labels
}

#' Shortcut (e.g., "sd") used for a variable in by or contrast specifications
#' (e.g., "x=[sd]"), or NULL
#' @keywords internal
#' @noRd
modelbased_shortcut <- function(var, specs) {
  spec <- specs[startsWith(specs, paste0(var, "=["))]
  if (length(spec) != 1) {
    return(NULL)
  }
  sub("^.*=\\[(.*)\\]$", "\\1", spec)
}

#' Names of the values a shortcut gives, from lowest to highest
#' @keywords internal
#' @noRd
modelbased_shortcut_names <- function(shortcut) {
  if (is.null(shortcut)) {
    return(NULL)
  }
  minus <- "\u2212"
  switch(
    shortcut,
    sd = ,
    meansd = c(paste0(minus, "1*SD*"), "*M*", "+1*SD*"),
    mad = c(paste0(minus, "1*MAD*"), "*Mdn*", "+1*MAD*"),
    fivenum = c("Min", "*Q*~1~", "*Mdn*", "*Q*~3~", "Max"),
    quartiles = c("*Q*~1~", "*Mdn*", "*Q*~3~"),
    minmax = c("Min", "Max"),
    NULL
  )
}

#' Definitions of the symbols in shortcut labels
#' @keywords internal
#' @noRd
modelbased_shortcut_abbreviations <- function(shortcuts) {
  defs <- character(0)
  if ("mad" %in% shortcuts) {
    defs <- c(defs, "*MAD*" = "median absolute deviation")
  }
  if ("quartiles" %in% shortcuts) {
    defs <- c(defs, "*Q*~1~" = "first quartile", "*Q*~3~" = "third quartile")
  } else if ("fivenum" %in% shortcuts) {
    # fivenum() gives Tukey's hinges, which can differ slightly from quartiles
    defs <- c(
      defs,
      "*Q*~1~" = "first quartile (lower hinge)",
      "*Q*~3~" = "third quartile (upper hinge)"
    )
  }
  defs
}

#' Format numeric levels (e.g., moderator values) for row labels
#' @keywords internal
#' @noRd
modelbased_values <- function(v) {
  if (is.numeric(v)) {
    whole <- all(abs(v - round(v)) < 1e-8, na.rm = TRUE)
    return(signs::signs(v, accuracy = if (whole) 1 else 0.01))
  }
  as.character(v)
}

#' Header, LaTeX header, and definition of the estimate column
#' @keywords internal
#' @noRd
modelbased_estimate_header <- function(coef_name, kind) {
  switch(
    coef_name,
    Mean = if (kind == "means") {
      c(header = "*M*", latex = "$M$", abbreviation = "estimated marginal mean")
    } else {
      c(header = "*M*", latex = "$M$", abbreviation = NA)
    },
    Median = c(header = "*Mdn*", latex = "$Mdn$", abbreviation = "median"),
    c(header = coef_name, latex = coef_name, abbreviation = NA)
  )
}

#' Table of modelbased output
#' @param kind `"means"`, `"contrasts"`, or `"slopes"`
#' @keywords internal
#' @noRd
modelbased_table <- function(
  x,
  kind,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  format_variable_names = TRUE
) {
  d <- as.data.frame(x)
  a <- attributes(x)
  info <- a$model_info
  if (
    isTRUE(info$is_bayesian) ||
      any(c("pd", "ROPE_Percentage", "BF") %in% colnames(d))
  ) {
    cli::cli_abort("Estimates from Bayesian models are not yet supported.")
  }

  label <- function(v) {
    if (format_variable_names) variable_name_formatter(v) else v
  }

  coef_col <- intersect(
    c(a$coef_name, "Mean", "Probability", "Difference", "Slope", "Median", "Coefficient", "Ratio"),
    colnames(d)
  )[1]
  if (is.na(coef_col)) {
    cli::cli_abort("Cannot find the estimates in {.arg x}.") # nocov
  }
  focal <- intersect(a$focal_terms, colnames(d))
  # Labels of a variable's levels, naming shortcut values (e.g., "+1*SD*")
  shortcuts <- character(0)
  contrast_values <- NULL
  level_labels <- function(var) {
    labels <- modelbased_shortcut_labels(d[[var]], var, a$by)
    if (is.null(labels)) {
      return(modelbased_values(d[[var]]))
    }
    shortcuts <<- c(shortcuts, attr(labels, "shortcut"))
    as.character(labels)
  }
  trend <- a$trend
  contrast_vars <- a$contrast

  if (kind == "means") {
    if (length(focal) == 0) {
      cli::cli_abort("Cannot find the focal variables of the means in {.arg x}.") # nocov
    }
    row_var <- focal[length(focal)]
    heading_vars <- focal[-length(focal)]
    row_label <- level_labels(row_var)
    first_header <- label(row_var)
  } else if (kind == "contrasts") {
    contrast_names <- sub("=.*$", "", contrast_vars)
    heading_vars <- setdiff(focal, contrast_names)
    row_label <- paste0(d$Level1, " \u2212 ", d$Level2)
    first_header <- if (length(contrast_vars) == 1) label(contrast_names) else "Contrast"
    # Values set with a shortcut (e.g., "x=[sd]") are named (e.g., "*M* \u2212 \u22121*SD*")
    if (length(contrast_vars) == 1) {
      shortcut <- modelbased_shortcut(contrast_names, contrast_vars)
      names <- modelbased_shortcut_names(shortcut)
      level1 <- suppressWarnings(as.numeric(as.character(d$Level1)))
      level2 <- suppressWarnings(as.numeric(as.character(d$Level2)))
      values <- sort(unique(c(level1, level2)))
      if (!is.null(names) && !anyNA(values) && length(values) == length(names)) {
        row_label <- paste0(names[match(level1, values)], " \u2212 ", names[match(level2, values)])
        shortcuts <- c(shortcuts, shortcut)
        contrast_values <- paste0(names, " = ", modelbased_values(values))
      }
    }
  } else {
    moderators <- setdiff(focal, trend)
    if (length(moderators) == 0) {
      heading_vars <- character(0)
      row_label <- rep(label(trend), nrow(d))
      first_header <- "Predictor"
    } else {
      row_var <- moderators[length(moderators)]
      heading_vars <- moderators[-length(moderators)]
      row_label <- level_labels(row_var)
      first_header <- label(row_var)
    }
  }

  # Unformatted columns, named as in apa_parameters()
  d_b <- tibble::tibble(Parameter = row_label, Estimate = d[[coef_col]])
  if ("SE" %in% colnames(d) && !all(is.na(d$SE))) {
    d_b$SE <- d$SE
  }
  ci <- if (is.null(a$ci)) 0.95 else a$ci
  if (all(c("CI_low", "CI_high") %in% colnames(d))) {
    d_b$CI <- ci
    d_b$CI_low <- d$CI_low
    d_b$CI_high <- d$CI_high
  }
  stat <- character(0)
  if (kind != "means") {
    if ("t" %in% colnames(d)) {
      if ("df" %in% colnames(d) && all(is.infinite(d$df))) {
        # Asymptotic tests
        d_b$z <- d$t
        stat <- "z"
      } else {
        d_b$t <- d$t
        stat <- "t"
        if ("df" %in% colnames(d)) {
          d_b$df_error <- d$df
          stat <- c("t", "df_error")
        }
      }
    } else if ("z" %in% colnames(d)) {
      d_b$z <- d$z
      stat <- "z"
    }
    if ("p" %in% colnames(d)) {
      d_b$p <- d$p
    }
  }
  predictor_parameters <- c(
    "Parameter",
    "Estimate",
    intersect("SE", colnames(d_b)),
    if ("CI_low" %in% colnames(d_b)) "CI",
    stat,
    intersect("p", colnames(d_b))
  )

  # Column formats
  cf <- if (is.null(column_formats)) the$column_formats else column_formats
  estimate_header <- modelbased_estimate_header(coef_col, kind)
  if (is.null(cf$Estimate)) {
    est <- cf$M
    est@name <- "Estimate"
    est@header <- estimate_header[["header"]]
    est@latex <- estimate_header[["latex"]]
    abbreviation <- estimate_header[["abbreviation"]]
    est@abbreviation <- if (is.na(abbreviation)) character(0) else abbreviation
    cf$Estimate <- est
  }
  cf$Parameter@formatter <- \(x, ...) x
  # Probabilities and differences between them have no leading zero
  probability <- (isTRUE(info$is_binomial) || isTRUE(info$is_bernoulli)) &&
    !identical(a$predict, "link") &&
    kind != "slopes"
  if (probability) {
    bounded <- \(x, accuracy = the$accuracy, ...) {
      align_chr(x, accuracy = accuracy, trim_leading_zeros = TRUE, format_integers = TRUE)
    }
    cf$Estimate@formatter <- bounded
    if (!is.null(cf$SE)) {
      se <- cf$SE
      se@formatter <- bounded
      cf$SE <- se
    }
    cf <- es_ci_format(cf)
  }

  tbl <- format_apa_parameters(
    d_b,
    predictor_parameters = predictor_parameters,
    starred = starred,
    bolded = bolded,
    column_formats = cf,
    t_with_df = TRUE
  )
  record <- attr(tbl, "apa_note", exact = TRUE)

  # Heading rows for the other variables in by
  if (length(heading_vars) > 0) {
    keys <- purrr::map(heading_vars, \(v) {
      shortcut <- modelbased_shortcut_labels(d[[v]], v, a$by)
      if (!is.null(shortcut)) {
        shortcuts <<- c(shortcuts, attr(shortcut, "shortcut"))
        return(paste0(label(v), ": ", as.character(shortcut)))
      }
      sep <- if (is.numeric(d[[v]])) " = " else ": "
      paste0(label(v), sep, modelbased_values(d[[v]]))
    })
    key <- do.call(paste, c(keys, sep = ", "))
    ord <- order(match(key, unique(key)))
    tbl <- tbl[ord, , drop = FALSE]
    key <- key[ord]
    tbl <- dplyr::mutate(tbl, dplyr::across(dplyr::everything(), as.character))
    pieces <- purrr::map(unique(key), \(k) {
      block <- tbl[key == k, , drop = FALSE]
      block[[1]] <- paste0("\u2003", block[[1]])
      heading <- tbl[0, , drop = FALSE]
      heading[1, 1] <- k
      dplyr::bind_rows(heading, block)
    })
    tbl <- dplyr::bind_rows(pieces)
    tbl <- dplyr::mutate(
      tbl,
      dplyr::across(dplyr::everything(), \(v) ifelse(is.na(v), "", v))
    )
  }
  colnames(tbl)[1] <- first_header

  # Predictors held constant
  model <- a$model
  adjusted <- setdiff(stats::na.omit(a$adjusted_for), c(trend, focal))
  if (isTRUE(info$is_mixed) && !is.null(model)) {
    random <- tryCatch(
      insight::find_random(model, flatten = TRUE),
      error = function(e) NULL
    )
    adjusted <- setdiff(adjusted, random)
  }
  grid <- a$datagrid
  held <- character(0)
  held_values <- character(0)
  averaged <- character(0)
  for (v in adjusted) {
    values <- if (!is.null(grid) && v %in% colnames(grid)) unique(grid[[v]]) else NULL
    if (is.numeric(values) && length(values) == 1) {
      held <- c(held, label(v))
      held_values <- c(held_values, align_chr(values, accuracy = the$accuracy))
    } else {
      averaged <- c(averaged, label(v))
    }
  }

  record$table <- "modelbased"
  record$abbreviations <- combine_abbreviations(
    record$abbreviations,
    modelbased_shortcut_abbreviations(shortcuts)
  )
  record$kind <- kind
  record$response <- if (is.null(a$response)) NULL else label(a$response)
  record$coef_name <- coef_col
  record$estimate <- a$estimate
  record$held <- held
  record$held_values <- held_values
  record$probability <- probability
  record$averaged <- averaged
  record$link <- if (is.null(info$link_function)) "identity" else info$link_function
  record$predict <- a$predict
  record$p_adjust <- if ("p" %in% colnames(d)) a$p_adjust else NULL
  record$trend <- if (is.null(trend)) NULL else label(trend)
  record$moderators <- if (kind == "slopes") label(setdiff(focal, trend)) else NULL
  record$contrast_vars <- if (kind == "contrasts") label(contrast_names) else NULL
  record$contrast_values <- contrast_values

  attr(tbl, "apa_note") <- record
  attr(tbl, "no_format_columns") <- first_header
  as_apa7_tbl(labels_formatted(tbl))
}

#' Names of p-value adjustment methods
#' @keywords internal
#' @noRd
p_adjust_names <- c(
  holm = "Holm's method",
  bonferroni = "the Bonferroni correction",
  tukey = "Tukey's method",
  hochberg = "Hochberg's method",
  hommel = "Hommel's method",
  BH = "the Benjamini\u2013Hochberg false discovery rate correction",
  fdr = "the Benjamini\u2013Hochberg false discovery rate correction",
  BY = "the Benjamini\u2013Yekutieli correction",
  sidak = "the \u0160id\u00e1k correction",
  scheffe = "Scheff\u00e9's method",
  mvt = "the multivariate *t* method",
  esarey = "Esarey and Sumner's method"
)

#' @keywords internal
#' @noRd
modelbased_note_sentences <- function(record) {
  sentences <- character(0)
  of <- if (is.null(record$response)) "" else paste0(" of ", record$response)
  probability <- isTRUE(record$probability)
  estimates <- if (probability) "estimated probabilities" else "estimated marginal means"

  # What the values are
  sentences <- c(
    sentences,
    switch(
      record$kind,
      means = paste0("Values are ", estimates, of, "."),
      contrasts = paste0(
        "Values are differences between ", estimates, of,
        " (the first level minus the second)."
      ),
      slopes = paste0(
        if (length(record$moderators) > 0) "Values are simple slopes of " else "The estimate is the average slope of ",
        record$trend,
        " (the expected change", sub("^ of", " in", of), " for a one-unit increase in ", record$trend, ")",
        if (length(record$moderators) > 0) paste0(" at each value of ", and_list(record$moderators)),
        "."
      )
    )
  )

  # Values of a contrasted variable set with a shortcut
  if (length(record$contrast_values) > 0) {
    sentences <- c(
      sentences,
      paste0(
        "The values of ", record$contrast_vars, " are ",
        and_list(record$contrast_values), "."
      )
    )
  }

  # Other predictors
  held <- record$held
  averaged <- record$averaged
  if (identical(record$estimate, "average")) {
    sentences <- c(
      sentences,
      "Estimates are averages of the predictions for the observed values of the other predictors."
    )
  } else if (identical(record$estimate, "population")) {
    sentences <- c(
      sentences,
      "Estimates are averages of counterfactual predictions for all observations in the sample."
    )
  } else if (length(held) > 0 || length(averaged) > 0) {
    parts <- c(
      if (length(held) > 0) {
        paste0(
          and_list(held),
          if (length(held) == 1) " is held at its mean (" else " are held at their means (",
          and_list(record$held_values),
          ")"
        )
      },
      if (length(averaged) > 0) {
        paste0("estimates are averaged over the levels of ", and_list(averaged))
      }
    )
    sentence <- paste(parts, collapse = ", and ")
    sentences <- c(
      sentences,
      paste0(toupper(substring(sentence, 1, 1)), substring(sentence, 2), ".")
    )
  }

  # Scale of estimates from models with a link function
  if (!identical(record$link, "identity")) {
    link_names <- c(
      logit = "logit",
      probit = "probit",
      log = "log",
      cloglog = "complementary log-log",
      inverse = "inverse",
      sqrt = "square root"
    )
    link <- if (record$link %in% names(link_names)) link_names[[record$link]] else record$link
    what <- switch(record$kind, means = "Estimates", contrasts = "Differences", slopes = "Slopes")
    if (identical(record$predict, "link")) {
      sentences <- c(sentences, paste0(what, " are on the ", link, " scale."))
    } else {
      sentences <- c(
        sentences,
        paste0(what, " are on the response scale (back-transformed from the ", link, " scale).")
      )
    }
  }

  # p-value adjustment
  if (!is.null(record$p_adjust)) {
    adj <- record$p_adjust
    if (identical(adj, "none")) {
      if (record$kind == "contrasts") {
        sentences <- c(sentences, "*p*-values are not adjusted for multiple comparisons.")
      }
    } else {
      method <- if (adj %in% names(p_adjust_names)) p_adjust_names[[adj]] else paste0("the ", adj, " method")
      sentences <- c(
        sentences,
        paste0("*p*-values are adjusted for multiple comparisons with ", method, ".")
      )
    }
  }

  # Abbreviations and probability notes
  c(sentences, parameters_note_sentences(record))
}
