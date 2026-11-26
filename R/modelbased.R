#' Estimated marginal means, contrasts, and slopes tables
#'
#' Makes APA-style tables of estimated marginal means, contrasts between them
#' (e.g., pairwise comparisons after an ANOVA), and simple slopes (e.g., to
#' probe a moderation effect), computed with the
#' [modelbased](https://easystats.github.io/modelbased/) package.
#'
#' `x` is either a model or the output of [modelbased::estimate_means()],
#' [modelbased::estimate_contrasts()], or [modelbased::estimate_slopes()].
#' With a model, `by`, `contrast`, `trend`, and the arguments in `...` (e.g.,
#' `p_adjust`) are passed to the modelbased function, so
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
#' In `apa_contrasts()`, contrasts from models of continuous outcomes with an
#' identity link (e.g., `lm()` and `aov()`) get Cohen's *d* with its
#' confidence interval: each difference divided by the model's residual
#' standard deviation (\eqn{\hat{\sigma}}{sigma}), as in
#' `emmeans::eff_size()`. The interval accounts for the uncertainty in
#' \eqn{\hat{\sigma}}{sigma}. For mixed models, there is no single right
#' standard deviation to divide by, so *d* is shown only when it is given with
#' `sd` (e.g., the
#' square root of the sum of the variance components and the residual
#' variance). *d* is not computed for logistic and other generalized linear
#' models.
#'
#' In `apa_slopes()`, when the slope of `trend` depends on a single
#' continuous moderator in `by` through a two-way interaction (e.g.,
#' `lm(y ~ x * m)`), the note gives the Johnson-Neyman intervals (Johnson &
#' Neyman, 1936): the values of the moderator, within its observed range, at
#' which the slope is significant. The boundaries are computed exactly from
#' the coefficients and their covariance matrix (the values of the moderator
#' at which the slope's test statistic equals its critical value), with *t*
#' and the residual degrees of freedom for `lm()` models and the normal
#' distribution for other models (e.g., mixed models). They are not computed
#' for models with a link function (e.g., logistic regression), or when the
#' slope also depends on other variables (e.g., a three-way interaction).
#'
#' Bayesian models are not yet supported.
#'
#' @param x a model, or output of [modelbased::estimate_means()] (for
#'   `apa_means()`), [modelbased::estimate_contrasts()] (for
#'   `apa_contrasts()`), or [modelbased::estimate_slopes()] (for
#'   `apa_slopes()`)
#' @param by variables whose levels (or values) the estimates are computed at
#'   (e.g., `by = Species`, `by = c(am, cyl)`, `by = "hp=[sd]"`, or
#'   `by = list(hp = "[sd]")`). Variable
#'   names can be quoted or unquoted, but values (e.g., `"hp=[fivenum]"`) must
#'   be quoted. In `apa_means()` and `apa_slopes()`, the last variable labels
#'   the rows and the others get heading rows; in `apa_contrasts()`, all of
#'   them get heading rows. Passed to the modelbased function.
#' @param contrast variable whose levels (or values) are compared (e.g.,
#'   `contrast = cyl`), quoted or unquoted. Passed to
#'   [modelbased::estimate_contrasts()].
#' @param trend predictor whose slopes are estimated (e.g., `trend = wt`),
#'   quoted or unquoted. Passed to [modelbased::estimate_slopes()].
#' @param ... other arguments passed to the modelbased function when `x` is a
#'   model (e.g., `p_adjust = "holm"`, `predict = "link"`, or
#'   `estimate = "average"`)
#' @param starred columns to which significance stars are added (e.g., `"p"`
#'   or `"Estimate"`)
#' @param bolded columns that are bolded when *p* \eqn{\le}{<=} .05
#' @param column_formats [column_formats()] object. If `NULL` (default), the
#'   defaults are used. The estimate column is named `"Estimate"`.
#' @param johnson_neyman in `apa_slopes()`, whether the note gives the
#'   Johnson-Neyman intervals. If `NULL` (default), they are given when they
#'   can be computed (see Details). If `TRUE`, an error is given when they
#'   cannot be computed.
#' @param effect_size in `apa_contrasts()`, whether to add Cohen's *d* and its
#'   confidence interval. If `NULL` (default), *d* is added when it can be
#'   computed (models of continuous outcomes with an identity link, such as
#'   `lm()`, or any model when `sd` is given). If `TRUE`, an error is given
#'   when it cannot be computed.
#' @param sd in `apa_contrasts()`, the standard deviation that differences are
#'   divided by to compute Cohen's *d*. If `NULL` (default), the model's
#'   residual standard deviation is used (not for mixed models).
#' @param format_variable_names if `TRUE` (default), variable names are
#'   formatted (e.g., `"Sepal.Length"` becomes "Sepal Length"). Factor levels
#'   are shown as they are.
#'
#' @references
#' Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery
#' rate: A practical and powerful approach to multiple testing. *Journal of the
#' Royal Statistical Society: Series B (Methodological), 57*(1), 289--300.
#' \doi{10.1111/j.2517-6161.1995.tb02031.x}
#'
#' Holm, S. (1979). A simple sequentially rejective multiple test
#' procedure. *Scandinavian Journal of Statistics, 6*(2), 65--70.
#'
#' Johnson, P. O., & Neyman, J. (1936). Tests of certain linear hypotheses
#' and their application to some educational problems. *Statistical Research
#' Memoirs, 1*, 57--93.
#'
#' Kramer, C. Y. (1956). Extension of multiple range tests to group means
#' with unequal numbers of replications. *Biometrics, 12*(3), 307--310.
#' \doi{10.2307/3001469}
#'
#' Tukey, J. W. (1994). The problem of multiple comparisons. In H. I.
#' Braun (Ed.), *The collected works of John W. Tukey: Vol. VIII. Multiple
#' comparisons, 1948--1983* (pp. 1--300). Chapman & Hall.
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
  by = NULL,
  ...,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  format_variable_names = TRUE
) {
  x <- modelbased_call(x, "estimate_means", list(by = rlang::enquo(by)), ...)
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
  contrast = NULL,
  by = NULL,
  ...,
  effect_size = NULL,
  sd = NULL,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  format_variable_names = TRUE
) {
  if (!is.null(effect_size) && !(isTRUE(effect_size) || isFALSE(effect_size))) {
    cli::cli_abort("{.arg effect_size} must be {.code NULL}, {.code TRUE}, or {.code FALSE}.")
  }
  if (!is.null(sd) && !(is.numeric(sd) && length(sd) == 1 && !is.na(sd) && sd > 0)) {
    cli::cli_abort("{.arg sd} must be {.code NULL} or a single positive number.")
  }
  x <- modelbased_call(x, "estimate_contrasts", list(contrast = rlang::enquo(contrast), by = rlang::enquo(by)), ...)
  modelbased_table(
    x,
    kind = "contrasts",
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    format_variable_names = format_variable_names,
    effect_size = effect_size,
    sd = sd
  )
}

#' @rdname apa_means
#' @export
apa_slopes <- function(
  x,
  trend = NULL,
  by = NULL,
  ...,
  johnson_neyman = NULL,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  format_variable_names = TRUE
) {
  if (!is.null(johnson_neyman) && !(isTRUE(johnson_neyman) || isFALSE(johnson_neyman))) {
    cli::cli_abort("{.arg johnson_neyman} must be {.code NULL}, {.code TRUE}, or {.code FALSE}.")
  }
  x <- modelbased_call(x, "estimate_slopes", list(trend = rlang::enquo(trend), by = rlang::enquo(by)), ...)
  modelbased_table(
    x,
    kind = "slopes",
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    format_variable_names = format_variable_names,
    johnson_neyman = johnson_neyman
  )
}

#' modelbased output, computed from a model if needed, with numeric
#' variables named in `by` without values set to their mean and 1 SD above
#' and below it
#' @keywords internal
#' @noRd
modelbased_call <- function(x, fun, variable_args, ...) {
  args <- rlang::enquos(...)
  # Variable arguments that were given
  variable_args <- Filter(\(q) !rlang::quo_is_null(q), variable_args)
  if (inherits(x, "data.frame")) {
    # modelbased output: the arguments are not evaluated, because unquoted
    # variable names cannot be found without the model's data
    if ((length(args) > 0 || length(variable_args) > 0) && inherits(x, fun)) {
      cli::cli_warn(
        "Arguments in {.arg ...} are ignored because {.arg x} is already output of {.fn modelbased::{fun}}."
      )
    }
    return(modelbased_estimates(x, fun))
  }
  data <- tryCatch(
    insight::get_data(x, verbose = FALSE),
    error = function(e) NULL
  )
  variables <- if (is.null(data)) character(0) else colnames(data)
  # by, contrast, and trend take quoted or unquoted variable names
  values <- lapply(variable_args, modelbased_variable_names, variables = variables)
  values <- c(values, lapply(args, rlang::eval_tidy))
  # Named lists (e.g., list(x = "[sd]")) become strings (e.g., "x=[sd]"),
  # which modelbased handles in all of its functions
  for (arg in intersect(c("by", "contrast"), names(values))) {
    values[[arg]] <- modelbased_spec_strings(values[[arg]])
  }
  if (!is.null(values$by)) {
    values$by <- modelbased_numeric_by(data, values$by)
  }
  # marginaleffects computes wrong standard errors (0 or NA) when variable
  # names are not syntactic (e.g., `Partner Status`), so the model is refit
  # with syntactic names, and the original names are restored in the table
  renamed <- modelbased_syntactic_model(x, data)
  if (!is.null(renamed)) {
    x <- renamed$model
    for (arg in intersect(c("by", "contrast", "trend"), names(values))) {
      values[[arg]] <- modelbased_rename_specs(values[[arg]], renamed$names)
    }
  }
  out <- do.call(modelbased_estimates, c(list(x, fun), values))
  if (!is.null(renamed)) {
    # Syntactic names, named by the original names
    attr(out, "apa7_original_names") <- stats::setNames(names(renamed$names), renamed$names)
  }
  out
}

#' The model refit with syntactic variable names
#'
#' @param model a model
#' @param data the model's data
#' @returns `NULL` if all of the model's variables have syntactic names (or the
#'   model cannot be refit), otherwise a list with the refit `model` and
#'   `names`, the syntactic names named by the original names
#' @keywords internal
#' @noRd
modelbased_syntactic_model <- function(model, data) {
  if (is.null(data)) {
    return(NULL)
  }
  variables <- tryCatch(
    insight::find_variables(model, flatten = TRUE, verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(variables)) {
    variables <- colnames(data)
  }
  variables <- intersect(variables, colnames(data))
  bad <- variables[make.names(variables) != variables]
  if (length(bad) == 0) {
    return(NULL)
  }
  taken <- colnames(data)
  new_names <- character(0)
  for (b in bad) {
    candidate <- make.names(b)
    while (candidate %in% c(taken, new_names)) {
      candidate <- paste0(candidate, "_")
    }
    new_names <- c(new_names, candidate)
  }
  names(new_names) <- bad
  fail <- function(reason = NULL) {
    cli::cli_warn(c(
      "Variable names that are not syntactic ({.val {bad}}) can make standard errors, tests, and confidence intervals wrong, and the model could not be refit with syntactic names.",
      "i" = "Rename the variables (e.g., {.code {make.names(bad[1])}}) and fit the model again.",
      if (!is.null(reason)) c("x" = reason)
    ))
    NULL
  }

  cl <- insight::get_call(model)
  if (is.null(cl) || !is.call(cl)) {
    return(fail())
  }
  env <- tryCatch(environment(stats::formula(model)), error = function(e) NULL)
  if (is.null(env)) {
    env <- globalenv()
  }
  rename_symbols <- function(e) {
    if (is.symbol(e) && as.character(e) %in% names(new_names)) {
      return(as.name(new_names[[as.character(e)]]))
    }
    if (is.call(e)) {
      return(as.call(lapply(as.list(e), rename_symbols)))
    }
    e
  }
  args <- as.list(cl)[-1]
  arg_names <- if (is.null(names(args))) rep("", length(args)) else names(args)
  for (i in seq_along(args)) {
    if (identical(arg_names[i], "data")) {
      next
    }
    a <- args[[i]]
    # A formula stored in an object (e.g., lm(f, data = d))
    if (is.symbol(a)) {
      value <- tryCatch(eval(a, env), error = function(e) NULL)
      if (inherits(value, "formula")) {
        a <- value
      }
    }
    if (inherits(a, "formula")) {
      f_env <- environment(a)
      a <- rename_symbols(a)
      a <- stats::as.formula(a, env = f_env)
    } else {
      a <- rename_symbols(a)
    }
    if (is.null(a)) {
      args[i] <- list(NULL)
    } else {
      args[[i]] <- a
    }
  }
  renamed_data <- data
  colnames(renamed_data)[match(bad, colnames(renamed_data))] <- new_names
  refit_env <- new.env(parent = env)
  assign(".apa7_data", renamed_data, envir = refit_env)
  args$data <- as.name(".apa7_data")
  new_call <- as.call(c(cl[[1]], args))
  refit <- tryCatch(eval(new_call, refit_env), error = function(e) e)
  if (inherits(refit, "error")) {
    return(fail(conditionMessage(refit)))
  }
  # The refit model must be the same model
  same <- tryCatch(
    isTRUE(all.equal(
      as.numeric(stats::logLik(model)),
      as.numeric(stats::logLik(refit)),
      tolerance = 1e-6
    )),
    error = function(e) {
      isTRUE(all.equal(
        unname(insight::get_parameters(model)$Estimate),
        unname(insight::get_parameters(refit)$Estimate),
        tolerance = 1e-6
      ))
    }
  )
  if (!same) {
    return(fail("The refit model differs from the original model."))
  }
  list(model = refit, names = new_names)
}

#' Variable names in specifications (e.g., `"x y=[sd]"`) replaced with
#' syntactic names
#' @param specs character vector of specifications, or other values (returned
#'   unchanged)
#' @param new_names syntactic names named by the original names
#' @keywords internal
#' @noRd
modelbased_rename_specs <- function(specs, new_names) {
  if (!is.character(specs)) {
    return(specs)
  }
  vapply(
    specs,
    \(s) {
      name <- trimws(sub("=.*$", "", s))
      if (!name %in% names(new_names)) {
        return(s)
      }
      rest <- if (grepl("=", s, fixed = TRUE)) sub("^[^=]*", "", s) else ""
      paste0(new_names[[name]], rest)
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' Specifications as strings
#'
#' Turns a named list or named vector (e.g., `list(x = "[sd]", g = NULL)` or
#' `list(x = c(1, 2))`) into strings (e.g., `"x=[sd]"` or `"x=c(1, 2)"`).
#' Unnamed elements are variable names. Other inputs are returned unchanged.
#' @keywords internal
#' @noRd
modelbased_spec_strings <- function(x) {
  if (!is.list(x) && is.null(names(x))) {
    return(x)
  }
  if (is.list(x) && is.null(names(x))) {
    return(unlist(x))
  }
  nms <- names(x)
  out <- character(0)
  for (i in seq_along(x)) {
    value <- x[[i]]
    name <- nms[i]
    if (is.na(name) || !nzchar(name)) {
      out <- c(out, as.character(value))
    } else if (is.null(value)) {
      out <- c(out, name)
    } else if (is.character(value) && length(value) == 1 && grepl("^\\[.*\\]$", value)) {
      out <- c(out, paste0(name, "=", value))
    } else if (is.numeric(value)) {
      out <- c(out, paste0(name, "=c(", paste(value, collapse = ", "), ")"))
    } else {
      out <- c(out, paste0(name, "=c(", paste0("'", as.character(value), "'", collapse = ", "), ")"))
    }
  }
  out
}

#' Variable names from a quoted or unquoted argument
#'
#' A name (e.g., `hp`) or a `c()` of names and strings (e.g.,
#' `c(am, "hp=[sd]")`) that are variables in the model's data become strings.
#' Anything else is evaluated (e.g., `"hp"`, or an object holding names).
#' @param q quosure of the argument
#' @param variables names of the variables in the model's data
#' @keywords internal
#' @noRd
modelbased_variable_names <- function(q, variables) {
  expr <- rlang::quo_get_expr(q)
  as_name <- function(e) {
    if (is.symbol(e) && as.character(e) %in% variables) {
      return(as.character(e))
    }
    if (is.character(e) && length(e) == 1) {
      return(e)
    }
    NULL
  }
  name <- as_name(expr)
  if (!is.null(name)) {
    return(name)
  }
  if (is.call(expr) && identical(expr[[1]], quote(c))) {
    parts <- lapply(as.list(expr)[-1], as_name)
    if (length(parts) > 0 && !any(vapply(parts, is.null, logical(1)))) {
      return(unlist(parts))
    }
  }
  tryCatch(
    rlang::eval_tidy(q),
    error = function(e) {
      unknown <- setdiff(all.vars(expr), variables)
      if (length(unknown) > 0 && length(variables) > 0) {
        cli::cli_abort(
          "{.val {unknown}} {?is/are} not {?a variable/variables} in the model's data.",
          call = NULL
        )
      }
      rlang::cnd_signal(e)
    }
  )
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
modelbased_numeric_by <- function(data, by) {
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
  # Specifications are "x=[sd]", or "x = c('[sd]')" when by is a list (e.g.,
  # list(x = "[sd]"))
  specs <- gsub("\\s", "", as.character(specs))
  pattern <- "^(.+)=(c\\()?['\"]?\\[([A-Za-z0-9]+)\\]['\"]?\\)?$"
  matched <- grepl(pattern, specs) & sub(pattern, "\\1", specs) == var
  if (sum(matched) != 1) {
    return(NULL)
  }
  sub(pattern, "\\3", specs[matched])
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
  format_variable_names = TRUE,
  effect_size = FALSE,
  sd = NULL,
  johnson_neyman = FALSE
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

  # Original names of variables that were renamed to refit the model
  original <- a$apa7_original_names
  label <- function(v) {
    if (!is.null(original) && length(v) > 0) {
      v <- ifelse(v %in% names(original), unname(original[v]), v)
    }
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
    contrast_names <- trimws(sub("=.*$", "", contrast_vars))
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

  # Standardized differences (Cohen's d)
  es <- NULL
  if (kind == "contrasts" && !isFALSE(effect_size)) {
    es <- contrast_effect_sizes(
      d,
      model = a$model,
      info = info,
      probability = probability,
      sd = sd,
      ci = ci,
      required = isTRUE(effect_size)
    )
  }
  if (!is.null(es)) {
    d_header <- "*d*"
    d_ci_header <- paste0(round(ci * 100), "% CI for *d*")
    tbl[[d_header]] <- align_chr(es$d, accuracy = the$accuracy)
    tbl[[d_ci_header]] <- ci_md(
      align_chr(es$CI_low, accuracy = the$accuracy),
      align_chr(es$CI_high, accuracy = the$accuracy)
    )
    # After the CI of the difference (before the test statistic)
    before <- intersect(c(grep("CI$", colnames(tbl), value = TRUE)[1]), colnames(tbl))
    if (length(before) == 1 && !is.na(before)) {
      tbl <- dplyr::relocate(tbl, dplyr::all_of(c(d_header, d_ci_header)), .after = dplyr::all_of(before))
    }
    record$headers <- c(record$headers, stats::setNames(c(d_header, d_ci_header), c("d", "d_CI")))
  }

  # Johnson-Neyman regions of significance for a single numeric moderator
  jn <- NULL
  if (kind == "slopes" && !isFALSE(johnson_neyman)) {
    moderator <- setdiff(focal, trend)
    if (length(moderator) == 1 && is.numeric(d[[moderator]])) {
      jn <- johnson_neyman(
        a$model,
        trend,
        moderator,
        ci = ci,
        required = isTRUE(johnson_neyman),
        call = rlang::caller_env()
      )
      if (!is.null(jn)) {
        jn$trend <- label(trend)
        jn$moderator <- label(moderator)
      }
    } else if (isTRUE(johnson_neyman)) {
      cli::cli_abort("Johnson-Neyman intervals need exactly one numeric moderator in {.arg by}.", call = rlang::caller_env())
    }
  }

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
  record$johnson_neyman <- jn
  record$effect_size <- if (!is.null(es)) list(sd = attr(es, "sd"), source = attr(es, "source"), ci = ci)

  attr(tbl, "apa_note") <- record
  attr(tbl, "no_format_columns") <- first_header
  as_apa7_tbl(labels_formatted(tbl))
}

#' Names of p-value adjustment methods
#' @keywords internal
#' @noRd
p_adjust_names <- function() c(
  holm = paste0("Holm's method ", cite("holmSimpleSequentiallyRejective1979")),
  bonferroni = "the Bonferroni correction",
  tukey = paste0("Tukey's method ", cite(c("kramerExtensionMultipleRange1956", "tukeyProblemMultipleComparisons1994"))),
  hochberg = "Hochberg's method",
  hommel = "Hommel's method",
  BH = paste0("the Benjamini\u2013Hochberg false discovery rate correction ", cite("benjaminiControllingFalseDiscovery1995")),
  fdr = paste0("the Benjamini\u2013Hochberg false discovery rate correction ", cite("benjaminiControllingFalseDiscovery1995")),
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

  # Johnson-Neyman regions of significance
  sentences <- c(sentences, johnson_neyman_sentence(record$johnson_neyman))

  # Standardized differences
  es <- record$effect_size
  if (length(es) > 0) {
    sd_text <- trimws(align_chr(es$sd, accuracy = the$accuracy))
    sentences <- c(
      sentences,
      if (identical(es$source, "sigma")) {
        paste0(
          "*d* = difference divided by the residual standard deviation of the model (",
          sd_text,
          "); its confidence interval accounts for the uncertainty in the residual standard deviation."
        )
      } else {
        paste0("*d* = difference divided by ", sd_text, ".")
      }
    )
  }

  # p-value adjustment
  if (!is.null(record$p_adjust)) {
    adj <- record$p_adjust
    if (identical(adj, "none")) {
      if (record$kind == "contrasts") {
        sentences <- c(sentences, "*p*-values are not adjusted for multiple comparisons.")
      }
    } else {
      method <- if (adj %in% names(p_adjust_names())) p_adjust_names()[[adj]] else paste0("the ", adj, " method")
      sentences <- c(
        sentences,
        paste0("*p*-values are adjusted for multiple comparisons with ", method, ".")
      )
    }
  }

  # Abbreviations and probability notes
  c(sentences, parameters_note_sentences(record))
}

#' Cohen's d for contrasts
#'
#' Each difference is divided by the model's residual standard deviation (or
#' `sd`). The standard error of d accounts for the uncertainty of the
#' residual SD: \eqn{SE_d^2 = (SE/\sigma)^2 + d^2 / (2 df)}, as in
#' `emmeans::eff_size()`.
#' @param d data frame of modelbased contrasts
#' @param model the model, or NULL
#' @param info modelbased model information
#' @param probability whether the contrasts are differences in probabilities
#' @param sd standardizer given by the user, or NULL
#' @param ci confidence level
#' @param required if TRUE, an error is given when d cannot be computed
#' @returns tibble with d, CI_low, and CI_high (with attributes sd and source),
#'   or NULL
#' @keywords internal
#' @noRd
contrast_effect_sizes <- function(d, model, info, probability, sd = NULL, ci = 0.95, required = FALSE) {
  fail <- function(...) {
    if (required) {
      cli::cli_abort(c(...), call = rlang::caller_env(3))
    }
    NULL
  }
  if (!("Difference" %in% colnames(d))) {
    return(fail("Effect sizes need differences between means."))
  }
  link <- if (is.null(info$link_function)) "identity" else info$link_function
  if (probability || !identical(link, "identity")) {
    return(fail(
      "Cohen's {.emph d} is computed only for models of continuous outcomes with an identity link (e.g., {.fn lm} or {.fn aov}).",
      i = "For logistic and other generalized linear models, differences are not on a standardized scale."
    ))
  }
  source <- "user"
  df <- if ("df" %in% colnames(d) && all(is.finite(d$df))) d$df else NULL
  if (is.null(sd)) {
    if (isTRUE(info$is_mixed)) {
      return(fail(
        "For mixed models, give the standard deviation to standardize the differences with {.arg sd}.",
        i = "For example, the square root of the sum of the variance components and the residual variance, or the residual {.emph SD} for within-cluster differences."
      ))
    }
    sigma <- if (is.null(model)) NULL else tryCatch(insight::get_sigma(model, ci = NULL, verbose = FALSE), error = function(e) NULL)
    sigma <- suppressWarnings(as.numeric(sigma))
    if (length(sigma) != 1 || is.na(sigma) || sigma <= 0) {
      return(fail(
        "Cohen's {.emph d} needs the model's residual standard deviation, which could not be found.",
        i = "Give the standard deviation with {.arg sd}."
      ))
    }
    sd <- sigma
    source <- "sigma"
    sigma_df <- tryCatch(insight::get_df(model, type = "residual"), error = function(e) NULL)
    if (length(sigma_df) == 1 && is.finite(sigma_df)) {
      df <- rep(sigma_df, nrow(d))
    }
  }
  es <- d$Difference / sd
  se <- if ("SE" %in% colnames(d)) d$SE / sd else rep(NA_real_, nrow(d))
  # The standardizer's uncertainty, when it was estimated from the model
  if (source == "sigma" && !is.null(df)) {
    se <- sqrt(se^2 + es^2 / (2 * df))
  }
  crit <- if (is.null(df)) stats::qnorm(1 - (1 - ci) / 2) else stats::qt(1 - (1 - ci) / 2, df)
  out <- tibble::tibble(d = es, CI_low = es - crit * se, CI_high = es + crit * se)
  attr(out, "sd") <- sd
  attr(out, "source") <- source
  out
}
