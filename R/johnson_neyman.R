#' Johnson-Neyman regions of significance
#'
#' For a model in which the slope of `trend` depends linearly on a numeric
#' `moderator` (a two-way interaction), the simple slope at moderator value m
#' is b1 + b3 * m, with variance v11 + 2 * m * v13 + m^2 * v33. It is
#' significant where (b1 + b3 * m)^2 > crit^2 * variance, a quadratic
#' inequality in m whose roots are the Johnson-Neyman boundaries (Johnson &
#' Neyman, 1936; Bauer & Curran, 2005).
#' @param model a model with coefficients and a covariance matrix
#' @param trend name of the predictor whose slope is examined
#' @param moderator name of the numeric moderator
#' @param ci confidence level (the significance level is 1 - ci)
#' @param required if `TRUE`, an error is given when the regions cannot be
#'   computed; otherwise `NULL` is returned
#' @param call environment of the function that errors are reported for
#' @returns list with `segments` (tibble of `from`, `to`, `significant`, and
#'   `direction` within the observed range of the moderator), `boundaries`
#'   (roots within the observed range), `range`, `alpha`, and `df` (`Inf` for
#'   z tests), or `NULL`
#' @keywords internal
#' @noRd
johnson_neyman <- function(model, trend, moderator, ci = 0.95, required = FALSE, call = rlang::caller_env()) {
  fail <- function(...) {
    if (required) {
      cli::cli_abort(c(...), call = call, .envir = parent.frame())
    }
    NULL
  }
  if (is.null(model)) {
    return(fail("Johnson-Neyman intervals need the model."))
  }
  info <- tryCatch(insight::model_info(model, verbose = FALSE), error = function(e) NULL)
  if (!isTRUE(info$is_linear) || isTRUE(info$is_bayesian)) {
    return(fail(
      "Johnson-Neyman intervals are computed only for models of continuous outcomes with an identity link (e.g., {.fn lm} or {.fn lme4::lmer})."
    ))
  }
  b <- tryCatch(insight::get_parameters(model, effects = "fixed"), error = function(e) NULL)
  V <- tryCatch(as.matrix(insight::get_varcov(model, component = "conditional", verbose = FALSE)), error = function(e) NULL)
  if (is.null(b) || is.null(V)) {
    return(fail("The coefficients or their covariance matrix could not be found."))
  }
  est <- stats::setNames(b$Estimate, b$Parameter)
  names_b <- names(est)
  interaction <- intersect(c(paste0(trend, ":", moderator), paste0(moderator, ":", trend)), names_b)
  if (!(trend %in% names_b) || length(interaction) != 1) {
    return(fail(
      "Johnson-Neyman intervals need a model with {.var {trend}} and its interaction with {.var {moderator}} as numeric predictors."
    ))
  }
  # The slope must depend on the moderator alone (no other terms with trend)
  with_trend <- names_b[vapply(strsplit(names_b, ":", fixed = TRUE), \(parts) trend %in% parts, logical(1))]
  if (!setequal(with_trend, c(trend, interaction))) {
    return(fail(
      "Johnson-Neyman intervals need the slope of {.var {trend}} to depend only on {.var {moderator}}, but {.var {trend}} is also in {.val {setdiff(with_trend, c(trend, interaction))}}."
    ))
  }
  data <- tryCatch(insight::get_data(model, verbose = FALSE), error = function(e) NULL)
  if (
    is.null(data) || !(moderator %in% colnames(data)) || !is.numeric(data[[moderator]]) ||
      length(unique(stats::na.omit(data[[moderator]]))) <= 2
  ) {
    return(fail("Johnson-Neyman intervals need a continuous moderator (a numeric variable with more than two values)."))
  }
  observed <- range(data[[moderator]], na.rm = TRUE)

  b1 <- est[[trend]]
  b3 <- est[[interaction]]
  v11 <- V[trend, trend]
  v33 <- V[interaction, interaction]
  v13 <- V[trend, interaction]
  df <- if (inherits(model, "lm") && !inherits(model, "glm")) stats::df.residual(model) else Inf
  alpha <- 1 - ci
  crit <- if (is.finite(df)) stats::qt(1 - alpha / 2, df) else stats::qnorm(1 - alpha / 2)

  slope <- function(m) b1 + b3 * m
  se <- function(m) sqrt(v11 + 2 * m * v13 + m^2 * v33)
  significant <- function(m) abs(slope(m)) / se(m) >= crit

  # Roots of (b3^2 - c^2 v33) m^2 + 2 (b1 b3 - c^2 v13) m + (b1^2 - c^2 v11) = 0
  qa <- b3^2 - crit^2 * v33
  qb <- 2 * (b1 * b3 - crit^2 * v13)
  qc <- b1^2 - crit^2 * v11
  roots <- numeric(0)
  if (abs(qa) > .Machine$double.eps) {
    disc <- qb^2 - 4 * qa * qc
    if (disc >= 0) {
      roots <- sort((-qb + c(-1, 1) * sqrt(disc)) / (2 * qa))
    }
  } else if (abs(qb) > .Machine$double.eps) {
    roots <- -qc / qb # nocov
  }
  boundaries <- roots[roots > observed[1] & roots < observed[2]]
  cuts <- c(observed[1], boundaries, observed[2])
  mids <- (utils::head(cuts, -1) + utils::tail(cuts, -1)) / 2
  segments <- tibble::tibble(
    from = utils::head(cuts, -1),
    to = utils::tail(cuts, -1),
    significant = significant(mids),
    direction = ifelse(slope(mids) > 0, "positive", "negative")
  )
  list(
    segments = segments,
    boundaries = boundaries,
    roots = roots,
    range = observed,
    alpha = alpha,
    df = df
  )
}

#' Note sentence of Johnson-Neyman regions
#' @param jn list from `johnson_neyman()`, with `trend` and `moderator`
#'   labels added
#' @keywords internal
#' @noRd
johnson_neyman_sentence <- function(jn) {
  if (length(jn) == 0) {
    return(character(0))
  }
  num <- function(v) trimws(align_chr(v, accuracy = the$accuracy))
  p_text <- paste0("*p* \u2264 ", sub("^0", "", format(jn$alpha)))
  seg <- jn$segments
  range_text <- paste0(num(jn$range[1]), " to ", num(jn$range[2]))
  sig <- seg[seg$significant, , drop = FALSE]
  where <- if (nrow(sig) == 0) {
    paste0(" is not significant (", p_text, ") anywhere in the observed range of ", jn$moderator, " (", range_text, ")")
  } else if (nrow(sig) == nrow(seg)) {
    paste0(" is significant (", p_text, ") across the observed range of ", jn$moderator, " (", range_text, ")")
  } else {
    parts <- paste0(num(sig$from), " to ", num(sig$to))
    paste0(
      " is significant (", p_text, ") when ", jn$moderator, " is from ",
      and_list(parts),
      " (observed range: ", range_text, ")"
    )
  }
  paste0(
    "Johnson", intToUtf8(8211), "Neyman intervals ",
    cite("johnsonTestsCertainLinear1936"),
    ": the slope of ", jn$trend, where, "."
  )
}
