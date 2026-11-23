#' @param studies for meta-analyses (`metafor::rma()`), if `TRUE`, the table
#'   has a row for each study (its effect size, confidence interval, and
#'   weight) followed by the pooled estimate. If `FALSE`, the table has the
#'   model's coefficients (the pooled estimate, or the intercept and
#'   moderators of a meta-regression). If `NULL` (default), `TRUE` for models
#'   without moderators and `FALSE` for meta-regressions.
#' @param weights for the study rows of meta-analyses, if `TRUE` (default),
#'   each study's percentage of the total weight is shown
#' @param transform for meta-analyses, if `TRUE` (default), effect sizes
#'   computed on a transformed scale are shown on their original scale: log
#'   risk ratios, odds ratios, incidence rate ratios, and ratios of means are
#'   exponentiated, and Fisher's *z*-transformed correlations are converted
#'   back to correlations. In meta-regressions, exponentiated coefficients are
#'   added in a separate column. If `FALSE`, estimates stay on the scale on
#'   which they were analyzed.
#' @details
#' For meta-analyses fit with `metafor::rma()` (random-effects or
#' equal-effects models, without or with moderators), the table of a model
#' without moderators has a row for each study, labeled by the study labels
#' (`slab`), with its effect size, confidence interval, and percentage of the
#' total weight, followed by the pooled estimate under a rule. The column of
#' effect sizes is named by the measure given to `metafor::escalc()` (e.g.,
#' *g* for standardized mean differences, *r* for correlations, and *OR* for
#' odds ratios). [apa_note()] names the measure and the model (including the
#' estimator of \eqn{\tau^2}{tau^2}), and gives the test of the pooled
#' estimate, its prediction interval, and the test of heterogeneity. The
#' table of a meta-regression has the intercept and moderators, as for other
#' regression models. [apa_performance()] gives the heterogeneity statistics.
#'
#' Multilevel meta-analyses fit with `metafor::rma.mv()` (e.g., several effect
#' sizes nested in studies, with `random = ~ 1 | study / effect`) get the
#' same tables, with a row for each effect size. [apa_note()] names the
#' levels and their numbers of units, and gives each level's variance
#' component (\eqn{\sigma^2}{sigma^2}) and the multilevel \eqn{I^2}{I^2}
#' (Cheung, 2014). Random effects with an `~ inner | outer` structure (e.g.,
#' correlated outcomes within studies) are not yet supported.
#' @references Cheung, M. W.-L. (2014). Modeling dependent effect sizes with
#'   three-level meta-analyses: A structural equation modeling approach.
#'   *Psychological Methods, 19*(2), 211--229. \doi{10.1037/a0032968}
#' @export
#' @rdname apa_parameters
#' @examples
#' \donttest{
#' if (requireNamespace("metafor", quietly = TRUE) &&
#'     requireNamespace("metadat", quietly = TRUE)) {
#'   d_bcg <- metafor::escalc(
#'     measure = "RR",
#'     ai = tpos, bi = tneg, ci = cpos, di = cneg,
#'     data = metadat::dat.bcg,
#'     slab = paste(author, year)
#'   )
#'   fit_meta <- metafor::rma(yi, vi, data = d_bcg)
#'   apa_parameters(fit_meta)
#'   apa_performance(fit_meta)
#'
#'   # Meta-regression
#'   metafor::rma(yi, vi, mods = ~ ablat, data = d_bcg) |>
#'     apa_parameters()
#' }
#' }
apa_parameters.rma <- function(
  fit,
  predictor_parameters = c(
    "Parameter",
    "Coefficient",
    "SE",
    "CI",
    "Exp_Coefficient",
    "z",
    "t",
    "df_error",
    "p"
  ),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  studies = NULL,
  weights = TRUE,
  transform = TRUE,
  ...
) {
  meta_check_class(fit)
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }
  int_only <- isTRUE(fit$int.only)
  if (is.null(studies)) {
    studies <- int_only
  }
  if (isTRUE(studies) && !int_only) {
    cli::cli_abort(c(
      "{.code studies = TRUE} needs a model without moderators.",
      i = "Use {.code studies = FALSE} for the coefficients of a meta-regression."
    ))
  }
  info <- meta_measure(fit$measure, transform)
  if (isTRUE(studies)) {
    return(meta_study_table(fit, info, weights = weights, column_formats = column_formats))
  }

  names(predictor_parameters) <- NULL
  b <- as.vector(fit$b)
  is_t <- meta_t_test(fit)
  d_b <- tibble::tibble(
    Parameter = sub("^intrcpt$", "(Intercept)", rownames(fit$b)),
    Coefficient = b,
    SE = fit$se,
    CI = meta_level(fit),
    CI_low = fit$ci.lb,
    CI_high = fit$ci.ub
  )
  if (is_t) {
    d_b$t <- fit$zval
    d_b$df_error <- fit$ddf[1]
  } else {
    d_b$z <- fit$zval
  }
  d_b$p <- fit$pval
  # Names are formatted here, so that the pooled estimate's label is kept
  d_b$Parameter <- column_formats$Parameter@formatter(d_b$Parameter)
  if (int_only) {
    d_b$Parameter <- meta_pooled_label(fit)
  }
  column_formats$Parameter@formatter <- \(x, ...) x

  if (isTRUE(transform) && identical(info[["transform"]], "exp") && !int_only) {
    d_b <- add_exp_columns(d_b, predictor_parameters)
    column_formats <- label_exp_columns(
      column_formats,
      c(header = info$header, latex = paste0("$", info$symbol, "$"), abbreviation = info$singular %||% info$name)
    )
  } else if (isTRUE(transform) && !is.null(info[["transform"]]) && int_only) {
    # The pooled estimate is shown on the original scale
    f <- meta_transform_function(info[["transform"]])
    d_b$Coefficient <- f(d_b$Coefficient)
    d_b$CI_low <- f(d_b$CI_low)
    d_b$CI_high <- f(d_b$CI_high)
    predictor_parameters <- setdiff(predictor_parameters, c("SE", "Exp_Coefficient", "Exp_CI"))
  } else {
    predictor_parameters <- setdiff(predictor_parameters, c("Exp_Coefficient", "Exp_CI"))
  }

  # The pooled estimate is labeled by its measure (e.g., *g*)
  if (int_only && !is.null(column_formats$Coefficient)) {
    column_formats$Coefficient@header <- info$header
    column_formats$Coefficient@latex <- info$header
  }

  format_apa_parameters(
    d_b,
    predictor_parameters = predictor_parameters,
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    t_with_df = t_with_df,
    note = list(meta = meta_record(fit, info, studies = FALSE))
  ) |>
    labels_formatted()
}

#' @details
#' For meta-analyses (`metafor::rma()`), the metrics are the number of
#' studies (`"k"`), the test of residual heterogeneity (`"QE"` and `"p_QE"`,
#' with its *df* in the header), \eqn{\tau^2}{tau^2} (`"tau2"`) and
#' \eqn{\tau}{tau} (`"tau"`), \eqn{I^2}{I^2} (`"I2"`), \eqn{H^2}{H^2}
#' (`"H2"`), confidence intervals of \eqn{\tau^2}{tau^2} and \eqn{I^2}{I^2}
#' (`"tau2_CI"` and `"I2_CI"`, from [stats::confint()]), and, for
#' meta-regressions, the omnibus test of the moderators (`"QM"` and `"p_QM"`)
#' and the proportion of heterogeneity they explain (`"R2"`). The defaults are
#' `"k"`, `"QE"`, `"p_QE"`, `"tau2"`, and `"I2"`, plus `"QM"`, `"p_QM"`, and
#' `"R2"` for meta-regressions. Equal-effects models have no
#' \eqn{\tau^2}{tau^2}.
#'
#' For multilevel meta-analyses (`metafor::rma.mv()`), `"k"` is the number of
#' effect sizes, and the variance components are reported for each level:
#' \eqn{\sigma^2}{sigma^2} (`"sigma2"`), \eqn{\sigma}{sigma} (`"sigma"`), and
#' their confidence intervals (`"sigma2_CI"`), with the level as a subscript.
#' `"I2"` is the multilevel \eqn{I^2}{I^2} for all levels together, and
#' `"I2_levels"` gives it for each level (Cheung, 2014). The defaults are
#' `"k"`, `"QE"`, `"p_QE"`, `"sigma2"`, and `"I2"`, plus `"QM"` and `"p_QM"`
#' for meta-regressions.
#' @export
#' @rdname apa_performance
apa_performance.rma <- function(
  fit,
  metrics = NULL,
  column_formats = NULL
) {
  meta_check_class(fit)
  int_only <- isTRUE(fit$int.only)
  multilevel <- meta_multilevel(fit)
  is_mv <- inherits(fit, "rma.mv")
  random <- if (is_mv) !is.null(multilevel) else !(fit$method %in% c("EE", "FE", "CE"))
  if (is.null(metrics)) {
    metrics <- c(
      "k",
      "QE",
      "p_QE",
      if (random && is_mv) c("sigma2", "I2"),
      if (random && !is_mv) c("tau2", "I2"),
      if (!int_only) c("QM", "p_QM", if (random && !is_mv) "R2")
    )
  }
  if (any(metrics == "all")) {
    metrics <- c(
      "k", "QE", "p_QE",
      if (random && is_mv) c("sigma2", "sigma2_CI", "sigma", "I2", "I2_levels"),
      if (random && !is_mv) c("tau2", "tau2_CI", "tau", "I2", "I2_CI", "H2"),
      if (!int_only) c("QM", "p_QM", if (random && !is_mv) "R2")
    )
  }
  univariate <- c("tau2", "tau2_CI", "tau", "I2_CI", "H2", "R2")
  multilevel_only <- c("sigma2", "sigma2_CI", "sigma", "I2_levels")
  allowed <- c("k", "QE", "p_QE", univariate, multilevel_only, "I2", "QM", "p_QM")
  unknown <- setdiff(metrics, allowed)
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown {cli::qty(unknown)} metric{?s} for meta-analyses: {.val {unknown}}. Use any of {.val {allowed}}.")
  }
  metrics <- setdiff(metrics, if (is_mv) univariate else multilevel_only)
  if (!random) {
    metrics <- setdiff(metrics, c(univariate, multilevel_only, "I2"))
  }
  if (int_only) {
    metrics <- setdiff(metrics, c("QM", "p_QM", "R2"))
  }

  num <- function(v) align_chr(v, accuracy = the$accuracy)
  p_format <- function(v) align_chr(apa_p(v), center = ".")
  ci <- NULL
  ci_mv <- NULL
  if (is_mv && "sigma2_CI" %in% metrics) {
    ci_mv <- tryCatch(
      suppressWarnings(stats::confint(fit)),
      error = function(e) NULL
    )
    # One component gives a single result rather than a list of them
    if (!is.null(ci_mv) && !is.null(ci_mv$random)) {
      ci_mv <- list(ci_mv)
    }
  }
  if (!is_mv && any(c("tau2_CI", "I2_CI") %in% metrics)) {
    ci <- tryCatch(
      suppressWarnings(stats::confint(fit)$random),
      error = function(e) NULL
    )
  }
  level <- meta_level(fit)
  ci_header <- paste0(round(level * 100), "% CI")
  qe_df <- fit$k - fit$p
  qe_sub <- if (int_only) "" else "~E~"

  d <- tibble::tibble(.rows = 1)
  for (m in metrics) {
    # One column for each level of a multilevel model
    if (m %in% multilevel_only) {
      for (i in seq_along(multilevel$labels)) {
        sub <- meta_subscript(multilevel$labels[i])
        switch(
          m,
          sigma2 = d[[paste0("*&sigma;*^2^", sub)]] <- num(multilevel$sigma2[i]),
          sigma = d[[paste0("*&sigma;*", sub)]] <- num(sqrt(multilevel$sigma2[i])),
          sigma2_CI = {
            bounds <- if (is.null(ci_mv) || length(ci_mv) < i) NULL else ci_mv[[i]]$random[1, c("ci.lb", "ci.ub")]
            d[[paste0(ci_header, " for *&sigma;*^2^", sub)]] <- if (is.null(bounds)) "" else ci_md(num(bounds[[1]]), num(bounds[[2]]))
          },
          I2_levels = d[[paste0("*I*^2^", sub, " (%)")]] <- align_chr(multilevel$I2_levels[i] %||% NA_real_, accuracy = 0.1)
        )
      }
      next
    }
    switch(
      m,
      k = d[["*k*"]] <- as.character(fit$k),
      QE = d[[paste0("*Q*", qe_sub, "(", qe_df, ")")]] <- num(fit$QE),
      p_QE = d[[paste0("*p*", qe_sub)]] <- p_format(fit$QEp),
      tau2 = d[["*&tau;*^2^"]] <- num(fit$tau2),
      tau2_CI = d[[paste0(ci_header, " for *&tau;*^2^")]] <- if (is.null(ci)) "" else ci_md(num(ci["tau^2", "ci.lb"]), num(ci["tau^2", "ci.ub"])),
      tau = d[["*&tau;*"]] <- num(sqrt(fit$tau2)),
      I2 = d[["*I*^2^ (%)"]] <- align_chr(if (is_mv) multilevel$I2 %||% NA_real_ else fit$I2, accuracy = 0.1),
      I2_CI = d[[paste0(ci_header, " for *I*^2^")]] <- if (is.null(ci)) "" else ci_md(align_chr(ci["I^2(%)", "ci.lb"], accuracy = 0.1), align_chr(ci["I^2(%)", "ci.ub"], accuracy = 0.1)),
      H2 = d[["*H*^2^"]] <- num(fit$H2),
      QM = d[[paste0("*Q*~M~(", fit$QMdf[1], ")")]] <- num(fit$QM),
      p_QM = d[["*p*~M~"]] <- p_format(fit$QMp),
      R2 = d[["*R*^2^ (%)"]] <- align_chr(fit$R2 %||% NA_real_, accuracy = 0.1)
    )
  }
  attr(d, "apa_note") <- list(
    table = "meta_fit",
    metrics = metrics,
    method = fit$method,
    int_only = int_only,
    multilevel = !is.null(multilevel),
    ci = level
  )
  as_apa7_tbl(d)
}

#' Only univariate models are supported
#' @keywords internal
#' @noRd
meta_check_class <- function(fit, call = rlang::caller_env()) {
  if (inherits(fit, "rma.mv")) {
    if (isTRUE(fit$withG) || isTRUE(fit$withH)) {
      cli::cli_abort(c(
        "Random effects with an {.code ~ inner | outer} structure (e.g., {.code ~ outcome | study}) are not yet supported.",
        i = "apa7 supports {.fn metafor::rma.mv} models with random intercepts at one or more (nested) levels, such as {.code random = ~ 1 | study / effect}."
      ), call = call)
    }
    return(invisible(TRUE))
  }
  if (!inherits(fit, "rma.uni")) {
    cli::cli_abort(c(
      "apa7 supports meta-analyses fit with {.fn metafor::rma} and {.fn metafor::rma.mv}, not {.cls {class(fit)[1]}} models.",
      i = "Mantel-Haenszel and Peto models are not yet supported."
    ), call = call)
  }
}

#' Markdown subscript of a level label
#'
#' Subscripts cannot contain spaces, so spaces become non-breaking spaces
#' (e.g., "~School in District~").
#' @keywords internal
#' @noRd
meta_subscript <- function(label) {
  paste0("~", gsub(" ", "\u00a0", label, fixed = TRUE), "~")
}

#' Levels of a multilevel meta-analysis
#'
#' For `rma.mv` models, the label, variance, and number of levels of each
#' random intercept, and the multilevel I2 (total and for each level),
#' computed as in Cheung (2014) with the typical sampling variance
#' (k - p) / tr(P), where P = W - WX(X'WX)^-1 X'W.
#' @returns NULL for univariate models
#' @keywords internal
#' @noRd
meta_multilevel <- function(fit) {
  if (!inherits(fit, "rma.mv") || !isTRUE(fit$withS)) {
    return(NULL)
  }
  labels <- purrr::map_chr(fit$s.names, \(name) {
    parts <- variable_name_formatter(strsplit(name, "/", fixed = TRUE)[[1]])
    if (length(parts) == 1) {
      return(parts)
    }
    paste0(parts[length(parts)], " in ", paste(rev(parts[-length(parts)]), collapse = " in "))
  })
  sigma2 <- fit$sigma2
  I2 <- NULL
  I2_levels <- NULL
  typical <- tryCatch(
    {
      W <- solve(as.matrix(fit$V))
      X <- as.matrix(fit$X)
      P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
      (fit$k - fit$p) / sum(diag(P))
    },
    error = function(e) NULL
  )
  if (!is.null(typical)) {
    I2 <- 100 * sum(sigma2) / (sum(sigma2) + typical)
    I2_levels <- 100 * sigma2 / (sum(sigma2) + typical)
  }
  list(
    labels = unname(labels),
    sigma2 = sigma2,
    nlevels = fit$s.nlevels,
    I2 = I2,
    I2_levels = I2_levels
  )
}

#' Are the tests t tests (Knapp-Hartung or similar adjustments)?
#' @keywords internal
#' @noRd
meta_t_test <- function(fit) {
  !is.null(fit$test) && fit$test %in% c("t", "knha", "adhoc")
}

#' Confidence level of a metafor model
#' @keywords internal
#' @noRd
meta_level <- function(fit) {
  level <- fit$level %||% 0.95
  # metafor stores the level as alpha (e.g., .05)
  if (level < 0.5) 1 - level else level
}

#' Header, name, and transformation of an effect-size measure
#' @param measure the measure given to metafor::escalc() (e.g., "SMD")
#' @param transform whether to show estimates on their original scale
#' @keywords internal
#' @noRd
meta_measure <- function(measure, transform = TRUE) {
  measures <- list(
    SMD = list(symbol = "g", name = "Hedges' *g* values (standardized mean differences)"),
    SMDH = list(symbol = "g", name = "Hedges' *g* values (standardized mean differences, without assuming equal variances)"),
    SMD1 = list(symbol = "g", name = "standardized mean differences (standardized by the control group's *SD*)"),
    SMCC = list(symbol = "g", name = "standardized mean changes (change-score standardization)"),
    SMCR = list(symbol = "g", name = "standardized mean changes (raw-score standardization)"),
    MD = list(symbol = "MD", name = "mean differences"),
    MC = list(symbol = "MC", name = "mean changes"),
    COR = list(symbol = "r", name = "correlations", bounded = TRUE),
    UCOR = list(symbol = "r", name = "correlations (bias-corrected)", bounded = TRUE),
    ZCOR = list(symbol = "r", log_symbol = "*z*~*r*~", name = "correlations", transform = "tanh", bounded = TRUE),
    PCOR = list(symbol = "r", name = "partial correlations", bounded = TRUE),
    ZPCOR = list(symbol = "r", log_symbol = "*z*~*r*~", name = "partial correlations", transform = "tanh", bounded = TRUE),
    RR = list(symbol = "RR", name = "risk ratios", singular = "risk ratio", transform = "exp"),
    OR = list(symbol = "OR", name = "odds ratios", singular = "odds ratio", transform = "exp"),
    PETO = list(symbol = "OR", name = "Peto odds ratios", singular = "Peto odds ratio", transform = "exp"),
    IRR = list(symbol = "IRR", name = "incidence rate ratios", singular = "incidence rate ratio", transform = "exp"),
    ROM = list(symbol = "ROM", name = "ratios of means", singular = "ratio of means", transform = "exp"),
    RD = list(symbol = "RD", name = "risk differences"),
    IRD = list(symbol = "IRD", name = "incidence rate differences"),
    PR = list(symbol = "Proportion", name = "proportions", bounded = TRUE, plain = TRUE),
    PLN = list(symbol = "Proportion", log_symbol = "ln(Proportion)", name = "proportions", transform = "exp", bounded = TRUE, plain = TRUE),
    PLO = list(symbol = "Proportion", log_symbol = "logit(Proportion)", name = "proportions", transform = "plogis", bounded = TRUE, plain = TRUE)
  )
  info <- measures[[measure %||% "GEN"]]
  if (is.null(info)) {
    info <- list(symbol = "Estimate", name = NULL, plain = TRUE)
  }
  plain <- isTRUE(info$plain)
  info$header <- if (plain) info$symbol else paste0("*", info$symbol, "*")
  info$measure <- measure
  if (!is.null(info[["transform"]])) {
    if (isTRUE(transform)) {
      info$transformed <- TRUE
    } else {
      # Shown on the analysis scale (e.g., log risk ratios)
      info$transformed <- FALSE
      info$header <- info$log_symbol %||% paste0("ln(", info$header, ")")
      info$bounded <- FALSE
    }
  } else {
    info$transformed <- FALSE
  }
  info
}

#' @keywords internal
#' @noRd
meta_transform_function <- function(transform) {
  switch(
    transform,
    exp = exp,
    tanh = tanh,
    plogis = stats::plogis
  )
}

#' Label of the pooled estimate
#' @keywords internal
#' @noRd
meta_pooled_label <- function(fit) {
  switch(
    fit$method,
    EE = "Pooled estimate (equal effects)",
    CE = "Pooled estimate (common effect)",
    FE = "Pooled estimate (fixed effects)",
    "Pooled estimate (random effects)"
  )
}

#' Study rows and the pooled estimate
#' @keywords internal
#' @noRd
meta_study_table <- function(fit, info, weights = TRUE, column_formats = NULL) {
  level <- meta_level(fit)
  crit <- stats::qnorm(1 - (1 - level) / 2)
  keep <- fit$not.na %||% rep(TRUE, length(fit$yi.f))
  yi <- as.vector(fit$yi.f[keep])
  vi <- as.vector(fit$vi.f[keep])
  slab <- as.character(fit$slab[keep])
  est <- c(yi, as.vector(fit$b))
  lo <- c(yi - crit * sqrt(vi), fit$ci.lb)
  hi <- c(yi + crit * sqrt(vi), fit$ci.ub)
  if (isTRUE(info$transformed)) {
    f <- meta_transform_function(info[["transform"]])
    est <- f(est)
    lo <- f(lo)
    hi <- f(hi)
  }
  format_estimate <- function(v) {
    align_chr(
      v,
      accuracy = the$accuracy,
      trim_leading_zeros = isTRUE(info$bounded),
      format_integers = isTRUE(info$bounded)
    )
  }
  pooled_label <- meta_pooled_label(fit)
  d <- tibble::tibble(Study = c(slab, pooled_label))
  d[[info$header]] <- format_estimate(est)
  d[[paste0(round(level * 100), "% CI")]] <- ci_md(format_estimate(lo), format_estimate(hi))
  if (isTRUE(weights)) {
    w <- as.vector(stats::weights(fit))
    d[["Weight (%)"]] <- c(align_chr(w, accuracy = 0.1), "")
  }
  attr(d, "no_format_columns") <- "Study"
  attr(d, "rule_above") <- pooled_label
  attr(d, "apa_note") <- meta_record(fit, info, studies = TRUE, weights = weights)
  as_apa7_tbl(d)
}

#' What the note of a meta-analysis table needs
#' @keywords internal
#' @noRd
meta_record <- function(fit, info, studies, weights = FALSE) {
  pi <- NULL
  multilevel <- meta_multilevel(fit)
  random <- if (inherits(fit, "rma.mv")) !is.null(multilevel) else !(fit$method %in% c("EE", "FE", "CE"))
  if (studies && random) {
    pr <- tryCatch(stats::predict(fit), error = function(e) NULL)
    if (!is.null(pr) && !is.null(pr$pi.lb)) {
      pi <- c(pr$pi.lb[1], pr$pi.ub[1])
      if (isTRUE(info$transformed)) {
        pi <- meta_transform_function(info[["transform"]])(pi)
      }
    }
  }
  list(
    table = if (studies) "meta" else NULL,
    studies = studies,
    weights = weights,
    k = fit$k,
    measure = info$measure,
    measure_name = info$name,
    header = info$header,
    transform = info[["transform"]],
    transformed = isTRUE(info$transformed),
    bounded = isTRUE(info$bounded),
    method = fit$method,
    multilevel = if (!is.null(multilevel)) multilevel[c("labels", "nlevels")],
    t_test = meta_t_test(fit),
    int_only = isTRUE(fit$int.only),
    pooled = list(
      stat = fit$zval[1],
      df = if (meta_t_test(fit)) fit$ddf[1],
      p = fit$pval[1],
      pi = pi
    ),
    heterogeneity = list(
      QE = fit$QE,
      df = fit$k - fit$p,
      p = fit$QEp,
      tau2 = if (random && is.null(multilevel)) fit$tau2,
      sigma2 = if (!is.null(multilevel)) stats::setNames(multilevel$sigma2, multilevel$labels),
      I2 = if (!is.null(multilevel)) multilevel$I2 else if (random) fit$I2
    ),
    ci = meta_level(fit)
  )
}

#' Model description of a meta-analysis
#' @keywords internal
#' @noRd
meta_estimator <- function(method) {
  estimators <- c(
    REML = "restricted maximum likelihood",
    ML = "maximum likelihood",
    DL = "the DerSimonian\u2013Laird estimator",
    HE = "the Hedges estimator",
    HS = "the Hunter\u2013Schmidt estimator",
    HSk = "the Hunter\u2013Schmidt estimator (with small-sample correction)",
    SJ = "the Sidik\u2013Jonkman estimator",
    PM = "the Paule\u2013Mandel estimator",
    MP = "the Paule\u2013Mandel estimator",
    EB = "the empirical Bayes estimator",
    GENQ = "the generalized *Q*-statistic estimator",
    PMM = "the median-unbiased Paule\u2013Mandel estimator",
    GENQM = "the median-unbiased generalized *Q*-statistic estimator"
  )
  if (method %in% names(estimators)) estimators[[method]] else method
}

#' @keywords internal
#' @noRd
meta_model_sentence <- function(record) {
  method <- record$method
  ml <- record$multilevel
  model <- if (length(ml) > 0) {
    levels <- paste0(
      # Lowercase words in running text (e.g., "school in district")
      gsub("\\b([A-Z])(?=[a-z])", "\\L\\1", ml$labels, perl = TRUE),
      " (", ml$nlevels, " levels)"
    )
    paste0(
      "a multilevel random-effects model with random intercepts for ",
      and_list(levels),
      ", with variance components estimated by ",
      meta_estimator(method)
    )
  } else if (method %in% c("EE", "CE")) {
    "an equal-effects model"
  } else if (method == "FE") {
    "a fixed-effects model"
  } else {
    paste0(
      "a random-effects model, with *&tau;*^2^ estimated by ",
      meta_estimator(method)
    )
  }
  knha <- if (isTRUE(record$t_test)) {
    ", and tests and confidence intervals use the Knapp\u2013Hartung adjustment"
  } else {
    ""
  }
  units <- if (length(ml) > 0) "effect sizes" else "studies"
  if (isTRUE(record$int_only)) {
    paste0("The ", record$k, " ", units, " were pooled with ", model, knha, ".")
  } else {
    paste0(
      "The meta-regression of ", record$k, " ", units, " used ",
      sub("^an? ", "a ", sub("random-effects model", "mixed-effects model", model)),
      knha,
      "."
    )
  }
}

#' Sentence naming the effect-size measure
#' @keywords internal
#' @noRd
meta_measure_sentence <- function(record) {
  if (is.null(record$measure_name)) {
    return(character(0))
  }
  name <- record$measure_name
  scales <- c(exp = "log", tanh = "Fisher's *z*", plogis = "logit")
  if (is.null(record[["transform"]])) {
    return(paste0("Effect sizes are ", name, "."))
  }
  scale <- scales[[record[["transform"]]]]
  # Meta-regression coefficients stay on the analysis scale
  if (!isTRUE(record$int_only)) {
    return(paste0(
      "Effect sizes are ", name, ", analyzed on the ", scale,
      " scale, and coefficients are on that scale."
    ))
  }
  if (isTRUE(record$transformed)) {
    how <- if (record[["transform"]] == "exp") "exponentiated" else "converted back"
    paste0(
      "Effect sizes are ", name, ", analyzed on the ", scale,
      " scale and ", how, " for the table."
    )
  } else {
    paste0("Effect sizes are ", name, " on the ", scale, " scale.")
  }
}

#' Note sentences of meta-analysis study tables (and meta-regressions)
#' @keywords internal
#' @noRd
meta_note_sentences <- function(record) {
  if (length(record) == 0) {
    return(character(0))
  }
  sentences <- c(meta_measure_sentence(record), meta_model_sentence(record))
  if (isTRUE(record$studies)) {
    if (isTRUE(record$weights)) {
      sentences <- c(sentences, "Weights are percentages of the total weight.")
    }
    pooled <- record$pooled
    stat <- if (is.null(pooled$df)) {
      "*z*"
    } else {
      paste0("*t*(", align_chr(pooled$df, accuracy = the$accuracy, drop0trailing = TRUE), ")")
    }
    estimate <- function(v) {
      align_chr(
        v,
        accuracy = the$accuracy,
        trim_leading_zeros = isTRUE(record$bounded),
        format_integers = isTRUE(record$bounded)
      )
    }
    pooled_sentence <- paste0(
      "Test of the pooled estimate: ",
      stat, " = ", trimws(align_chr(pooled$stat, accuracy = the$accuracy)),
      ", ", meta_p_text(pooled$p),
      if (!is.null(pooled$pi)) {
        paste0(
          "; ", round(record$ci * 100), "% PI ",
          gsub("[\u2002-\u200a ]+", " ", trimws(ci_md(trimws(estimate(pooled$pi[1])), trimws(estimate(pooled$pi[2])))))
        )
      } else {
        ""
      },
      "."
    )
    het <- record$heterogeneity
    het_sentence <- paste0(
      "Heterogeneity: *Q*(", het$df, ") = ",
      trimws(align_chr(het$QE, accuracy = the$accuracy)),
      ", ", meta_p_text(het$p),
      if (!is.null(het$tau2)) {
        paste0(
          ", *&tau;*^2^ = ", trimws(align_chr(het$tau2, accuracy = the$accuracy)),
          ", *I*^2^ = ", trimws(align_chr(het$I2, accuracy = 0.1)), "%"
        )
      } else if (!is.null(het$sigma2)) {
        paste0(
          paste0(
            ", *&sigma;*^2^", meta_subscript(names(het$sigma2)), " = ",
            trimws(align_chr(unname(het$sigma2), accuracy = the$accuracy)),
            collapse = ""
          ),
          if (!is.null(het$I2)) paste0(", *I*^2^ = ", trimws(align_chr(het$I2, accuracy = 0.1)), "%") else ""
        )
      } else {
        ""
      },
      "."
    )
    sentences <- c(sentences, pooled_sentence, het_sentence)
    abbreviations <- c(CI = "confidence interval")
    if (!is.null(pooled$pi)) {
      abbreviations <- c(abbreviations, PI = "prediction interval")
    }
    sentences <- c(sentences, abbreviation_sentence(abbreviations))
  }
  sentences
}

#' p in note text (e.g., "*p* = .02" or "*p* < .001")
#' @keywords internal
#' @noRd
meta_p_text <- function(p) {
  text <- trimws(apa_p(p))
  if (startsWith(text, "<") || startsWith(text, ">")) {
    paste0("*p* ", substr(text, 1, 1), " ", trimws(substring(text, 2)))
  } else {
    paste0("*p* = ", text)
  }
}

#' Note sentences of meta-analysis heterogeneity tables
#' @keywords internal
#' @noRd
meta_fit_note_sentences <- function(record) {
  metrics <- record$metrics
  sentences <- character(0)
  if (isTRUE(record$multilevel) && any(c("sigma2", "sigma", "sigma2_CI", "I2", "I2_levels") %in% metrics)) {
    sentences <- c(
      sentences,
      paste0("Variance components were estimated by ", meta_estimator(record$method), "."),
      if (any(c("I2", "I2_levels") %in% metrics)) {
        "*I*^2^ is the multilevel *I*^2^ (Cheung, 2014): the percentage of total variability due to heterogeneity at all levels, or at the level in the subscript."
      }
    )
  } else if (!(record$method %in% c("EE", "FE", "CE")) && any(c("tau2", "tau", "tau2_CI", "I2", "R2") %in% metrics)) {
    sentences <- c(
      sentences,
      paste0("*&tau;*^2^ was estimated by ", meta_estimator(record$method), ".")
    )
  }
  abbreviations <- c(
    `*k*` = if ("k" %in% metrics) {
      if (isTRUE(record$multilevel)) "number of effect sizes" else "number of studies"
    },
    `*Q*` = if ("QE" %in% metrics && isTRUE(record$int_only)) "test of heterogeneity",
    `*Q*~E~` = if ("QE" %in% metrics && !isTRUE(record$int_only)) "test of residual heterogeneity",
    `*&tau;*^2^` = if (any(c("tau2", "tau2_CI") %in% metrics)) "variance of the true effects",
    `*&tau;*` = if ("tau" %in% metrics) "standard deviation of the true effects",
    `*&sigma;*^2^` = if (any(c("sigma2", "sigma2_CI") %in% metrics)) "variance of the true effects at the level in the subscript",
    `*&sigma;*` = if ("sigma" %in% metrics) "standard deviation of the true effects at the level in the subscript",
    `*I*^2^` = if (any(c("I2", "I2_CI") %in% metrics) && !isTRUE(record$multilevel)) "percentage of total variability due to heterogeneity",
    `*H*^2^` = if ("H2" %in% metrics) "ratio of total variability to sampling variability",
    `*Q*~M~` = if ("QM" %in% metrics) "omnibus test of the moderators",
    `*R*^2^` = if ("R2" %in% metrics) "percentage of heterogeneity explained by the moderators",
    CI = if (any(c("tau2_CI", "I2_CI", "sigma2_CI") %in% metrics)) "confidence interval"
  )
  c(sentences, abbreviation_sentence(unlist(abbreviations)))
}
