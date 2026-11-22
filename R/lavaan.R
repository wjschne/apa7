#' Fit indices of lavaan models
#'
#' @details
#' For structural equation models and confirmatory factor analyses fit with
#' `lavaan` (e.g., `lavaan::cfa()` or `lavaan::sem()`), the default metrics
#' are the model \eqn{\chi^2}{chi^2} test (`"Chi2"`, `"Chi2_df"`, `"p_Chi2"`), the
#' comparative fit index (`"CFI"`), the Tucker-Lewis index (`"TLI"`), the root
#' mean square error of approximation with its confidence interval
#' (`"RMSEA"`, `"RMSEA_CI"`), and the standardized root mean square residual
#' (`"SRMR"`). `"AIC"` and `"BIC"` are also available, and `"all"` selects
#' every metric. Values come from [lavaan::fitMeasures()]. When the model was
#' fit with a robust estimator (e.g., `estimator = "MLR"` or `"WLSMV"`), the
#' scaled \eqn{\chi^2}{chi^2} test and the robust (or, if unavailable, scaled) CFI,
#' TLI, and RMSEA are displayed. [apa_note()] gives the sample size, the
#' estimator, and how missing data were handled, and defines the
#' abbreviations.
#' @export
#' @rdname apa_performance
#' @examples
#' \donttest{
#' if (requireNamespace("lavaan", quietly = TRUE)) {
#'   model <- "visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6
#'             speed =~ x7 + x8 + x9"
#'   fit_cfa <- lavaan::cfa(model, data = lavaan::HolzingerSwineford1939)
#'   apa_performance(fit_cfa)
#' }
#' }
apa_performance.lavaan <- function(
  fit,
  metrics = c("Chi2", "Chi2_df", "p_Chi2", "CFI", "TLI", "RMSEA", "RMSEA_CI", "SRMR"),
  column_formats = NULL
) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("lavaan models require the lavaan package. Install it with install.packages(\"lavaan\").") # nocov
  }
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }
  names(metrics) <- NULL
  if (any(metrics == "all")) {
    metrics <- lavaan_fit_metrics
  }
  unknown <- setdiff(metrics, lavaan_fit_metrics)
  if (length(unknown) > 0) {
    stop(
      "Unknown fit metrics for lavaan models: ",
      paste(unknown, collapse = ", "),
      ". Choose from ",
      paste(lavaan_fit_metrics, collapse = ", "),
      "."
    )
  }

  info <- lavaan_fit_info(fit)
  d <- info$values

  # Format each metric
  out <- list()
  for (m in metrics) {
    if (m == "RMSEA_CI") {
      low <- column_formats$RMSEA@formatter(d$RMSEA_CI_low, format_integers = TRUE)
      high <- column_formats$RMSEA@formatter(d$RMSEA_CI_high, format_integers = TRUE)
      out[[m]] <- ci_md(low, high, missing = is.na(d$RMSEA_CI_low))
    } else if (!is.null(column_formats[[m]])) {
      out[[m]] <- column_formats[[m]]@formatter(d[[m]])
    } else {
      out[[m]] <- the$number_formatter(d[[m]]) # nocov
    }
  }

  # Headers
  ci_level <- round(info$rmsea_ci_level * 100)
  cf <- column_formats
  cf$RMSEA_CI <- column_format(
    name = "RMSEA_CI",
    header = paste0(ci_level, "% CI"),
    latex = paste0(ci_level, "\\% CI"),
    formatter = \(x, ...) x,
    abbreviation = c(CI = "confidence interval for RMSEA")
  )
  headers <- purrr::map_chr(metrics, \(m) {
    if (is.null(cf[[m]])) m else cf[[m]]@header
  })
  d_new <- tibble::as_tibble(stats::setNames(out, headers))

  attr(d_new, "apa_note") <- fit_note_record(
    n = info$n,
    estimator = info$estimator,
    scaled = info$scaled,
    robust_indices = if (info$scaled) intersect(c("CFI", "TLI", "RMSEA"), metrics),
    robust_kind = info$robust_kind,
    missing = info$missing,
    abbreviations = column_abbreviations(metrics, cf)
  )
  as_apa7_tbl(d_new)
}

#' Fit metrics available for lavaan models
#' @keywords internal
#' @noRd
lavaan_fit_metrics <- c(
  "Chi2", "Chi2_df", "p_Chi2", "CFI", "TLI", "RMSEA", "RMSEA_CI", "SRMR",
  "AIC", "BIC"
)

#' Fit measures, estimator, and missing-data method of a lavaan model
#'
#' Scaled tests and robust (or scaled) indices are used when the model has a
#' scaled test statistic.
#' @keywords internal
#' @noRd
lavaan_fit_info <- function(fit) {
  fm <- lavaan::fitMeasures(fit)
  get <- function(name) if (name %in% names(fm)) unname(fm[[name]]) else NA_real_
  # Robust versions where available, otherwise scaled versions
  pick <- function(name) {
    robust <- get(paste0(name, ".robust"))
    if (!is.na(robust)) {
      return(list(value = robust, kind = "robust"))
    }
    scaled <- get(paste0(name, ".scaled"))
    if (!is.na(scaled)) {
      return(list(value = scaled, kind = "scaled"))
    }
    list(value = get(name), kind = NA_character_)
  }

  options <- lavaan::lavInspect(fit, "options")
  tests <- setdiff(
    options$test,
    c("standard", "none", "browne.residual.nt", "browne.residual.adf",
      "browne.residual.nt.model", "browne.residual.adf.model")
  )
  scaled <- length(tests) > 0 && !is.na(get("chisq.scaled"))

  if (scaled) {
    cfi <- pick("cfi")
    tli <- pick("tli")
    rmsea <- pick("rmsea")
    suffix <- if (identical(rmsea$kind, "robust")) ".robust" else ".scaled"
    values <- tibble::tibble(
      Chi2 = get("chisq.scaled"),
      Chi2_df = get("df.scaled"),
      p_Chi2 = get("pvalue.scaled"),
      CFI = cfi$value,
      TLI = tli$value,
      RMSEA = rmsea$value,
      RMSEA_CI_low = get(paste0("rmsea.ci.lower", suffix)),
      RMSEA_CI_high = get(paste0("rmsea.ci.upper", suffix))
    )
    robust_kind <- if (identical(rmsea$kind, "robust")) "robust" else "scaled"
  } else {
    values <- tibble::tibble(
      Chi2 = get("chisq"),
      Chi2_df = get("df"),
      p_Chi2 = get("pvalue"),
      CFI = get("cfi"),
      TLI = get("tli"),
      RMSEA = get("rmsea"),
      RMSEA_CI_low = get("rmsea.ci.lower"),
      RMSEA_CI_high = get("rmsea.ci.upper")
    )
    robust_kind <- NA_character_
  }
  values$SRMR <- get("srmr")
  values$AIC <- get("aic")
  values$BIC <- get("bic")

  ci_level <- get("rmsea.ci.level")
  list(
    values = values,
    n = sum(lavaan::lavInspect(fit, "nobs")),
    estimator = lavaan_estimator_name(options, tests),
    scaled = scaled,
    robust_kind = robust_kind,
    missing = options$missing,
    rmsea_ci_level = if (is.na(ci_level)) 0.90 else ci_level
  )
}

#' Describe a lavaan estimator in words (e.g., "robust maximum likelihood
#' (MLR)")
#' @param options lavInspect(fit, "options")
#' @param tests scaled test statistics in use
#' @keywords internal
#' @noRd
lavaan_estimator_name <- function(options, tests) {
  est <- toupper(options$estimator)
  se <- options$se
  if (est == "ML") {
    if ("yuan.bentler.mplus" %in% tests || "yuan.bentler" %in% tests) {
      return("robust maximum likelihood (MLR)")
    }
    if ("satorra.bentler" %in% tests) {
      return(paste0(
        "maximum likelihood with a Satorra", intToUtf8(8211),
        "Bentler scaled test statistic (MLM)"
      ))
    }
    if (any(c("scaled.shifted", "mean.var.adjusted") %in% tests)) {
      return("maximum likelihood with a mean- and variance-adjusted test statistic (MLMV)") # nocov
    }
    if (identical(se, "robust.huber.white")) {
      return("maximum likelihood with robust standard errors") # nocov
    }
    return("maximum likelihood (ML)")
  }
  if (est %in% c("DWLS", "ULS")) {
    base <- if (est == "DWLS") "diagonally weighted least squares" else "unweighted least squares"
    if (any(c("scaled.shifted", "mean.var.adjusted") %in% tests)) {
      return(paste0(
        base,
        " with a mean- and variance-adjusted test statistic (",
        if (est == "DWLS") "WLSMV" else "ULSMV",
        ")"
      ))
    }
    if ("satorra.bentler" %in% tests) {
      return(paste0(base, " with a mean-adjusted test statistic (", if (est == "DWLS") "WLSM" else "ULSM", ")"))
    }
    return(paste0(base, " (", est, ")"))
  }
  names <- c(
    WLS = "weighted least squares (WLS)",
    GLS = "generalized least squares (GLS)",
    MML = "marginal maximum likelihood (MML)",
    PML = "pairwise maximum likelihood (PML)"
  )
  if (est %in% names(names)) names[[est]] else est
}

#' Record what a fit-index table's note needs
#' @keywords internal
#' @noRd
fit_note_record <- function(
  n = NA,
  estimator = NULL,
  scaled = FALSE,
  robust_indices = character(0),
  robust_kind = NA,
  missing = NULL,
  abbreviations = character(0)
) {
  list(
    table = "fit",
    n = n,
    estimator = estimator,
    scaled = scaled,
    robust_indices = robust_indices,
    robust_kind = robust_kind,
    missing = missing,
    abbreviations = abbreviations
  )
}

#' Combine the fit records of a list of models
#' @keywords internal
#' @noRd
merge_fit_note_records <- function(records) {
  pick <- function(field) unique(unlist(purrr::map(records, field)))
  list(
    table = "fit",
    n = pick("n"),
    estimator = pick("estimator"),
    scaled = any(purrr::map_lgl(records, \(r) isTRUE(r$scaled))),
    robust_indices = pick("robust_indices"),
    robust_kind = pick("robust_kind"),
    missing = pick("missing"),
    abbreviations = combine_abbreviations(unlist(unname(purrr::map(records, "abbreviations"))))
  )
}

#' @keywords internal
#' @noRd
fit_note_sentences <- function(record) {
  sentences <- character(0)

  # Sample size (only when all models share it)
  n <- record$n[!is.na(record$n)]
  if (length(n) == 1) {
    sentences <- c(sentences, paste0("*N* = ", format(n, big.mark = ","), "."))
  }

  # Estimator
  if (length(record$estimator) > 0) {
    sentences <- c(
      sentences,
      paste0("Estimation used ", or_list(record$estimator), ".")
    )
  }

  # Scaled test and robust indices
  if (isTRUE(record$scaled)) {
    kind <- record$robust_kind[!is.na(record$robust_kind)]
    indices <- record$robust_indices
    sentence <- paste0("*&chi;*^2^ is a scaled test statistic")
    if (length(indices) > 0 && length(kind) > 0) {
      sentence <- paste0(
        sentence,
        ", and ",
        and_list(indices),
        if (length(indices) == 1) " is the " else " are the ",
        or_list(kind),
        if (length(indices) == 1) " version" else " versions"
      )
    }
    sentences <- c(sentences, paste0(sentence, "."))
  }

  # Missing data
  if (any(record$missing %in% c("ml", "ml.x", "fiml", "direct"))) {
    sentences <- c(
      sentences,
      "Missing data were handled with full information maximum likelihood."
    )
  }

  sentences <- c(sentences, abbreviation_sentence(record$abbreviations))

  # Comparisons of nested models
  if (isTRUE(record$change)) {
    sentences <- c(sentences, "&Delta; = change from the previous model.")
  }
  if (length(record$difference_test) > 0) {
    sentences <- c(
      sentences,
      paste0(
        "&Delta;*&chi;*^2^ is ",
        or_list(lavaan_lrt_description(record$difference_test)),
        "."
      )
    )
  }
  sentences
}

#' "A", "A and B", "A, B, and C"
#' @keywords internal
#' @noRd
and_list <- function(x) {
  n <- length(x)
  if (n <= 1) {
    return(paste(x, collapse = ""))
  }
  if (n == 2) {
    return(paste(x, collapse = " and "))
  }
  paste0(paste(x[-n], collapse = ", "), ", and ", x[n])
}

#' @param standardize for `lavaan` models, if `TRUE` (default), fully standardized estimates (\eqn{\beta}{beta}) are displayed with their standard errors and *p*-values. If `FALSE`, unstandardized estimates (*B*) are displayed.
#' @param variances,intercepts for `lavaan` models, if `TRUE`, sections for (residual) variances or intercepts are added. Both are `FALSE` by default.
#' @param ci for `lavaan` models, confidence level of intervals shown when `predictor_parameters` includes `"CI"`
#' @details
#' For structural equation models and confirmatory factor analyses fit with `lavaan`, estimates come from [lavaan::standardizedSolution()] (`standardize = TRUE`, default) or [lavaan::parameterEstimates()] (`standardize = FALSE`). Each kind of parameter is introduced by a heading row: "Factor loadings" (e.g., Visual \eqn{\rightarrow}{->} x1), "Regressions" (e.g., Speed \eqn{\leftarrow}{<-} Visual), "Correlations" (or "Covariances", if unstandardized; e.g., Visual \eqn{\leftrightarrow}{<->} Textual), and "Defined parameters" (e.g., indirect effects defined with `:=`), followed, if requested, by variances, intercepts, and thresholds. The default columns are the estimate, its standard error, and its *p*-value; `"z"`, `"CI"`, and (with `standardize = TRUE`) `"Coefficient"` for unstandardized estimates can be added to `predictor_parameters`. Unstandardized parameters fixed to set the scale of a factor (e.g., marker loadings fixed at 1) are not tested. Variable names are shown as written in the model, with the first letter capitalized and underscores replaced by spaces. Multiple-group models are not yet supported.
#' @export
#' @rdname apa_parameters
#' @examples
#' \donttest{
#' if (requireNamespace("lavaan", quietly = TRUE)) {
#'   model <- "visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6
#'             speed =~ x7 + x8 + x9
#'             speed ~ visual + textual"
#'   lavaan::sem(model, data = lavaan::HolzingerSwineford1939) |>
#'     apa_parameters()
#' }
#' }
apa_parameters.lavaan <- function(
  fit,
  predictor_parameters = NULL,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  standardize = TRUE,
  variances = FALSE,
  intercepts = FALSE,
  thresholds = FALSE,
  ci = 0.95,
  ...
) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("lavaan models require the lavaan package. Install it with install.packages(\"lavaan\").") # nocov
  }
  if (lavaan::lavInspect(fit, "ngroups") > 1) {
    stop("apa_parameters() does not yet support multiple-group lavaan models.")
  }
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }
  if (is.null(predictor_parameters)) {
    predictor_parameters <- if (standardize) {
      c("Parameter", "Std_Coefficient", "SE", "p")
    } else {
      c("Parameter", "Coefficient", "SE", "p")
    }
  }
  names(predictor_parameters) <- NULL

  key <- c("lhs", "op", "rhs")
  pe <- lavaan::parameterEstimates(fit, level = ci)
  ss <- lavaan::standardizedSolution(fit, level = ci)
  # Tests, standard errors, and intervals come from the solution displayed
  if (standardize) {
    src <- dplyr::left_join(
      pe[c(key, "est")],
      ss[c(key, "est.std", "se", "z", "pvalue", "ci.lower", "ci.upper")],
      by = key
    )
  } else {
    src <- dplyr::left_join(pe, ss[c(key, "est.std")], by = key)
  }
  d <- tibble::tibble(
    lhs = src$lhs,
    op = src$op,
    rhs = src$rhs,
    Coefficient = src$est,
    Std_Coefficient = src$est.std,
    SE = src$se,
    z = src$z,
    p = src$pvalue,
    CI_low = src$ci.lower,
    CI_high = src$ci.upper
  )

  # Parameters fixed to set the scale (e.g., marker loadings fixed at 1, or
  # standardized variances of 1) are not tested
  pt <- lavaan::parTable(fit)
  free <- pt$free[match(paste(d$lhs, d$op, d$rhs), paste(pt$lhs, pt$op, pt$rhs))]
  fixed <- !is.na(free) & free == 0 & d$op != ":="
  untested <- (fixed & !standardize) | (!is.na(d$SE) & d$SE == 0)
  for (col in c("SE", "z", "p", "CI_low", "CI_high")) {
    d[[col]][untested] <- NA
  }
  if ("CI" %in% predictor_parameters) {
    d$CI <- ci
  }

  name <- lavaan_label
  sections <- list(
    loadings = d$op == "=~",
    regressions = d$op == "~",
    correlations = d$op == "~~" & d$lhs != d$rhs,
    defined = d$op == ":=",
    variances = d$op == "~~" & d$lhs == d$rhs & variances,
    intercepts = d$op == "~1" & intercepts,
    thresholds = d$op == "|" & thresholds
  )
  headings <- c(
    loadings = "Factor loadings",
    regressions = "Regressions",
    correlations = if (standardize) "Correlations" else "Covariances",
    defined = "Defined parameters",
    variances = "Variances",
    intercepts = "Intercepts",
    thresholds = "Thresholds"
  )
  # Variances of indicators and outcomes are residual variances
  endogenous <- unique(c(d$rhs[d$op == "=~"], d$lhs[d$op == "~"]))

  pieces <- list()
  for (section in names(sections)) {
    rows <- d[sections[[section]], ]
    if (nrow(rows) == 0) {
      next
    }
    rows$Parameter <- switch(
      section,
      loadings = paste0(name(rows$lhs), " \u2192 ", name(rows$rhs)),
      regressions = paste0(name(rows$lhs), " \u2190 ", name(rows$rhs)),
      correlations = paste0(name(rows$lhs), " \u2194 ", name(rows$rhs)),
      defined = name(rows$lhs),
      variances = paste0(
        name(rows$lhs),
        ifelse(rows$lhs %in% endogenous, " (residual)", "")
      ),
      intercepts = name(rows$lhs),
      thresholds = paste0(name(rows$lhs), " | ", rows$rhs)
    )
    rows$Parameter <- paste0("\u2003", rows$Parameter)
    pieces[[section]] <- dplyr::bind_rows(
      tibble::tibble(Parameter = headings[[section]]),
      rows
    )
  }
  d_b <- dplyr::bind_rows(pieces)
  d_b <- d_b[setdiff(colnames(d_b), key)]

  # Labels were made above
  column_formats$Parameter@formatter <- \(x, ...) x
  # Standardized intervals, like the estimates, have no leading zero
  if (standardize) {
    column_formats <- es_ci_format(column_formats)
  }
  if (column_formats$Parameter@header == "Variable") {
    column_formats$Parameter@header <- "Parameter"
    column_formats$Parameter@latex <- "Parameter"
  }

  options <- lavaan::lavInspect(fit, "options")
  tests <- setdiff(
    options$test,
    c("standard", "none", "browne.residual.nt", "browne.residual.adf",
      "browne.residual.nt.model", "browne.residual.adf.model")
  )
  fixed_loadings <- d[fixed & sections$loadings, ]

  d_new <- format_apa_parameters(
    d_b,
    predictor_parameters = predictor_parameters,
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    t_with_df = FALSE
  )
  attr(d_new, "apa_note") <- sem_note_record(
    standardize = standardize,
    n = sum(lavaan::lavInspect(fit, "nobs")),
    estimator = lavaan_estimator_name(options, tests),
    se = options$se,
    bootstrap = if (identical(options$se, "bootstrap")) lavaan_boot_count(fit, options),
    percentile_ci = identical(options$se, "bootstrap") && !standardize &&
      "CI" %in% predictor_parameters,
    missing = options$missing,
    fixed = if (!standardize && nrow(fixed_loadings) > 0) {
      paste0(name(fixed_loadings$lhs), " \u2192 ", name(fixed_loadings$rhs))
    },
    abbreviations = attr(d_new, "apa_note", exact = TRUE)$abbreviations,
    starred = anova_marked(starred, predictor_parameters),
    bolded = anova_marked(bolded, predictor_parameters)
  )
  as_apa7_tbl(labels_formatted(d_new))
}

#' Number of bootstrap draws lavaan used (failed draws are dropped)
#' @keywords internal
#' @noRd
lavaan_boot_count <- function(fit, options) {
  draws <- tryCatch(lavaan::lavInspect(fit, "boot"), error = function(e) NULL)
  if (is.null(draws) || length(dim(draws)) != 2) {
    return(options$bootstrap) # nocov
  }
  sum(stats::complete.cases(draws))
}

#' Light formatting of lavaan variable names: underscores to spaces, first
#' letter capitalized (e.g., "visual" to "Visual", "x1" to "X1")
#' @keywords internal
#' @noRd
lavaan_label <- function(x) {
  x <- gsub("_", " ", x, fixed = TRUE)
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

#' Record what a lavaan parameter table's note needs
#' @keywords internal
#' @noRd
sem_note_record <- function(
  standardize = TRUE,
  n = NA,
  estimator = NULL,
  se = NULL,
  bootstrap = NULL,
  missing = NULL,
  fixed = NULL,
  percentile_ci = FALSE,
  abbreviations = character(0),
  starred = FALSE,
  bolded = FALSE
) {
  list(
    table = "sem",
    percentile_ci = percentile_ci,
    standardize = standardize,
    n = n,
    estimator = estimator,
    se = se,
    bootstrap = bootstrap,
    missing = missing,
    fixed = fixed,
    abbreviations = abbreviations,
    starred = starred,
    bolded = bolded
  )
}

#' @keywords internal
#' @noRd
sem_note_sentences <- function(record) {
  sentences <- character(0)
  if (!is.na(record$n)) {
    sentences <- c(sentences, paste0("*N* = ", format(record$n, big.mark = ","), "."))
  }
  if (length(record$estimator) > 0) {
    sentences <- c(sentences, paste0("Estimation used ", record$estimator, "."))
  }
  if (isTRUE(record$standardize)) {
    sentences <- c(
      sentences,
      "Estimates are fully standardized, and their standard errors and *p*-values are for the standardized estimates."
    )
  }
  if (identical(record$se, "bootstrap")) {
    sentences <- c(
      sentences,
      paste0(
        "Standard errors are based on ",
        format(record$bootstrap, big.mark = ","),
        " bootstrap samples",
        if (isTRUE(record$percentile_ci)) ", and confidence intervals are bootstrap percentile intervals",
        "."
      )
    )
  } else if (isTRUE(record$se %in% c("robust.huber.white", "robust.sem", "robust.mlr"))) {
    sentences <- c(sentences, "Standard errors are robust.")
  }
  if (any(record$missing %in% c("ml", "ml.x", "fiml", "direct"))) {
    sentences <- c(
      sentences,
      "Missing data were handled with full information maximum likelihood."
    )
  }
  if (length(record$fixed) > 0) {
    sentences <- c(
      sentences,
      paste0(
        "To set the scale of ",
        if (length(record$fixed) == 1) "the factor, the loading " else "each factor, the loadings ",
        and_list(record$fixed),
        if (length(record$fixed) == 1) " was" else " were",
        " fixed at 1."
      )
    )
  }
  sentences <- c(sentences, abbreviation_sentence(record$abbreviations))
  c(sentences, probability_note_sentences(record$starred, record$bolded))
}

#' What apa_loadings() and apa_factor_cor() need from a lavaan model
#'
#' Loadings, residual variances, and latent correlations come from the fully
#' standardized solution. Loadings that are fixed at 0 (not part of the model)
#' are marked in `estimated` so that they are left blank.
#' @keywords internal
#' @noRd
lavaan_fa_parts <- function(fit) {
  if (lavaan::lavInspect(fit, "ngroups") > 1) {
    cli::cli_abort(
      "{.fn apa_loadings} and {.fn apa_factor_cor} do not yet support multiple-group lavaan models."
    )
  }
  std <- lavaan::lavInspect(fit, "std")
  if (is.null(std$lambda) || ncol(std$lambda) == 0) {
    cli::cli_abort("{.arg fit} has no latent variables with indicators.")
  }
  est <- lavaan::lavInspect(fit, "est")$lambda
  free <- lavaan::lavInspect(fit, "free")$lambda
  L <- unclass(std$lambda)
  estimated <- unclass(free) != 0 | unclass(est) != 0
  # Higher-order factors have no indicators in lambda (their loadings are
  # regressions among latent variables), so they have no column here
  keep_factors <- colSums(estimated) > 0
  keep_items <- rowSums(estimated) > 0
  L <- L[keep_items, keep_factors, drop = FALSE]
  estimated <- estimated[keep_items, keep_factors, drop = FALSE]
  attr(L, "class") <- NULL

  uniqueness <- diag(unclass(std$theta))[rownames(L)]
  Phi <- NULL
  if (ncol(L) > 1) {
    Phi <- unclass(lavaan::lavInspect(fit, "cor.lv"))[colnames(L), colnames(L), drop = FALSE]
    attr(Phi, "class") <- NULL
  }
  beta <- std$beta
  structural <- !is.null(beta) && any(unclass(beta) != 0)

  ss <- colSums(L^2)
  options <- lavaan::lavInspect(fit, "options")
  tests <- setdiff(
    options$test,
    c("standard", "none", "browne.residual.nt", "browne.residual.adf",
      "browne.residual.nt.model", "browne.residual.adf.model")
  )
  list(
    loadings = L,
    estimated = estimated,
    uniqueness = uniqueness,
    ss = ss,
    prop = ss / nrow(L),
    Phi = Phi,
    rotation = "none",
    method = "lavaan",
    n = sum(lavaan::lavInspect(fit, "nobs")),
    estimator = lavaan_estimator_name(options, tests),
    missing = options$missing,
    structural = structural
  )
}

#' Change metrics available for lists of lavaan models
#' @keywords internal
#' @noRd
lavaan_change_metrics <- c(
  "deltaChi2", "deltaChi2_df", "p_deltaChi2",
  "deltaCFI", "deltaTLI", "deltaRMSEA", "deltaSRMR"
)

#' Compare nested lavaan models, each with the one before it
#'
#' @param models named list of lavaan models, from least to most constrained
#' @keywords internal
#' @noRd
lavaan_performance_comparison <- function(models, metrics = NULL, column_formats = NULL) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("lavaan models require the lavaan package. Install it with install.packages(\"lavaan\").") # nocov
  }
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }
  if (length(models) < 2) {
    cli::cli_abort("Comparing lavaan models needs at least two models.")
  }
  if (is.null(metrics)) {
    metrics <- c(
      "Chi2", "Chi2_df", "CFI", "RMSEA", "RMSEA_CI", "SRMR",
      "deltaChi2", "deltaChi2_df", "p_deltaChi2", "deltaCFI", "deltaRMSEA"
    )
  }
  names(metrics) <- NULL
  if (any(metrics == "all")) {
    metrics <- c(lavaan_fit_metrics, lavaan_change_metrics)
  }
  unknown <- setdiff(metrics, c(lavaan_fit_metrics, lavaan_change_metrics))
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown metric{?s} for comparing lavaan models: {.val {unknown}}.",
      i = "Choose from {.val {c(lavaan_fit_metrics, lavaan_change_metrics)}}."
    ))
  }
  fit_metrics <- intersect(metrics, lavaan_fit_metrics)
  change_metrics <- intersect(metrics, lavaan_change_metrics)
  names(models) <- model_names(models)

  # Fit of each model
  if (length(fit_metrics) > 0) {
    d <- apa_performance(models, metrics = fit_metrics, column_formats = column_formats)
    record <- attr(d, "apa_note", exact = TRUE)
  } else {
    d <- apa_performance(models, metrics = "CFI", column_formats = column_formats)
    record <- attr(d, "apa_note", exact = TRUE)
    record$abbreviations <- character(0)
    d <- d["Model"]
  }
  d <- tibble::as_tibble(d)

  # Each model compared with the one before it
  k <- length(models)
  info <- purrr::map(models, \(m) lavaan_fit_info(m)$values)
  lrt <- purrr::map(seq_len(k - 1), \(i) {
    lavaan::lavTestLRT(models[[i]], models[[i + 1]])
  })
  lrt_value <- function(column) {
    c(NA, purrr::map_dbl(lrt, \(x) x[[column]][2]))
  }
  change <- function(index) {
    v <- purrr::map_dbl(info, \(x) x[[index]])
    c(NA, diff(v))
  }
  values <- list(
    deltaChi2 = lrt_value("Chisq diff"),
    deltaChi2_df = lrt_value("Df diff"),
    p_deltaChi2 = lrt_value("Pr(>Chisq)"),
    deltaCFI = change("CFI"),
    deltaTLI = change("TLI"),
    deltaRMSEA = change("RMSEA"),
    deltaSRMR = change("SRMR")
  )
  headers <- c(
    deltaChi2 = "&Delta;*&chi;*^2^",
    deltaChi2_df = "&Delta;*df*",
    p_deltaChi2 = "*p*",
    deltaCFI = "&Delta;CFI",
    deltaTLI = "&Delta;TLI",
    deltaRMSEA = "&Delta;RMSEA",
    deltaSRMR = "&Delta;SRMR"
  )
  spanner <- "Change from previous model"
  for (m in change_metrics) {
    v <- values[[m]]
    out <- switch(
      m,
      deltaChi2 = column_formats$Chi2@formatter(v),
      deltaChi2_df = column_formats$Chi2_df@formatter(v),
      p_deltaChi2 = column_formats$p_Chi2@formatter(v),
      align_chr(v, accuracy = .001, trim_leading_zeros = TRUE)
    )
    out[is.na(v)] <- ""
    d[[paste0(spanner, "_", headers[[m]])]] <- out
  }

  methods <- unique(purrr::map_chr(lrt, lavaan_lrt_method))
  record$change <- length(change_metrics) > 0
  record$difference_test <- if (any(c("deltaChi2", "p_deltaChi2") %in% change_metrics)) methods
  attr(d, "apa_note") <- record
  attr(d, "no_format_columns") <- "Model"
  as_apa7_tbl(d)
}

#' The chi-square difference method lavTestLRT() used
#' @keywords internal
#' @noRd
lavaan_lrt_method <- function(x) {
  heading <- paste(attr(x, "heading"), collapse = " ")
  # The method name may be in straight or curly quotes
  m <- regmatches(heading, regexpr("method = .[a-z0-9.]+", heading))
  if (length(m) == 0) {
    return("standard")
  }
  sub("method = .", "", m)
}

#' Describe chi-square difference tests in words
#' @keywords internal
#' @noRd
lavaan_lrt_description <- function(method) {
  sb <- paste0("Satorra", intToUtf8(8211), "Bentler")
  labels <- c(
    standard = "a likelihood ratio (*&chi;*^2^ difference) test",
    satorra.bentler.2001 = paste0("the ", sb, " (2001) scaled *&chi;*^2^ difference test"),
    satorra.bentler.2010 = paste0("the ", sb, " (2010) strictly positive scaled *&chi;*^2^ difference test"),
    satorra.2000 = "the Satorra (2000) scaled and shifted *&chi;*^2^ difference test"
  )
  out <- labels[method]
  out[is.na(out)] <- "a scaled *&chi;*^2^ difference test"
  unname(out)
}
