#' Publication bias table for a meta-analysis
#'
#' Makes an APA-style table of tests and corrections for publication bias
#' (small-study effects) in a meta-analysis fit with `metafor::rma()`, as
#' computed by metafor.
#'
#' The methods (rows) are:
#'
#' * `"egger"`: Egger's regression test ([metafor::regtest()]), a
#'   meta-regression of the effect sizes on their standard errors. The test
#'   statistic is the test of the standard error's coefficient, and the
#'   adjusted estimate is the limit estimate (the predicted effect as the
#'   standard error approaches zero, as in PET).
#' * `"rank"`: Begg and Mazumdar's rank correlation test
#'   ([metafor::ranktest()]), Kendall's \eqn{\tau}{tau} between the
#'   standardized effect sizes and their variances.
#' * `"trimfill"`: the trim-and-fill method ([metafor::trimfill()]), which
#'   imputes the studies (\eqn{k_0}{k0}) that seem to be missing from one side
#'   of the funnel plot and gives the pooled estimate with them included.
#' * `"fsn"`: the fail-safe *N* ([metafor::fsn()]), the number of additional
#'   studies with an average effect of zero that would make the pooled
#'   estimate nonsignificant. It is widely criticized and is not shown by
#'   default.
#'
#' Adjusted estimates are shown on the same scale as in [apa_parameters()]
#' (e.g., risk ratios are exponentiated, and Fisher's *z* values are
#' converted back to correlations), and [apa_note()] gives the unadjusted
#' pooled estimate for comparison and describes each method.
#'
#' These methods are available for models fit with `metafor::rma()`, not for
#' multilevel models (`metafor::rma.mv()`), for which the standard error (or
#' sampling variance) can be added as a moderator instead.
#'
#' @param fit a meta-analysis fit with `metafor::rma()` (without moderators)
#' @param methods methods to display, in order: any of `"egger"`, `"rank"`,
#'   `"trimfill"`, and `"fsn"`
#' @param transform if `TRUE` (default), estimates computed on a transformed
#'   scale are shown on their original scale, as in [apa_parameters()]
#'
#' @returns `apa7_tbl` (tibble)
#' @references
#' Begg, C. B., & Mazumdar, M. (1994). Operating characteristics of a rank
#' correlation test for publication bias. *Biometrics, 50*(4), 1088--1101.
#' \doi{10.2307/2533446}
#'
#' Duval, S., & Tweedie, R. (2000). Trim and fill: A simple
#' funnel-plot-based method of testing and adjusting for publication bias in
#' meta-analysis. *Biometrics, 56*(2), 455--463.
#' \doi{10.1111/j.0006-341X.2000.00455.x}
#'
#' Egger, M., Davey Smith, G., Schneider, M., & Minder, C. (1997). Bias in
#' meta-analysis detected by a simple, graphical test. *BMJ, 315*(7109),
#' 629--634. \doi{10.1136/bmj.315.7109.629}
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("metafor", quietly = TRUE) &&
#'     requireNamespace("metadat", quietly = TRUE)) {
#'   d_bcg <- metafor::escalc(
#'     measure = "RR",
#'     ai = tpos, bi = tneg, ci = cpos, di = cneg,
#'     data = metadat::dat.bcg
#'   )
#'   fit_meta <- metafor::rma(yi, vi, data = d_bcg)
#'   apa_publication_bias(fit_meta)
#' }
#' }
apa_publication_bias <- function(
  fit,
  methods = c("egger", "rank", "trimfill"),
  transform = TRUE
) {
  if (!inherits(fit, "rma")) {
    cli::cli_abort("{.arg fit} must be a meta-analysis fit with {.fn metafor::rma}, not {.obj_type_friendly {fit}}.")
  }
  if (inherits(fit, "rma.mv")) {
    cli::cli_abort(c(
      "Publication bias methods are not available for multilevel models ({.fn metafor::rma.mv}).",
      i = "Add the standard error (e.g., {.code mods = ~ sqrt(vi)}) as a moderator to test for small-study effects."
    ))
  }
  meta_check_class(fit)
  rlang::check_installed("metafor")
  if (!isTRUE(fit$int.only)) {
    cli::cli_abort("Publication bias methods need a model without moderators.")
  }
  allowed <- c("egger", "rank", "trimfill", "fsn")
  unknown <- setdiff(methods, allowed)
  if (length(methods) == 0 || length(unknown) > 0) {
    cli::cli_abort("{.arg methods} must contain only {.or {.val {allowed}}}.")
  }

  info <- meta_measure(fit$measure, transform)
  f <- if (isTRUE(info$transformed)) meta_transform_function(info[["transform"]]) else identity
  level <- meta_level(fit)
  num <- function(v) align_chr(v, accuracy = the$accuracy)
  estimate <- function(v) {
    align_chr(
      v,
      accuracy = the$accuracy,
      trim_leading_zeros = isTRUE(info$bounded),
      format_integers = isTRUE(info$bounded)
    )
  }

  rows <- list()
  details <- list()
  for (method in methods) {
    if (method == "egger") {
      r <- metafor::regtest(fit)
      t_test <- !is.null(r$dfs) && !is.na(r$dfs)
      rows[[method]] <- tibble::tibble(
        Method = "Egger's regression test",
        Statistic = if (t_test) paste0("*t*(", r$dfs, ")") else "*z*",
        Value = num(r$zval),
        p = r$pval,
        est = f(as.numeric(r$est)),
        lo = f(as.numeric(r$ci.lb)),
        hi = f(as.numeric(r$ci.ub))
      )
    } else if (method == "rank") {
      r <- metafor::ranktest(fit)
      rows[[method]] <- tibble::tibble(
        Method = "Rank correlation test",
        Statistic = "*&tau;*",
        Value = align_chr(r$tau, accuracy = the$accuracy, trim_leading_zeros = TRUE, format_integers = TRUE),
        p = r$pval,
        est = NA_real_,
        lo = NA_real_,
        hi = NA_real_
      )
    } else if (method == "trimfill") {
      r <- metafor::trimfill(fit)
      # metafor's default estimator
      details$trimfill <- list(k0 = r$k0, side = r$side, estimator = "L0")
      rows[[method]] <- tibble::tibble(
        Method = "Trim and fill",
        Statistic = "*k*~0~",
        Value = as.character(r$k0),
        p = NA_real_,
        est = f(as.numeric(r$beta)),
        lo = f(as.numeric(r$ci.lb)),
        hi = f(as.numeric(r$ci.ub))
      )
    } else if (method == "fsn") {
      r <- metafor::fsn(fit)
      details$fsn <- list(type = r$type, alpha = r$alpha)
      rows[[method]] <- tibble::tibble(
        Method = "Fail-safe *N*",
        Statistic = "*N*~fs~",
        Value = format(r$fsnum, big.mark = ","),
        p = NA_real_,
        est = NA_real_,
        lo = NA_real_,
        hi = NA_real_
      )
    }
  }
  r <- dplyr::bind_rows(rows)

  ci_header <- paste0(round(level * 100), "% CI")
  has_estimate <- any(!is.na(r$est))
  d <- tibble::tibble(Method = r$Method, Statistic = r$Statistic, Value = align_chr(r$Value))
  d[["*p*"]] <- ifelse(is.na(r$p), "", align_chr(apa_p(r$p), center = "."))
  if (all(d[["*p*"]] == "")) {
    d[["*p*"]] <- NULL
  }
  if (has_estimate) {
    estimate_header <- paste0("Adjusted ", info$header)
    d[[estimate_header]] <- ifelse(is.na(r$est), "", estimate(r$est))
    d[[ci_header]] <- ifelse(
      is.na(r$est),
      "",
      ci_md(estimate(r$lo), estimate(r$hi), missing = is.na(r$est))
    )
  }
  d <- dplyr::mutate(d, dplyr::across(dplyr::everything(), \(v) ifelse(is.na(v), "", v)))

  attr(d, "no_format_columns") <- c("Method", "Statistic")
  attr(d, "apa_note") <- list(
    table = "publication_bias",
    methods = methods,
    header = info$header,
    measure_name = info$name,
    transformed = isTRUE(info$transformed),
    t_test = meta_t_test(fit),
    pooled = c(
      f(as.numeric(fit$b)),
      f(fit$ci.lb),
      f(fit$ci.ub)
    ),
    bounded = isTRUE(info$bounded),
    k = fit$k,
    details = details,
    has_estimate = has_estimate,
    ci = level
  )
  as_apa7_tbl(d)
}

#' @keywords internal
#' @noRd
publication_bias_note_sentences <- function(record) {
  estimate <- function(v) {
    trimws(align_chr(
      v,
      accuracy = the$accuracy,
      trim_leading_zeros = isTRUE(record$bounded),
      format_integers = isTRUE(record$bounded)
    ))
  }
  pooled <- record$pooled
  sentences <- paste0(
    "The unadjusted pooled estimate of the ", record$k, " studies is ",
    record$header, " = ", estimate(pooled[1]),
    ", ", round(record$ci * 100), "% CI [",
    estimate(pooled[2]), ", ", estimate(pooled[3]), "]."
  )
  methods <- record$methods
  if ("egger" %in% methods) {
    sentences <- c(
      sentences,
      paste0(
        "Egger's regression test ", cite("eggerBiasMetaanalysisDetected1997"), " is the test of the standard error in a meta-regression of the effect sizes on their standard errors",
        if (isTRUE(record$has_estimate)) "; its adjusted estimate is the predicted effect as the standard error approaches zero" else "",
        "."
      )
    )
  }
  if ("rank" %in% methods) {
    sentences <- c(
      sentences,
      paste0("The rank correlation test ", cite("beggOperatingCharacteristicsRank1994"), " is Kendall's *&tau;* between the standardized effect sizes and their sampling variances.")
    )
  }
  tf <- record$details$trimfill
  if (!is.null(tf)) {
    sentences <- c(
      sentences,
      paste0(
        "Trim and fill ", cite("duvalTrimFillSimple2000"), " with the ", tf$estimator, " estimator imputed ",
        tf$k0, if (tf$k0 == 1) " study" else " studies",
        if (tf$k0 > 0) paste0(" on the ", tf$side, " side of the funnel plot") else "",
        "; its adjusted estimate includes ",
        if (tf$k0 == 1) "it" else "them",
        "."
      )
    )
  }
  fs <- record$details$fsn
  if (!is.null(fs)) {
    sentences <- c(
      sentences,
      paste0(
        "The fail-safe *N* (",
        tolower(fs$type),
        " method) is the number of additional studies with an average effect of zero that would make the pooled estimate nonsignificant (*p* \u2265 ",
        sub("^0", "", format(fs$alpha)),
        ")."
      )
    )
  }
  if (isTRUE(record$transformed) && isTRUE(record$has_estimate)) {
    sentences <- c(
      sentences,
      paste0("Estimates are ", record$measure_name, ", converted from the scale on which they were analyzed.")
    )
  }
  abbreviations <- c(
    if (isTRUE(record$has_estimate)) c(CI = "confidence interval"),
    if (!is.null(tf)) c(`*k*~0~` = "number of imputed studies"),
    if (!is.null(fs)) c(`*N*~fs~` = "fail-safe *N*")
  )
  c(sentences, abbreviation_sentence(abbreviations))
}
