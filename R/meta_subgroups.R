#' Subgroup analysis table for a meta-analysis
#'
#' Makes an APA-style table of a subgroup analysis of a meta-analysis fit
#' with `metafor::rma()`: the pooled estimate of each subgroup, followed by
#' the overall pooled estimate, with the test of subgroup differences in the
#' note.
#'
#' With `tau2 = "separate"` (default), each subgroup is pooled with its own
#' random-effects model (with the estimator of \eqn{\tau^2}{tau^2} of `fit`),
#' and the subgroup estimates are compared with \eqn{Q_{\text{between}}}{Q
#' between}, the heterogeneity statistic of an equal-effects meta-analysis of
#' the subgroup estimates. With `tau2 = "common"`, the subgroups share one
#' \eqn{\tau^2}{tau^2}: the estimates come from a mixed-effects
#' meta-regression with the subgroup as a categorical moderator, and the
#' subgroups are compared with the omnibus test of the moderator
#' (\eqn{Q_{\text{M}}}{QM}). The separate approach suits subgroups that may
#' differ in heterogeneity; the common approach gives more stable estimates
#' of \eqn{\tau^2}{tau^2} when subgroups have few studies (Borenstein et al.,
#' 2009, chap. 19).
#'
#' Estimates are shown on the same scale as in [apa_parameters()] (e.g., risk
#' ratios are exponentiated, and Fisher's *z* values are converted back to
#' correlations).
#'
#' @param fit a meta-analysis without moderators fit with `metafor::rma()`
#' @param subgroup the subgroup variable: the name of a column in the data of
#'   `fit` (quoted or unquoted), or a vector with one value for each effect
#'   size. Effect sizes whose subgroup is missing are left out of the
#'   subgroups (but not of the overall estimate).
#' @param tau2 `"separate"` (default) for a \eqn{\tau^2}{tau^2} for each
#'   subgroup, or `"common"` for one \eqn{\tau^2}{tau^2} shared by all
#'   subgroups
#' @param transform if `TRUE` (default), estimates computed on a transformed
#'   scale are shown on their original scale, as in [apa_parameters()]
#'
#' @returns `apa7_tbl` (tibble)
#' @references Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein,
#'   H. R. (2009). *Introduction to meta-analysis*. Wiley.
#'   \doi{10.1002/9780470743386}
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
#'   apa_subgroups(fit_meta, alloc)
#' }
#' }
apa_subgroups <- function(
  fit,
  subgroup,
  tau2 = c("separate", "common"),
  transform = TRUE
) {
  tau2 <- rlang::arg_match(tau2)
  if (!inherits(fit, "rma.uni")) {
    cli::cli_abort(
      "{.arg fit} must be a meta-analysis fit with {.fn metafor::rma}, not {.obj_type_friendly {fit}}."
    )
  }
  if (!isTRUE(fit$int.only)) {
    cli::cli_abort("{.arg fit} must be a model without moderators; the subgroups are added by {.fn apa_subgroups}.")
  }
  if (fit$method %in% c("EE", "FE", "CE")) {
    cli::cli_abort("Subgroup analyses need a random-effects model (e.g., {.code method = \"REML\"}).")
  }
  rlang::check_installed("metafor")

  # Subgroup values for every effect size
  quo <- rlang::enquo(subgroup)
  data <- fit$data
  if (rlang::quo_is_symbol(quo) && is.data.frame(data) && rlang::as_name(quo) %in% colnames(data)) {
    label <- rlang::as_name(quo)
    g <- data[[label]]
  } else {
    value <- tryCatch(rlang::eval_tidy(quo), error = function(e) {
      cli::cli_abort(
        "{.arg subgroup} must name a column of the data of {.arg fit} or be a vector with one value for each effect size.",
        call = rlang::caller_env(4)
      )
    })
    if (is.character(value) && length(value) == 1 && is.data.frame(data) && value %in% colnames(data)) {
      label <- value
      g <- data[[value]]
    } else {
      label <- NULL
      g <- value
    }
  }
  yi <- as.vector(fit$yi.f)
  vi <- as.vector(fit$vi.f)
  if (length(g) != length(yi)) {
    cli::cli_abort(
      "{.arg subgroup} must name a column of the data of {.arg fit} or have one value for each of its {length(yi)} effect sizes."
    )
  }
  keep <- !is.na(yi) & !is.na(vi) & !is.na(g)
  levels_g <- if (is.factor(g)) levels(droplevels(as.factor(g[keep]))) else sort(unique(as.character(g[keep])))
  if (length(levels_g) < 2) {
    cli::cli_abort("{.arg subgroup} must have at least 2 subgroups.")
  }
  gc <- as.character(g)
  level <- meta_level(fit)
  test <- if (meta_t_test(fit)) "knha" else "z"

  if (tau2 == "separate") {
    fits <- lapply(levels_g, \(l) {
      i <- keep & gc == l
      metafor::rma(yi[i], vi[i], method = fit$method, test = test, level = level * 100)
    })
    rows <- tibble::tibble(
      Subgroup = levels_g,
      k = vapply(fits, \(f) f$k, numeric(1)),
      est = vapply(fits, \(f) f$b[1], numeric(1)),
      se = vapply(fits, \(f) f$se, numeric(1)),
      lo = vapply(fits, \(f) f$ci.lb, numeric(1)),
      hi = vapply(fits, \(f) f$ci.ub, numeric(1)),
      p = vapply(fits, \(f) f$pval, numeric(1)),
      I2 = vapply(fits, \(f) f$I2, numeric(1))
    )
    between <- metafor::rma(rows$est, sei = rows$se, method = "EE")
    q <- list(stat = between$QE, df = nrow(rows) - 1, p = between$QEp, name = "between")
    tau2_common <- NULL
  } else {
    gf <- factor(gc[keep], levels = levels_g)
    cell <- metafor::rma(yi[keep], vi[keep], mods = ~ gf - 1, method = fit$method, test = test, level = level * 100)
    contrast <- metafor::rma(yi[keep], vi[keep], mods = ~ gf, method = fit$method, test = test, level = level * 100)
    rows <- tibble::tibble(
      Subgroup = levels_g,
      k = as.numeric(table(gf)),
      est = as.vector(cell$b),
      se = cell$se,
      lo = cell$ci.lb,
      hi = cell$ci.ub,
      p = cell$pval,
      I2 = NA_real_
    )
    q <- list(stat = contrast$QM, df = contrast$QMdf[1], df2 = if (test == "knha") contrast$QMdf[2], p = contrast$QMp, name = "M")
    tau2_common <- contrast$tau2
  }

  info <- meta_measure(fit$measure, transform)
  f <- if (isTRUE(info$transformed)) meta_transform_function(info[["transform"]]) else identity
  estimate <- function(v) {
    align_chr(
      v,
      accuracy = the$accuracy,
      trim_leading_zeros = isTRUE(info$bounded),
      format_integers = isTRUE(info$bounded)
    )
  }
  overall_label <- "Overall"
  est <- c(rows$est, as.vector(fit$b))
  lo <- c(rows$lo, fit$ci.lb)
  hi <- c(rows$hi, fit$ci.ub)
  first_header <- if (is.null(label)) "Subgroup" else variable_name_formatter(label)
  d <- tibble::tibble(Subgroup = c(rows$Subgroup, overall_label))
  d[["*k*"]] <- as.character(c(rows$k, fit$k))
  d[[info$header]] <- estimate(f(est))
  d[[paste0(round(level * 100), "% CI")]] <- ci_md(estimate(f(lo)), estimate(f(hi)))
  d[["*p*"]] <- align_chr(apa_p(c(rows$p, fit$pval)), center = ".")
  if (tau2 == "separate") {
    d[["*I*^2^ (%)"]] <- align_chr(c(rows$I2, fit$I2), accuracy = 0.1)
  }

  colnames(d)[1] <- first_header
  attr(d, "no_format_columns") <- first_header
  attr(d, "rule_above") <- overall_label
  attr(d, "apa_note") <- list(
    table = "meta_subgroups",
    subgroup = if (is.null(label)) NULL else variable_name_formatter(label),
    tau2 = tau2,
    method = fit$method,
    t_test = meta_t_test(fit),
    measure_name = info$name,
    transform = info[["transform"]],
    transformed = isTRUE(info$transformed),
    int_only = TRUE,
    q = q,
    tau2_common = tau2_common,
    n_missing = sum(!is.na(yi) & !is.na(vi) & is.na(g)),
    ci = level
  )
  as_apa7_tbl(d)
}

#' @keywords internal
#' @noRd
meta_subgroups_note_sentences <- function(record) {
  sentences <- meta_measure_sentence(record)
  estimator <- meta_estimator(record$method)
  knha <- if (isTRUE(record$t_test)) {
    paste0(", with the Knapp\u2013Hartung adjustment ", cite("knappImprovedTestsRandom2003"))
  } else {
    ""
  }
  if (record$tau2 == "separate") {
    sentences <- c(
      sentences,
      paste0(
        "Each subgroup",
        if (!is.null(record$subgroup)) paste0(" of ", record$subgroup),
        " was pooled with its own random-effects model, with *&tau;*^2^ estimated by ",
        estimator, knha, "."
      )
    )
  } else {
    sentences <- c(
      sentences,
      paste0(
        "Subgroup estimates are from a mixed-effects meta-regression with ",
        record$subgroup %||% "the subgroup",
        " as a moderator, with a common *&tau;*^2^ (",
        trimws(align_chr(record$tau2_common, accuracy = the$accuracy)),
        ") estimated by ", estimator, knha, "."
      )
    )
  }
  sentences <- c(sentences, "The overall estimate is from the meta-analysis of all effect sizes.")
  q <- record$q
  stat <- if (q$name == "between") {
    paste0("*Q*~between~(", q$df, ")")
  } else if (!is.null(q$df2)) {
    paste0("*F*(", q$df, ", ", q$df2, ")")
  } else {
    paste0("*Q*~M~(", q$df, ")")
  }
  sentences <- c(
    sentences,
    paste0(
      "Test of subgroup differences ", cite("borensteinIntroductionMetaanalysis2009"), ": ", stat, " = ",
      trimws(align_chr(q$stat, accuracy = the$accuracy)), ", ", meta_p_text(q$p), "."
    )
  )
  if (isTRUE(record$n_missing > 0)) {
    sentences <- c(
      sentences,
      paste0(
        if (is.null(record$subgroup)) {
          "Effect sizes without a subgroup"
        } else {
          paste0("Effect sizes with a missing value of ", record$subgroup)
        },
        " (*n* = ", record$n_missing, ") are not in any subgroup."
      )
    )
  }
  abbreviations <- c(
    `*k*` = "number of studies",
    CI = "confidence interval",
    `*I*^2^` = if (record$tau2 == "separate") "percentage of total variability due to heterogeneity"
  )
  c(sentences, abbreviation_sentence(unlist(abbreviations)))
}
