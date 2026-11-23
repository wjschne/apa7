#' @details
#' For zero-inflated and hurdle count models fit with `pscl::zeroinfl()` and
#' `pscl::hurdle()`, the count model and the zero model are each introduced by
#' a heading row ("Count model" and "Zero-inflation model" or "Zero hurdle
#' model"). Coefficients are tested with Wald *z* tests. Only the count model's
#' coefficients are exponentiated (by default), giving incidence rate ratios
#' (*IRR*), because the zero model has a different link and meaning. In a
#' zero-inflated model, the zero model predicts excess zeros (zeros not
#' produced by the count model); in a hurdle model, it predicts whether the
#' count is greater than zero. [apa_note()] says which, names the count
#' distribution, and gives the negative binomial dispersion parameter
#' (\eqn{\theta}{theta}).
#' @export
#' @rdname apa_parameters
#' @examples
#' \donttest{
#' if (requireNamespace("pscl", quietly = TRUE)) {
#'   data("bioChemists", package = "pscl")
#'   pscl::zeroinfl(art ~ fem + kid5 + ment | ment,
#'                  data = bioChemists, dist = "negbin") |>
#'     apa_parameters()
#' }
#' }
apa_parameters.zeroinfl <- function(
  fit,
  predictor_parameters = c(
    "Parameter",
    "Coefficient",
    "SE",
    "Exp_Coefficient",
    "z",
    "p"
  ),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  exponentiate = TRUE,
  ...
) {
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }
  names(predictor_parameters) <- NULL

  mp <- parameters::model_parameters(fit, ...)
  d_b <- tibble::as_tibble(mp)
  if (!("Component" %in% colnames(d_b))) {
    d_b$Component <- "conditional"
  }

  # Format variable names here, because headings are added below
  name_formatter <- column_formats$Parameter@formatter
  pretty <- attr(mp, "pretty_names")
  labels <- unname(pretty[d_b$Parameter])
  labels[is.na(labels)] <- d_b$Parameter[is.na(labels)]
  d_b$Parameter <- name_formatter(labels)

  # Only the count model is exponentiated, because the zero model has its own
  # link and meaning
  if (isTRUE(exponentiate)) {
    d_b <- add_exp_columns(d_b, predictor_parameters)
    not_count <- d_b$Component != "conditional"
    for (col in intersect(c("Exp_Coefficient", "Exp_CI_low", "Exp_CI_high"), colnames(d_b))) {
      d_b[[col]][not_count] <- NA
    }
    column_formats <- label_exp_columns(
      column_formats,
      c(
        header = "*IRR*",
        latex = "$IRR$",
        abbreviation = "incidence rate ratio"
      )
    )
  }

  # z tests have infinite df, which should not be displayed
  if ("df_error" %in% colnames(d_b) && all(is.infinite(d_b$df_error))) {
    d_b$df_error <- NULL
  }

  is_hurdle <- inherits(fit, "hurdle")
  headings <- c(
    conditional = "Count model",
    zero_inflated = if (is_hurdle) "Zero hurdle model" else "Zero-inflation model"
  )
  components <- intersect(names(headings), d_b$Component)
  pieces <- purrr::map(components, \(component) {
    block <- d_b[d_b$Component == component, ]
    block$Component <- NULL
    block$Parameter <- paste0("\u2003", block$Parameter)
    dplyr::bind_rows(
      tibble::tibble(Parameter = headings[[component]]),
      block
    )
  })
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
      vcov = describe_vcov(list(...)$vcov),
      count_zero = count_zero_record(fit)
    )
  ) |>
    labels_formatted()
}

#' @export
#' @rdname apa_parameters
apa_parameters.hurdle <- apa_parameters.zeroinfl

#' @details
#' For zero-inflated and hurdle models (`pscl::zeroinfl` and `pscl::hurdle`),
#' `R2` is computed by [performance::r2_zeroinflated()], and the default
#' metrics are `R2` and `AIC`.
#' @export
#' @rdname apa_performance
apa_performance.zeroinfl <- function(
  fit,
  metrics = c("R2", "AIC"),
  column_formats = NULL
) {
  apa_performance.lm(fit, metrics = metrics, column_formats = column_formats)
}

#' @export
#' @rdname apa_performance
apa_performance.hurdle <- apa_performance.zeroinfl

#' What the note needs to know about a zero-inflated or hurdle model
#' @keywords internal
#' @noRd
count_zero_record <- function(fit) {
  is_hurdle <- inherits(fit, "hurdle")
  dist <- fit$dist
  if (is.list(dist)) {
    count_dist <- dist$count
    zero_dist <- dist$zero
  } else {
    count_dist <- dist
    zero_dist <- "binomial"
  }
  theta <- fit$theta
  if (length(theta) > 0 && "count" %in% names(theta)) {
    theta <- theta[["count"]]
  }
  list(
    type = if (is_hurdle) "hurdle" else "zeroinfl",
    count_dist = count_dist,
    zero_dist = zero_dist,
    link = if (identical(zero_dist, "binomial")) fit$link else "log",
    theta = if (identical(count_dist, "negbin") && length(theta) == 1) unname(theta)
  )
}

#' Combine the count/zero records of a list of models
#'
#' The note describes the first model. Each model has its own dispersion
#' parameter, so it is left out when there is more than one model.
#' @keywords internal
#' @noRd
merge_count_zero_records <- function(records) {
  if (length(records) == 0) {
    return(NULL)
  }
  record <- records[[1]]
  if (length(records) > 1) {
    record$theta <- NULL
  }
  record
}

#' Note sentences for zero-inflated and hurdle models
#' @param record list made by `count_zero_record()`
#' @keywords internal
#' @noRd
count_zero_note_sentences <- function(record) {
  if (length(record) == 0) {
    return(character(0))
  }
  dist_names <- c(
    poisson = "Poisson",
    negbin = "negative binomial",
    geometric = "geometric"
  )
  count_dist <- record$count_dist
  dist_name <- if (count_dist %in% names(dist_names)) dist_names[[count_dist]] else count_dist
  if (record$type == "hurdle") {
    dist_name <- paste("zero-truncated", dist_name)
  }
  theta <- if (!is.null(record$theta)) {
    paste0(
      " with dispersion parameter *\u03b8* = ",
      formatC(record$theta, format = "f", digits = 2)
    )
  } else {
    ""
  }
  count_sentence <- paste0(
    "The count model is ",
    dist_name,
    theta,
    ", and its coefficients are on the log scale."
  )

  scale_names <- c(
    logit = "log odds",
    probit = "probit",
    cloglog = "complementary log-log",
    cauchit = "cauchit",
    log = "log"
  )
  link <- record$link
  scale <- if (link %in% names(scale_names)) scale_names[[link]] else link
  if (record$type == "hurdle") {
    zero_sentence <- if (identical(record$zero_dist, "binomial")) {
      paste0(
        "Zero hurdle coefficients ",
        if (link == "logit") "are log odds" else paste0("are on the ", scale, " scale"),
        " of a count greater than zero."
      )
    } else {
      paste0(
        "Zero hurdle coefficients are from a censored ",
        if (record$zero_dist %in% names(dist_names)) dist_names[[record$zero_dist]] else record$zero_dist,
        " model of whether the count is greater than zero."
      )
    }
  } else {
    zero_sentence <- paste0(
      "Zero-inflation coefficients ",
      if (link == "logit") "are log odds" else paste0("are on the ", scale, " scale"),
      " of an excess zero (a zero not produced by the count model)."
    )
  }
  c(count_sentence, zero_sentence)
}
