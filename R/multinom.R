#' @param category_headings for multinomial models, if `TRUE` (default), the
#'   coefficients of each outcome category are introduced by a heading row
#'   that names the comparison (e.g., "Part time vs. Not working"), with the
#'   predictors indented below it. If `FALSE`, each row is labeled with its
#'   category and predictor (e.g., "Part time: Children").
#' @details
#' For multinomial logistic regression models (`nnet::multinom`), each outcome
#' category except the reference category (the first level of the response)
#' has its own block of coefficients, introduced by a heading row such as
#' "Part time vs. Not working". Coefficients are log odds of the category
#' relative to the reference category, tested with Wald *z* tests, and their
#' exponentiated values (shown by default) are odds ratios relative to the
#' reference category (sometimes called relative risk ratios), labeled *OR*.
#' To compare the categories with a different reference, relevel the response
#' (e.g., with [stats::relevel()]) before fitting the model. [apa_note()] names
#' the reference category.
#' @export
#' @rdname apa_parameters
#' @examples
#' \donttest{
#' if (requireNamespace("nnet", quietly = TRUE) &&
#'     requireNamespace("carData", quietly = TRUE)) {
#'   d <- carData::Womenlf
#'   d$partic <- relevel(d$partic, ref = "not.work")
#'   nnet::multinom(partic ~ hincome + children, data = d, trace = FALSE) |>
#'     apa_parameters()
#' }
#' }
apa_parameters.multinom <- function(
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
  category_headings = TRUE,
  ...
) {
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }
  names(predictor_parameters) <- NULL

  mp <- parameters::model_parameters(fit, ...)
  d_b <- tibble::as_tibble(mp)

  # Format variable names here, because headings are added below
  name_formatter <- column_formats$Parameter@formatter
  pretty <- attr(mp, "pretty_names")
  labels <- unname(pretty[d_b$Parameter])
  labels[is.na(labels)] <- d_b$Parameter[is.na(labels)]
  d_b$Parameter <- name_formatter(labels)

  if (isTRUE(exponentiate)) {
    d_b <- add_exp_columns(d_b, predictor_parameters)
    column_formats <- label_exp_columns(
      column_formats,
      c(
        header = "*OR*",
        latex = "$OR$",
        abbreviation = "odds ratio (relative to the reference category)"
      )
    )
  }

  # z tests have infinite df, which should not be displayed
  if ("df_error" %in% colnames(d_b) && all(is.infinite(d_b$df_error))) {
    d_b$df_error <- NULL
  }

  categories <- fit$lev
  reference <- categories[1]
  response <- insight::find_response(fit)

  # One block of coefficients for each category (models with two categories
  # have a single block, without a Response column)
  if (!("Response" %in% colnames(d_b))) {
    d_b$Response <- categories[2]
  }
  compared <- unique(d_b$Response)
  if (category_headings) {
    pieces <- purrr::map(compared, \(category) {
      block <- d_b[d_b$Response == category, ]
      block$Response <- NULL
      block$Parameter <- paste0("\u2003", block$Parameter)
      dplyr::bind_rows(
        tibble::tibble(Parameter = paste0(category, " vs. ", reference)),
        block
      )
    })
    d_b <- dplyr::bind_rows(pieces)
  } else {
    d_b$Parameter <- paste0(d_b$Response, ": ", d_b$Parameter)
    d_b$Response <- NULL
  }

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
      multinomial = list(
        response = variable_name_formatter(response),
        reference = reference,
        exponentiated = isTRUE(exponentiate) &&
          "Exp_Coefficient" %in% predictor_parameters
      )
    )
  ) |>
    labels_formatted()
}

#' @details
#' For multinomial logistic regression models (`nnet::multinom`), `R2` is
#' McFadden's pseudo-*R*^2^, and the default metrics are `R2` and `AIC`.
#' @export
#' @rdname apa_performance
apa_performance.multinom <- function(
  fit,
  metrics = c("R2", "AIC"),
  column_formats = NULL
) {
  apa_performance.lm(fit, metrics = metrics, column_formats = column_formats)
}

#' Note sentence for multinomial models
#' @param multinomial list with `response`, `reference`, and `exponentiated`
#' @keywords internal
#' @noRd
multinomial_note_sentence <- function(multinomial) {
  if (length(multinomial) == 0) {
    return(character(0))
  }
  paste0(
    "Coefficients compare each category of ",
    multinomial$response,
    " with the reference category (",
    multinomial$reference,
    ")",
    if (isTRUE(multinomial$exponentiated)) "" else " on the log-odds scale",
    "."
  )
}
