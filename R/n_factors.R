#' Factor retention table
#'
#' Makes an APA-style table of how many factors (or components) each method
#' suggests retaining, as computed by [parameters::n_factors()].
#'
#' `x` can be the output of [parameters::n_factors()], or a data frame, in
#' which case [parameters::n_factors()] is run with the arguments in `...`
#' (e.g., `type = "PCA"` for components, or `package` to choose the packages
#' whose methods are used: by default nFactors and psych, which must be
#' installed).
#'
#' With `table = "methods"` (default), each row is a method and the number of
#' factors it suggests, in the order of the number of factors. The numbers
#' supported by the most methods are bolded (`bold_consensus = TRUE`). With
#' `table = "agreement"`, each row is a number of factors suggested by at
#' least one method, with the number of methods that suggest it and the
#' cumulative proportion of variance explained by that many factors.
#'
#' The note names the number of factors supported by the most methods and
#' those methods. Methods differ greatly in how well they work, and they often
#' disagree: parallel analysis and Velicer's minimum average partial (MAP)
#' test are generally recommended, while the Kaiser criterion (eigenvalues
#' greater than 1) and the chi-square tests (Bartlett's, Anderson's, and
#' Lawley's) tend to suggest too many factors. The number of methods that
#' agree is not by itself a good reason to retain a number of factors.
#'
#' @param x the output of [parameters::n_factors()], or a data frame passed to
#'   it
#' @param table `"methods"` (default) for a row per method, or `"agreement"`
#'   for a row per suggested number of factors
#' @param bold_consensus if `TRUE` (default), the numbers of factors supported
#'   by the most methods are bolded (methods table only)
#' @param format_method_names if `TRUE` (default), method names are spelled
#'   out (e.g., "Velicer's MAP" becomes "Velicer's minimum average partial
#'   (MAP)" and "beta" becomes "Multiple regression (*&beta;*)"). Set to `FALSE`
#'   to keep the names exactly as [parameters::n_factors()] gives them.
#' @param components if `TRUE`, the table and note speak of components rather
#'   than factors. If `NULL` (default), components are assumed when `x` is a
#'   data frame and `type = "PCA"` is passed in `...`. The output of
#'   [parameters::n_factors()] and [parameters::n_components()] does not record
#'   which was estimated, so set `components = TRUE` for the output of
#'   [parameters::n_components()].
#' @param ... arguments passed to [parameters::n_factors()] when `x` is a data
#'   frame (e.g., `type`, `rotation`, `algorithm`, and `package`)
#'
#' @returns `apa7_tbl` (tibble)
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("nFactors", quietly = TRUE)) {
#'   d <- na.omit(psych::bfi[, 1:25])
#'   apa_n_factors(d)
#'
#'   # One row per suggested number of factors
#'   nf <- parameters::n_factors(d)
#'   apa_n_factors(nf, table = "agreement")
#'
#'   apa_n_factors(nf) |>
#'     apa_note()
#' }
#' }
apa_n_factors <- function(
  x,
  table = c("methods", "agreement"),
  bold_consensus = TRUE,
  format_method_names = TRUE,
  components = NULL,
  ...
) {
  table <- match.arg(table)
  if (!(is.logical(bold_consensus) && length(bold_consensus) == 1 && !is.na(bold_consensus))) {
    cli::cli_abort("{.arg bold_consensus} must be {.code TRUE} or {.code FALSE}.")
  }
  if (inherits(x, "n_factors")) {
    nf <- x
  } else if (is.data.frame(x)) {
    nf <- parameters::n_factors(x, ...)
  } else {
    cli::cli_abort(
      "{.arg x} must be the output of {.fn parameters::n_factors} or a data frame, not {.obj_type_friendly {x}}."
    )
  }

  d_nf <- as.data.frame(nf)
  if (nrow(d_nf) == 0) {
    cli::cli_abort("{.fn parameters::n_factors} found no suggested number of factors.") # nocov
  }
  methods <- d_nf$Method
  labels <- if (format_method_names) n_factors_method_labels(methods) else methods
  n_suggested <- as.integer(d_nf$n_Factors)

  # The numbers of factors supported by the most methods
  counts <- table(n_suggested)
  consensus <- as.integer(names(counts)[counts == max(counts)])
  consensus_methods <- labels[n_suggested %in% consensus]

  # Components or factors
  dots <- list(...)
  if (is.null(components)) {
    components <- !inherits(x, "n_factors") && !is.null(dots$type) && toupper(dots$type[1]) == "PCA"
  }
  if (!(is.logical(components) && length(components) == 1 && !is.na(components))) {
    cli::cli_abort("{.arg components} must be {.code NULL}, {.code TRUE}, or {.code FALSE}.")
  }
  is_pca <- components
  unit <- if (is_pca) "components" else "factors"
  unit_header <- if (is_pca) "Components" else "Factors"

  if (table == "methods") {
    shown <- as.character(n_suggested)
    bolded <- bold_consensus && length(unique(n_suggested)) > 1
    if (bolded) {
      shown[n_suggested %in% consensus] <- paste0("**", shown[n_suggested %in% consensus], "**")
    }
    d <- tibble::tibble(Method = labels)
    d[[unit_header]] <- shown
    attr(d, "no_format_columns") <- "Method"
  } else {
    bolded <- FALSE
    agreement <- tibble::tibble(n = as.integer(names(counts)), methods = as.integer(counts))
    variance <- attr(nf, "Variance_Explained", exact = TRUE)
    d <- tibble::tibble(n = agreement$n, Methods = agreement$methods)
    names(d)[1] <- unit_header
    if (!is.null(variance) && all(c("n_Factors", "Variance_Cumulative") %in% colnames(variance))) {
      cumulative <- variance$Variance_Cumulative[match(agreement$n, variance$n_Factors)]
      d[["Cumulative variance"]] <- align_chr(
        cumulative,
        accuracy = .01,
        trim_leading_zeros = TRUE
      )
    }
  }

  attr(d, "apa_note") <- list(
    table = "n_factors",
    layout = table,
    unit = unit,
    consensus = consensus,
    consensus_methods = consensus_methods,
    n_methods = length(methods),
    bolded = bolded,
    variance = table == "agreement" && "Cumulative variance" %in% colnames(d)
  )
  as_apa7_tbl(d)
}

#' Spelled-out names of the methods of parameters::n_factors()
#' @keywords internal
#' @noRd
n_factors_method_labels <- function(methods) {
  labels <- c(
    `Acceleration factor` = "Acceleration factor",
    CNG = "Cattell–Nelson–Gorsuch (CNG)",
    beta = "Multiple regression (*&beta;*)",
    t = "Multiple regression (*t*)",
    p = "Multiple regression (*p*)",
    `VSS complexity 1` = "Very simple structure (complexity 1)",
    `VSS complexity 2` = "Very simple structure (complexity 2)",
    `Velicer's MAP` = "Velicer's minimum average partial (MAP)",
    `Optimal coordinates` = "Optimal coordinates",
    `Parallel analysis` = "Parallel analysis",
    `Kaiser criterion` = "Kaiser criterion (eigenvalues > 1)",
    `Scree (R2)` = "Standard error scree (*R*^2^)",
    `Scree (SE)` = "Standard error scree (*SE*)",
    BIC = "*BIC*",
    `BIC (adjusted)` = "Sample-size adjusted *BIC*",
    Bentler = "Bentler's test",
    Bartlett = "Bartlett's test",
    Anderson = "Anderson's test",
    Lawley = "Lawley's test",
    `EGA (glasso)` = "Exploratory graph analysis (glasso)",
    `EGA (TMFG)` = "Exploratory graph analysis (TMFG)"
  )
  out <- unname(labels[methods])
  out[is.na(out)] <- methods[is.na(out)]
  out
}

#' @keywords internal
#' @noRd
n_factors_note_sentences <- function(record) {
  unit <- record$unit
  consensus <- record$consensus
  n_consensus <- length(record$consensus_methods)
  of_methods <- paste0(n_consensus, " of ", record$n_methods, " methods")
  sentences <- if (length(consensus) == 1) {
    paste0(
      "The number of ", unit, " supported by the most methods is ", consensus,
      ", by ", of_methods, ": ", and_list(record$consensus_methods), "."
    )
  } else {
    paste0(
      "The numbers of ", unit, " supported by the most methods are ",
      and_list(as.character(consensus)), ", each by ",
      n_consensus / length(consensus), " of ", record$n_methods, " methods."
    )
  }
  if (identical(record$layout, "agreement")) {
    sentences <- c(
      sentences,
      paste0(
        "Methods = number of methods that suggest retaining that many ", unit, "."
      )
    )
    if (isTRUE(record$variance)) {
      sentences <- c(
        sentences,
        paste0(
          "Cumulative variance = proportion of variance explained by that many ",
          unit, "."
        )
      )
    }
  }
  if (isTRUE(record$bolded)) {
    sentences <- c(
      sentences,
      if (length(consensus) == 1) {
        paste0("The number of ", unit, " supported by the most methods is bolded.")
      } else {
        paste0("The numbers of ", unit, " supported by the most methods are bolded.")
      }
    )
  }
  sentences
}
