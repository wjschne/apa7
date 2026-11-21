#' Write a table note
#'
#' Makes the text of an APA-style table note from what a table function
#' recorded about how the table was made. The text is a starting point:
#' edit it or add to it as needed.
#'
#' For [apa_anova()] tables, the note can say
#'
#' * which type of sums of squares was used,
#' * which effects have sphericity-corrected degrees of freedom (for `afex`
#'   models, only effects involving a within-subjects factor with more than
#'   two levels are corrected),
#' * what the effect size (and *MSE*, for `afex` models) stands for, and
#' * what stars or bold values mean.
#'
#' The sentences are in APA order: general notes first, then probability notes.
#' The text does not start with "*Note*." because apaquarto adds it. With
#' [apaquarto](https://wjschne.github.io/apaquarto/), make the table in one
#' chunk and display it in another, using the note as the `apa-note` chunk
#' option:
#'
#' ````
#' ```{r}
#' #| label: anova-fit
#' tbl <- apa_anova(fit, type = 3)
#' ```
#'
#' ```{r}
#' #| label: tbl-anova
#' #| tbl-cap: Analysis of Variance
#' #| apa-note: !expr apa_note(tbl)
#' tbl
#' ```
#' ````
#'
#' The information is stored in an `"apa_note"` attribute of the table. dplyr
#' functions such as `mutate()` and `filter()` keep it, but some operations
#' (e.g., `bind_rows()` and joins) drop it, so call `apa_note()` on the
#' table that the table function returned.
#'
#' @param x a table made by [apa_anova()] (or [apa_parameters()] for `aov` and `afex` models)
#' @param collapse string that joins the sentences. Use `NULL` to get a character vector with one sentence per element.
#'
#' @returns character
#' @export
#' @examples
#' d <- mtcars
#' d$cyl <- factor(d$cyl)
#' d$am <- factor(d$am)
#' fit <- aov(mpg ~ cyl * am, data = d)
#' tbl <- apa_anova(fit, starred = "F")
#' apa_note(tbl)
#'
#' if (requireNamespace("afex", quietly = TRUE)) {
#'   data(obk.long, package = "afex")
#'   fit_afex <- afex::aov_ez(
#'     "id", "value", obk.long,
#'     between = "treatment", within = "phase", fun_aggregate = mean
#'   )
#'   apa_note(apa_anova(fit_afex))
#' }
apa_note <- function(x, collapse = " ") {
  record <- attr(x, "apa_note", exact = TRUE)
  if (is.null(record)) {
    cli::cli_abort(c(
      "{.arg x} has no table note information.",
      i = "{.fn apa_note} works with tables made by {.fn apa_anova}.",
      i = "The information is stored in an attribute that some operations (e.g., {.fn dplyr::bind_rows} and joins) drop. Call {.fn apa_note} on the table before changing it."
    ))
  }

  sentences <- switch(
    record$table,
    anova = anova_note_sentences(record),
    cli::cli_abort("Unknown table type {.val {record$table}}.") # nocov
  )

  if (is.null(collapse)) {
    return(sentences)
  }
  paste(sentences, collapse = collapse)
}

#' Record what an ANOVA table's note needs
#'
#' @param ss_type 1, 2, 3, or NA
#' @param es_column effect size column shown in the table (or NULL)
#' @param column_formats column_formats used for the table
#' @param starred,bolded whether stars or bolding were applied
#' @param correction sphericity correction for afex models ("GG", "HF", "none")
#' @param corrected_factors formatted names of within-subjects factors with
#'   more than two levels
#' @param mse whether an afex *MSE* column is shown
#' @keywords internal
#' @noRd
anova_note_record <- function(
  ss_type = NA,
  es_column = NULL,
  column_formats = NULL,
  starred = FALSE,
  bolded = FALSE,
  correction = NULL,
  corrected_factors = character(0),
  mse = FALSE
) {
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }
  es <- NULL
  if (!is.null(es_column) && !is.na(es_column)) {
    es <- list(
      column = es_column,
      header = if (is.null(column_formats[[es_column]])) {
        es_column
      } else {
        column_formats[[es_column]]@header
      },
      label = anova_es_labels[[es_column]]
    )
  }
  list(
    table = "anova",
    ss_type = ss_type,
    correction = correction,
    corrected_factors = corrected_factors,
    effect_size = es,
    mse = mse,
    starred = starred,
    bolded = bolded
  )
}

#' @keywords internal
#' @noRd
anova_es_labels <- list(
  Eta2 = "eta squared",
  Eta2_partial = "partial eta squared",
  Eta2_generalized = "generalized eta squared",
  Omega2 = "omega squared",
  Omega2_partial = "partial omega squared",
  Epsilon2 = "epsilon squared",
  Epsilon2_partial = "partial epsilon squared"
)

#' @keywords internal
#' @noRd
anova_note_sentences <- function(record) {
  sentences <- character(0)

  # Sums of squares
  if (!is.null(record$ss_type) && !is.na(record$ss_type)) {
    sentences <- c(
      sentences,
      switch(
        as.character(record$ss_type),
        `1` = "Sums of squares are sequential (Type I).",
        `2` = "Type II sums of squares.",
        `3` = "Type III sums of squares."
      )
    )
  }

  # Sphericity
  if (length(record$corrected_factors) > 0 && !is.null(record$correction)) {
    correction_name <- switch(
      record$correction,
      GG = paste0("Greenhouse", intToUtf8(8211), "Geisser"),
      HF = paste0("Huynh", intToUtf8(8211), "Feldt"),
      NA_character_
    )
    if (is.na(correction_name)) {
      sentences <- c(
        sentences,
        "Degrees of freedom are not corrected for violations of sphericity."
      )
    } else {
      sentences <- c(
        sentences,
        paste0(
          "Degrees of freedom and *p*-values for effects involving ",
          or_list(record$corrected_factors),
          " are ",
          correction_name,
          " corrected."
        )
      )
    }
  }

  # Abbreviations
  abbreviations <- character(0)
  if (isTRUE(record$mse)) {
    abbreviations <- c(abbreviations, "*MSE* = mean square error")
  }
  if (!is.null(record$effect_size) && !is.null(record$effect_size$label)) {
    abbreviations <- c(
      abbreviations,
      paste0(record$effect_size$header, " = ", record$effect_size$label)
    )
  }
  if (length(abbreviations) > 0) {
    sentences <- c(sentences, paste0(paste(abbreviations, collapse = "; "), "."))
  }

  # Probability notes
  if (isTRUE(record$starred)) {
    sentences <- c(sentences, paste0(apa_p_star_note(), "."))
  }
  if (isTRUE(record$bolded)) {
    sentences <- c(
      sentences,
      paste0(
        "Bold values are statistically significant at *p*",
        intToUtf8(c(160, 8804, 160)),
        ".05."
      )
    )
  }

  sentences
}

#' "A", "A or B", "A, B, or C"
#' @keywords internal
#' @noRd
or_list <- function(x) {
  n <- length(x)
  if (n <= 2) {
    return(paste(x, collapse = " or "))
  }
  paste0(paste(x[-n], collapse = ", "), ", or ", x[n])
}

#' Type of sums of squares from the heading of an ANOVA table
#' @keywords internal
#' @noRd
anova_heading_type <- function(tab) {
  heading <- paste(attr(tab, "heading", exact = TRUE), collapse = " ")
  if (grepl("Type III", heading, fixed = TRUE)) {
    return(3L)
  }
  if (grepl("Type II", heading, fixed = TRUE)) {
    return(2L)
  }
  if (grepl("Analysis of Variance Table", heading, fixed = TRUE)) {
    return(1L)
  }
  NA_integer_
}

#' Whether stars or bolding were applied to columns in the table
#' @keywords internal
#' @noRd
anova_marked <- function(marked, anova_parameters) {
  marked <- marked[!is.na(marked) & marked != ""]
  length(intersect(marked, anova_parameters)) > 0
}
