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
#' For [apa_loadings()] tables, the note can say
#'
#' * how the factors were extracted and rotated (e.g., "Principal axis
#'   factoring with oblimin rotation"), and that loadings after an oblique
#'   rotation are pattern coefficients,
#' * which loadings are omitted (`min_loading`) or bolded (`bold_max`),
#' * what the *h*^2^, *u*^2^, and Complexity columns stand for, and
#' * that the cumulative proportion of variance is approximate when the
#'   factors are correlated.
#'
#' For confirmatory factor analyses fit with `lavaan`, the note instead gives
#' the sample size and estimator, says that the loadings are standardized, and
#' says that blank cells are loadings fixed at 0.
#'
#' For [apa_factor_cor()] tables, the note says how the factors were
#' extracted and rotated (for `lavaan` models, the sample size, the
#' estimator, and that the correlations are standardized).
#'
#' For [apa_htest()] tables, the note can say
#'
#' * which tests were done (e.g., Welch's *t* tests) and what the groups were
#'   compared with or the variables were correlated with,
#' * that *p*-values are one-sided, if they are,
#' * how Cohen's *d* (or Hedges' *g*) was standardized,
#' * what the effect size and its confidence interval stand for, and
#' * what stars or bold values mean.
#'
#' For [apa_parameters()] tables of regression models (`lm`, `glm`, `polr`,
#' `clm`, `lme4`, `glmmTMB`, and `nlme` models, or a list of models), the note can say
#'
#' * which robust standard errors were used (e.g., `vcov = "HC3"`),
#' * how the degrees of freedom of mixed-model *t* tests were approximated
#'   (Kenward-Roger or Satterthwaite),
#' * that ordinal-model thresholds are not tested,
#' * the scale of the zero-inflation and dispersion coefficients of `glmmTMB`
#'   models,
#' * the residual correlation structure and variance function of `nlme::lme`
#'   models,
#' * what the standardized coefficient (\eqn{\beta}{beta}), exponentiated coefficient
#'   (e.g., *OR*), confidence interval, and model-comparison (e.g.,
#'   \eqn{\Delta R^2}{Delta R^2}) columns stand for, and
#' * what stars or bold values mean.
#'
#' Abbreviations are defined from the `abbreviation` slot of each column's
#' [column_format()], so a note defines every abbreviated header that has a
#' definition. To define another header, or to change a definition, set the
#' slot in the `column_formats` passed to the table function (or set the
#' default with [apa7_defaults()]):
#'
#' ```r
#' cf <- column_formats()
#' cf$Sum_Squares@abbreviation <- "sum of squares"
#' apa_anova(fit, anova_parameters = c("Parameter", "Sum_Squares", "df", "F", "p"),
#'           column_formats = cf)
#' ```
#'
#' The sentences are in APA order: general notes first, then probability notes.
#' The text does not start with "*Note*." because apaquarto adds it. With
#' [apaquarto](https://wjschne.github.io/apaquarto/), make the table in one
#' chunk and display it in another, using the note as the `apa-note` chunk
#' option. Write `apa7::apa_note()` there, because in documents with cached
#' chunks, chunk options can be evaluated before `library(apa7)` takes effect:
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
#' #| apa-note: !expr apa7::apa_note(tbl)
#' tbl
#' ```
#' ````
#'
#' The information is stored in an `"apa_note"` attribute of the table. dplyr
#' functions such as `mutate()` and `filter()` keep it, but some operations
#' (e.g., `bind_rows()` and joins) drop it, so call `apa_note()` on the
#' table that the table function returned.
#'
#' @param x a table made by [apa_anova()], [apa_htest()], [apa_parameters()], [apa_loadings()], or [apa_factor_cor()]
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
#'
#' # Factor analysis
#' fit_fa <- psych::fa(Harman74.cor$cov, 4, fm = "pa", rotate = "varimax")
#' apa_loadings(fit_fa, min_loading = .3, communality = TRUE) |>
#'   apa_note()
#'
#' # Regression
#' lm(mpg ~ cyl + wt, data = mtcars) |>
#'   apa_parameters(starred = "p") |>
#'   apa_note()
apa_note <- function(x, collapse = " ") {
  record <- attr(x, "apa_note", exact = TRUE)
  if (is.null(record)) {
    cli::cli_abort(c(
      "{.arg x} has no table note information.",
      i = "{.fn apa_note} works with tables made by {.fn apa_anova}, {.fn apa_htest}, {.fn apa_parameters}, {.fn apa_loadings}, {.fn apa_factor_cor}, and {.fn apa_performance} (for lavaan models).",
      i = "The information is stored in an attribute that some operations (e.g., {.fn dplyr::bind_rows} and joins) drop. Call {.fn apa_note} on the table before changing it."
    ))
  }

  sentences <- switch(
    record$table,
    anova = anova_note_sentences(record),
    loadings = loadings_note_sentences(record),
    factor_cor = factor_cor_note_sentences(record),
    parameters = parameters_note_sentences(record),
    htest = htest_note_sentences(record),
    fit = fit_note_sentences(record),
    sem = sem_note_sentences(record),
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
#' @param abbreviations definitions of the abbreviated headers shown (from
#'   column_abbreviations())
#' @keywords internal
#' @noRd
anova_note_record <- function(
  ss_type = NA,
  abbreviations = character(0),
  starred = FALSE,
  bolded = FALSE,
  correction = NULL,
  corrected_factors = character(0),
  mixed = FALSE,
  df_method = NA,
  effect_size_header = NULL
) {
  list(
    table = "anova",
    ss_type = ss_type,
    correction = correction,
    corrected_factors = corrected_factors,
    mixed = mixed,
    df_method = df_method,
    effect_size_header = effect_size_header,
    abbreviations = abbreviations,
    starred = starred,
    bolded = bolded
  )
}

#' Definitions of the abbreviated headers of columns
#'
#' @param columns column format names of the columns shown, in table order
#' @param column_formats column_formats (or a named list of column_format
#'   objects) used for the table
#' @returns character vector of definitions named by the term defined (the
#'   header, unless the abbreviation is named), without duplicate terms
#' @keywords internal
#' @noRd
column_abbreviations <- function(columns, column_formats) {
  out <- character(0)
  for (name in columns) {
    cf <- column_formats[[name]]
    if (is.null(cf)) {
      next
    }
    abbreviation <- cf@abbreviation
    if (length(abbreviation) == 0 || is.na(abbreviation) || !nzchar(abbreviation)) {
      next
    }
    term <- names(abbreviation)
    if (is.null(term) || is.na(term) || !nzchar(term)) {
      term <- cf@header
    }
    if (!(term %in% names(out))) {
      out[[term]] <- unname(abbreviation)
    }
  }
  out
}

#' Combine definitions, keeping the first definition of each term
#' @keywords internal
#' @noRd
combine_abbreviations <- function(...) {
  out <- c(...)
  if (length(out) == 0) {
    return(character(0))
  }
  out[!duplicated(names(out))]
}

#' "*MSE* = mean square error; CI = confidence interval."
#' @keywords internal
#' @noRd
abbreviation_sentence <- function(abbreviations) {
  if (length(abbreviations) == 0) {
    return(NULL)
  }
  paste0(
    paste(names(abbreviations), abbreviations, sep = " = ", collapse = "; "),
    "."
  )
}

#' @keywords internal
#' @noRd
anova_note_sentences <- function(record) {
  sentences <- character(0)

  # Sums of squares (mixed models: type of tests)
  if (isTRUE(record$mixed) && !is.null(record$ss_type) && !is.na(record$ss_type)) {
    sentences <- c(
      sentences,
      paste0(
        "*F* tests of fixed effects are Type ",
        c("I", "II", "III")[record$ss_type],
        "."
      )
    )
  } else if (!is.null(record$ss_type) && !is.na(record$ss_type)) {
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

  # Denominator degrees of freedom of mixed models
  if (isTRUE(record$mixed) && isTRUE(record$df_method %in% c("kenward", "satterthwaite"))) {
    sentences <- c(
      sentences,
      paste0(
        "Denominator degrees of freedom are ",
        switch(
          record$df_method,
          kenward = paste0("Kenward", intToUtf8(8211), "Roger"),
          satterthwaite = "Satterthwaite"
        ),
        " approximations."
      )
    )
  }
  if (isTRUE(record$mixed) && !is.null(record$effect_size_header)) {
    sentences <- c(
      sentences,
      paste0(
        record$effect_size_header,
        " is computed from *F* and its degrees of freedom, which only approximates it in mixed models."
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
  sentences <- c(sentences, abbreviation_sentence(record$abbreviations))

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

#' "Principal axis factoring with oblimin rotation."
#' @keywords internal
#' @noRd
fa_method_sentence <- function(record) {
  method <- if (is.null(record$method)) NULL else fa_method_labels[[record$method]]
  if (is.null(method)) {
    # Methods without a known name are given as psych calls them
    method <- paste0("Factor analysis (", record$method, " extraction)")
  }
  rotation <- if (is.null(record$rotation)) "none" else record$rotation
  # A single factor cannot be rotated, so rotation is not worth mentioning
  if (identical(record$nfactors, 1L)) {
    return(paste0(method, "."))
  }
  if (identical(rotation, "none")) {
    return(paste0(method, " without rotation."))
  }
  rotation_label <- fa_rotation_labels[[rotation]]
  if (is.null(rotation_label)) {
    rotation_label <- rotation
  }
  paste0(method, " with ", rotation_label, " rotation.")
}

#' @keywords internal
#' @noRd
fa_method_labels <- list(
  pca = "Principal components analysis",
  pa = "Principal axis factoring",
  minres = "Minimum residual factor analysis",
  uls = "Unweighted least squares factor analysis",
  ols = "Ordinary least squares factor analysis",
  wls = "Weighted least squares factor analysis",
  gls = "Generalized least squares factor analysis",
  ml = "Maximum likelihood factor analysis",
  minchi = "Minimum chi-square factor analysis",
  minrank = "Minimum rank factor analysis",
  alpha = "Alpha factor analysis"
)

#' @keywords internal
#' @noRd
fa_rotation_labels <- list(
  geominT = "geomin (orthogonal)",
  geominQ = "geomin (oblique)",
  bentlerT = "Bentler (orthogonal)",
  bentlerQ = "Bentler (oblique)",
  targetT = "target (orthogonal)",
  targetQ = "target (oblique)",
  TargetT = "target (orthogonal)",
  TargetQ = "target (oblique)",
  Promax = "promax",
  cluster = "cluster",
  biquartimin = "biquartimin"
)

#' @keywords internal
#' @noRd
loadings_note_sentences <- function(record) {
  is_cfa <- identical(record$method, "lavaan")
  is_pca <- identical(record$method, "pca")
  if (is_cfa) {
    sentences <- c(
      cfa_intro_sentences(record),
      "Loadings are fully standardized estimates from a confirmatory factor analysis."
    )
    if (isTRUE(record$fixed_zero)) {
      sentences <- c(sentences, "Blank cells are loadings fixed at 0.")
    }
  } else {
    sentences <- fa_method_sentence(record)
  }

  if (isTRUE(record$oblique) && !is_cfa) {
    sentences <- c(sentences, "Loadings are pattern coefficients.")
  }

  if (!is.null(record$min_loading) && !is.na(record$min_loading)) {
    cutoff <- sub("^0", "", format(record$min_loading, nsmall = 2))
    sentences <- c(
      sentences,
      paste0("Loadings smaller than ", cutoff, " in absolute value are omitted.")
    )
  }

  if (isTRUE(record$bold_max)) {
    sentences <- c(
      sentences,
      "The largest loading for each variable is in bold."
    )
  }

  sentences <- c(sentences, abbreviation_sentence(record$abbreviations))

  if (isTRUE(record$variance) && isTRUE(record$oblique)) {
    sentences <- c(
      sentences,
      paste0(
        "Because the ",
        if (is_pca) "components" else "factors",
        " are correlated, the variance they explain overlaps, and the cumulative proportion is approximate."
      )
    )
  }

  sentences
}

#' @keywords internal
#' @noRd
factor_cor_note_sentences <- function(record) {
  if (!identical(record$method, "lavaan")) {
    return(fa_method_sentence(record))
  }
  c(
    cfa_intro_sentences(record),
    "Correlations among the latent variables are from the fully standardized solution.",
    if (isTRUE(record$structural)) {
      "Correlations between latent variables linked by regression paths are implied by the model."
    }
  )
}

#' Sample size, estimator, and missing-data sentences for lavaan models
#' @keywords internal
#' @noRd
cfa_intro_sentences <- function(record) {
  sentences <- character(0)
  if (length(record$n) == 1 && !is.na(record$n)) {
    sentences <- paste0("*N* = ", format(record$n, big.mark = ","), ".")
  }
  if (length(record$estimator) > 0) {
    sentences <- c(sentences, paste0("Estimation used ", record$estimator, "."))
  }
  if (any(record$missing %in% c("ml", "ml.x", "fiml", "direct"))) {
    sentences <- c(
      sentences,
      "Missing data were handled with full information maximum likelihood."
    )
  }
  sentences
}

#' Sentence about the covariance matrix behind the standard errors
#' @param vcov the `vcov` argument passed to parameters::model_parameters()
#' @keywords internal
#' @noRd
describe_vcov <- function(vcov) {
  if (is.null(vcov)) {
    return(NULL)
  }
  if (!is.character(vcov)) {
    return("Standard errors are from a user-supplied covariance matrix.")
  }
  v <- vcov[1]
  type <- if (grepl("^HC[0-9]", v)) {
    paste0("heteroskedasticity-consistent (", v, ")")
  } else if (v %in% c("vcovHC", "HC")) {
    "heteroskedasticity-consistent"
  } else if (grepl("^CR[0-9]", v)) {
    paste0("cluster-robust (", v, ")")
  } else if (v %in% c("vcovCL", "vcovCR", "CR")) {
    "cluster-robust"
  } else if (v %in% c("vcovHAC", "HAC", "NeweyWest", "vcovPL", "kernHAC")) {
    "heteroskedasticity- and autocorrelation-consistent"
  } else if (v %in% c("vcovBS", "BS", "bootstrap")) {
    "bootstrapped"
  } else {
    paste0("robust (", v, ")")
  }
  paste0("Standard errors are ", type, ".")
}

#' Record what a regression table's note needs
#' @param note list from an apa_parameters method (vcov, df_method,
#'   thresholds)
#' @param headers headers of the columns shown, named by column format name
#' @param abbreviations definitions of the abbreviated headers shown
#' @param starred,bolded whether stars or bolding were applied
#' @keywords internal
#' @noRd
parameters_note_record <- function(
  note,
  headers,
  abbreviations = character(0),
  starred = FALSE,
  bolded = FALSE
) {
  list(
    table = "parameters",
    headers = headers,
    # Symbols of random effects (e.g., *SD* and *r*) are defined, too
    abbreviations = combine_abbreviations(abbreviations, note$random_abbreviations),
    vcov = note$vcov,
    df_method = note$df_method,
    thresholds = note$thresholds,
    components = note$components,
    residual = note$residual,
    starred = isTRUE(starred),
    bolded = isTRUE(bolded),
    performance = NULL,
    performance_starred = FALSE
  )
}

#' Combine the note records of the models in a list of models
#' @keywords internal
#' @noRd
merge_parameters_note_records <- function(
  records,
  performance = NULL,
  performance_starred = FALSE,
  column_formats = NULL
) {
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }
  records <- purrr::compact(records)
  headers <- unlist(unname(purrr::map(records, "headers")))
  headers <- headers[!duplicated(names(headers))]

  pick <- function(field) {
    unique(unlist(purrr::map(records, field)))
  }

  performance_headers <- NULL
  performance_abbreviations <- character(0)
  if (length(performance) > 0) {
    performance <- performance[!is.na(performance)]
    performance_headers <- purrr::map_chr(
      performance,
      \(m) if (is.null(column_formats[[m]])) m else column_formats[[m]]@header
    )
    names(performance_headers) <- performance
    performance_abbreviations <- column_abbreviations(performance, column_formats)
    # In mixed models, R2 is the marginal R2, so it needs a definition
    if (
      all(c("R2", "R2_conditional") %in% performance) &&
        !(performance_headers[["R2"]] %in% names(performance_abbreviations))
    ) {
      performance_abbreviations <- combine_abbreviations(
        stats::setNames(
          "variance explained by fixed effects",
          performance_headers[["R2"]]
        ),
        performance_abbreviations
      )
    }
  }

  list(
    table = "parameters",
    headers = headers,
    abbreviations = combine_abbreviations(
      unlist(unname(purrr::map(records, "abbreviations"))),
      performance_abbreviations
    ),
    vcov = pick("vcov"),
    df_method = pick("df_method"),
    thresholds = pick("thresholds"),
    components = pick("components"),
    residual = pick("residual"),
    starred = any(purrr::map_lgl(records, "starred")),
    bolded = any(purrr::map_lgl(records, "bolded")),
    performance = performance_headers,
    performance_starred = isTRUE(performance_starred)
  )
}

#' @keywords internal
#' @noRd
parameters_note_sentences <- function(record) {
  sentences <- character(0)
  headers <- record$headers

  # Standard errors
  sentences <- c(sentences, record$vcov)

  # Degrees of freedom of mixed-model t tests
  if ("t" %in% names(headers) && length(record$df_method) > 0) {
    df_names <- c(
      kenward = paste0("Kenward", intToUtf8(8211), "Roger"),
      satterthwaite = "Satterthwaite"
    )
    known <- intersect(record$df_method, names(df_names))
    if ("nlme" %in% record$df_method) {
      sentences <- c(
        sentences,
        "Degrees of freedom are computed by nlme from the grouping levels at which each effect varies."
      )
    }
    if (length(known) > 0) {
      sentences <- c(
        sentences,
        paste0(
          "Degrees of freedom are ",
          paste(df_names[known], collapse = " and "),
          " approximations."
        )
      )
    }
  }

  # Ordinal thresholds
  if (length(record$thresholds) > 0) {
    scale_names <- c(
      logit = "logit",
      probit = "probit",
      cloglog = "complementary log-log",
      loglog = "log-log",
      cauchit = "Cauchit"
    )
    link <- record$thresholds[1]
    link_name <- if (link %in% names(scale_names)) scale_names[[link]] else link
    sentences <- c(
      sentences,
      paste0(
        "Thresholds (cutpoints) are on the ",
        link_name,
        " scale and are not tested."
      )
    )
  }

  # Residual correlation structures and variance functions of nlme models
  sentences <- c(sentences, record$residual)

  # Zero-inflation and dispersion parts of glmmTMB models
  if ("zero_inflated" %in% record$components) {
    sentences <- c(
      sentences,
      "Zero-inflation coefficients are log odds of an excess zero (a zero not produced by the conditional model)."
    )
  }
  if ("dispersion" %in% record$components) {
    sentences <- c(
      sentences,
      "Dispersion coefficients are on the log scale."
    )
  }

  # Abbreviations, in column order
  sentences <- c(sentences, abbreviation_sentence(record$abbreviations))

  # Probability notes
  c(
    sentences,
    probability_note_sentences(
      starred = isTRUE(record$starred) || isTRUE(record$performance_starred),
      bolded = record$bolded
    )
  )
}

#' Note text for apa_flextable(note = ...)
#' @param data the data passed to apa_flextable()
#' @param note `NULL`, `FALSE`, `TRUE`, or a character string
#' @returns note text, or NULL for no note
#' @keywords internal
#' @noRd
note_text <- function(data, note) {
  if (is.null(note) || isFALSE(note)) {
    return(NULL)
  }
  if (isTRUE(note)) {
    if (is.null(attr(data, "apa_note", exact = TRUE))) {
      cli::cli_abort(c(
        "{.code note = TRUE} needs a table made by {.fn apa_anova}, {.fn apa_htest}, {.fn apa_parameters}, {.fn apa_loadings}, or {.fn apa_factor_cor}.",
        i = "The information for the note is stored in an attribute that some operations (e.g., {.fn dplyr::bind_rows} and joins) drop.",
        i = "To write your own note, pass it as text (e.g., {.code note = \"Data are from ...\"})."
      ))
    }
    text <- apa_note(data)
  } else if (is.character(note)) {
    text <- paste(note[!is.na(note)], collapse = " ")
  } else {
    cli::cli_abort(
      "{.arg note} must be {.code TRUE}, {.code FALSE}, {.code NULL}, or a character string, not {.obj_type_friendly {note}}."
    )
  }
  if (length(text) == 0 || !nzchar(trimws(text))) {
    return(NULL)
  }
  text
}

#' Add a table note below a flextable, styled like apa_style() footers but
#' single-spaced
#' @keywords internal
#' @noRd
add_note_footer <- function(
  ft,
  text,
  font_family = NULL,
  font_size = 12,
  text_color = "black",
  horizontal_padding = 3
) {
  if (is.null(text)) {
    return(ft)
  }
  if (is.null(font_family)) {
    font_family <- the$font_family
  }
  n_before <- flextable::nrow_part(ft, "footer")
  ft <- flextable::add_footer_lines(
    ft,
    values = ftExtra::as_paragraph_md(paste0("*Note*. ", text))
  )
  i <- n_before + 1
  ft |>
    flextable::font(i = i, part = "footer", fontname = font_family) |>
    flextable::fontsize(i = i, part = "footer", size = font_size) |>
    flextable::color(i = i, part = "footer", color = text_color) |>
    flextable::line_spacing(i = i, part = "footer", space = 1) |>
    flextable::align(i = i, part = "footer", align = "left") |>
    flextable::valign(i = i, part = "footer", valign = "top") |>
    flextable::padding(
      i = i,
      part = "footer",
      padding.top = 2,
      padding.bottom = 0,
      padding.left = horizontal_padding,
      padding.right = horizontal_padding
    )
}
