#' Factor loadings table
#'
#' Makes an APA-style table of factor loadings from an exploratory factor
#' analysis or a principal components analysis.
#'
#' `fit` can be a [psych::fa()] or [psych::principal()] object, or the output
#' of [parameters::factor_analysis()] or [parameters::principal_components()].
#' For the parameters functions, the loadings come from the underlying model,
#' so their `sort` and `threshold` settings do not matter: use `sort_loading`
#' and `min_loading` instead. Unrotated [parameters::principal_components()]
#' output wraps a [stats::prcomp()] model. Its loadings are the correlations
#' between the variables and the components when the variables are
#' standardized (the default, `standardize = TRUE`).
#'
#' Loadings smaller in absolute value than `min_loading` are left blank. With
#' `bold_max = TRUE`, the largest displayed loading in each row is bolded.
#'
#' Rows for variance explained (`variance = TRUE`) are added below the
#' loadings: the sum of squared loadings, the proportion of total variance,
#' and the cumulative proportion (the last only when there is more than one
#' factor). [apa_flextable()] draws a rule above these rows. With an oblique
#' rotation the factors are correlated, so the variance explained by each
#' factor overlaps with the others and the cumulative proportion is only
#' approximate. Report the factor correlations with [apa_factor_cor()].
#'
#' The variable names are formatted (e.g., title case) when
#' `format_variable_names = TRUE`. Either way, [apa_flextable()] leaves the
#' `Variable` column as it is, so item text is never changed again when the
#' table is rendered.
#'
#' @param fit a `psych::fa`, `psych::principal`, `parameters::factor_analysis`, or `parameters::principal_components` object
#' @param sort_loading sort the rows so that variables are grouped by the factor with their largest loading, and by the size of that loading within each factor (as in [psych::fa.sort()])
#' @param min_loading minimum absolute loading to display. Use `NA` or 0 to show all loadings.
#' @param factor_names names for the factors. Either an unnamed character vector in the order the factors appear in the table (psych orders factors by variance explained), or a named vector that maps the original names to new names (e.g., `c(PA1 = "Verbal")`). The unnamed form is safer: after an oblimin rotation, the labels psych assigns (PA1, PA2, ...) can change from one run to the next, but the order of the factors does not.
#' @param variance add rows for the variance explained by each factor
#' @param communality add a communality (*h*^2^) column
#' @param uniqueness add a uniqueness (*u*^2^) column
#' @param complexity add a column with Hofmann's complexity index
#' @param bold_max bold the largest displayed loading (in absolute value) in each row
#' @param format_variable_names format the variable names with the `Variable` column format. Set to `FALSE` to keep item text exactly as it is.
#' @param digits number of decimal places for loadings
#'
#' @returns `apa7_tbl` (tibble)
#' @export
#' @examples
#' fit <- psych::fa(Harman74.cor$cov, 4, fm = "pa", rotate = "varimax")
#' apa_loadings(
#'   fit,
#'   factor_names = c("Verbal", "Spatial", "Speed", "Memory")
#' )
#'
#' # With rows for variance explained and bolded primary loadings
#' apa_loadings(fit, variance = TRUE, communality = TRUE, bold_max = TRUE)
#'
#' # Principal components
#' psych::principal(Harman74.cor$cov, 4) |>
#'   apa_loadings()
#'
#' # parameters output
#' parameters::principal_components(mtcars[, 1:7], n = 2) |>
#'   apa_loadings()
apa_loadings <- function(
  fit,
  sort_loading = TRUE,
  min_loading = 0.2,
  factor_names = NULL,
  variance = FALSE,
  communality = FALSE,
  uniqueness = FALSE,
  complexity = FALSE,
  bold_max = FALSE,
  format_variable_names = TRUE,
  digits = 2
) {
  parts <- fa_parts(fit)
  L <- parts$loadings
  factor_columns <- colnames(L)
  new_names <- fa_factor_names(factor_columns, factor_names)

  ord <- seq_len(nrow(L))
  if (sort_loading) {
    ord <- fa_sort_order(L)
  }
  L <- L[ord, , drop = FALSE]
  u2 <- parts$uniqueness[ord]

  show <- matrix(TRUE, nrow(L), ncol(L))
  if (!is.na(min_loading) && min_loading > 0) {
    show <- abs(L) >= min_loading
  }

  loadings_chr <- vapply(
    seq_len(ncol(L)),
    \(j) {
      v <- align_chr(L[, j], accuracy = .1^digits, trim_leading_zeros = TRUE)
      v[!show[, j]] <- ""
      v
    },
    character(nrow(L))
  ) |>
    matrix(nrow = nrow(L))

  if (bold_max) {
    is_max <- abs(L) == apply(abs(L), 1, max) & show
    loadings_chr[is_max] <- bold_padded(loadings_chr[is_max])
  }

  colnames(loadings_chr) <- factor_columns

  variable <- rownames(L)
  if (format_variable_names) {
    variable <- the$column_formats$Variable@formatter(variable)
  }

  d <- tibble::tibble(Variable = variable) |>
    dplyr::bind_cols(tibble::as_tibble(loadings_chr))

  if (communality) {
    d[["*h*^2^"]] <- align_chr(
      1 - u2,
      accuracy = .1^digits,
      trim_leading_zeros = TRUE
    )
  }

  if (uniqueness) {
    d[["*u*^2^"]] <- align_chr(
      u2,
      accuracy = .1^digits,
      trim_leading_zeros = TRUE
    )
  }

  if (complexity) {
    d[["Complexity"]] <- align_chr(
      rowSums(L^2)^2 / rowSums(L^4),
      accuracy = .1^digits
    )
  }

  if (variance) {
    d <- dplyr::bind_rows(d, fa_variance_rows(parts, digits))
    d <- dplyr::mutate(
      d,
      dplyr::across(dplyr::everything(), \(v) ifelse(is.na(v), "", v))
    )
    attr(d, "rule_above") <- "Sum of squared loadings"
  }

  colnames(d)[match(factor_columns, colnames(d))] <- new_names

  attr(d, "no_format_columns") <- "Variable"
  as_apa7_tbl(d)
}

#' Factor correlation table
#'
#' Makes an APA-style table of the correlations among factors (or
#' components) from an obliquely rotated solution: a [psych::fa()] or
#' [psych::principal()] object, or the output of
#' [parameters::factor_analysis()] or [parameters::principal_components()].
#'
#' The factors are numbered in the first column, and the numbers are used as
#' column headers. Only the lower triangle is shown. The factors are in the
#' same order as in [apa_loadings()].
#'
#' @inheritParams apa_loadings
#' @param digits number of decimal places
#'
#' @returns `apa7_tbl` (tibble)
#' @export
#' @examples
#' fit <- psych::fa(Harman74.cor$cov, 4, fm = "pa", rotate = "oblimin")
#' apa_factor_cor(
#'   fit,
#'   factor_names = c("Verbal", "Spatial", "Memory", "Speed")
#' )
apa_factor_cor <- function(
  fit,
  factor_names = NULL,
  digits = 2
) {
  parts <- fa_parts(fit)
  factor_columns <- colnames(parts$loadings)
  k <- length(factor_columns)

  Phi <- parts$Phi
  if (is.null(Phi)) {
    if (k == 1) {
      cli::cli_abort("A one-factor solution has no factor correlations.")
    }
    cli::cli_abort(c(
      "{.arg fit} has no factor correlations.",
      i = "Factors from an orthogonal rotation (here, {.val {parts$rotation}}) are uncorrelated. Use an oblique rotation such as {.val oblimin} or {.val promax}."
    ))
  }

  new_names <- fa_factor_names(factor_columns, factor_names)
  Phi <- Phi[factor_columns, factor_columns, drop = FALSE]

  R <- matrix(
    align_chr(c(Phi), accuracy = .1^digits, trim_leading_zeros = TRUE),
    nrow = k
  )
  R[upper.tri(R)] <- ""
  diag(R) <- intToUtf8(8212)
  colnames(R) <- as.character(seq_len(k))

  d <- tibble::tibble(Factor = paste0(seq_len(k), ". ", new_names)) |>
    dplyr::bind_cols(tibble::as_tibble(R))

  attr(d, "no_format_columns") <- "Factor"
  as_apa7_tbl(d)
}

#' Extract what the loadings tables need from a supported fit object
#'
#' @returns list with `loadings` (matrix, variables by factors, in the model's
#'   factor order), `uniqueness`, `ss` and `prop` (sum of squared loadings and
#'   proportion of variance per factor), `Phi` (factor correlations or NULL),
#'   and `rotation`
#' @keywords internal
#' @noRd
fa_parts <- function(fit) {
  if (inherits(fit, c("parameters_efa", "parameters_pca"))) {
    model <- attr(fit, "model")
    if (inherits(model, "psych")) {
      return(fa_parts(model))
    }
    if (inherits(model, "prcomp")) {
      n <- length(attr(fit, "loadings_columns"))
      L <- model$rotation[, seq_len(n), drop = FALSE] %*%
        diag(model$sdev[seq_len(n)], nrow = n)
      colnames(L) <- colnames(model$rotation)[seq_len(n)]
      ss <- colSums(L^2)
      return(list(
        loadings = L,
        uniqueness = 1 - rowSums(L^2),
        ss = ss,
        prop = ss / nrow(L),
        Phi = NULL,
        rotation = "none"
      ))
    }
    cli::cli_abort( # nocov start
      "Cannot find the model behind {.arg fit} (a {.cls {class(model)}} object)."
    ) # nocov end
  }

  if (!(inherits(fit, "psych") && inherits(fit, c("fa", "principal")))) {
    cli::cli_abort(
      "{.arg fit} must be a {.fn psych::fa}, {.fn psych::principal}, {.fn parameters::factor_analysis}, or {.fn parameters::principal_components} object, not {.obj_type_friendly {fit}}."
    )
  }

  L <- unclass(fit$loadings)
  attr(L, "class") <- NULL
  V <- fit$Vaccounted
  if (is.null(V)) {
    # nocov start
    ss <- colSums(L^2)
    V <- rbind(`SS loadings` = ss, `Proportion Var` = ss / nrow(L))
    # nocov end
  }
  V <- V[, colnames(L), drop = FALSE]

  list(
    loadings = L,
    uniqueness = fit$uniquenesses,
    ss = V["SS loadings", ],
    prop = V["Proportion Var", ],
    Phi = fit$Phi,
    rotation = fit$rotation
  )
}

#' Row order grouping variables by their largest loading (as psych::fa.sort)
#' @keywords internal
#' @noRd
fa_sort_order <- function(L) {
  if (ncol(L) == 1) {
    return(order(-abs(L[, 1])))
  }
  primary <- apply(abs(L), 1, which.max)
  primary_loading <- abs(L)[cbind(seq_len(nrow(L)), primary)]
  order(primary, -primary_loading)
}

#' Bold aligned strings, keeping the padding outside the markdown
#' @keywords internal
#' @noRd
bold_padded <- function(x) {
  pad <- regmatches(
    x,
    regexpr(paste0("^[ ", intToUtf8(8199), "]*"), x)
  )
  paste0(pad, "**", substring(x, nchar(pad) + 1), "**")
}

#' @keywords internal
#' @noRd
fa_factor_names <- function(factor_columns, factor_names = NULL) {
  if (is.null(factor_names)) {
    return(factor_columns)
  }

  if (is.null(names(factor_names))) {
    if (length(factor_names) != length(factor_columns)) {
      cli::cli_abort(
        "{.arg factor_names} has {length(factor_names)} name{?s}, but the solution has {length(factor_columns)} factor{?s}."
      )
    }
    return(as.character(factor_names))
  }

  unknown <- setdiff(names(factor_names), factor_columns)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown factor{?s} in {.arg factor_names}: {.val {unknown}}.",
      i = "The factors are {.val {factor_columns}}."
    ))
  }

  new_names <- factor_columns
  new_names[match(names(factor_names), factor_columns)] <- factor_names
  new_names
}

#' @keywords internal
#' @noRd
fa_variance_rows <- function(parts, digits = 2) {
  factor_columns <- colnames(parts$loadings)

  rows <- list(
    `Sum of squared loadings` = align_chr(parts$ss, accuracy = .1^digits),
    `Proportion of variance` = align_chr(
      parts$prop,
      accuracy = .1^digits,
      trim_leading_zeros = TRUE
    )
  )

  if (length(factor_columns) > 1) {
    rows[["Cumulative proportion"]] <- align_chr(
      cumsum(parts$prop),
      accuracy = .1^digits,
      trim_leading_zeros = TRUE
    )
  }

  d <- do.call(rbind, rows)
  colnames(d) <- factor_columns
  tibble::tibble(Variable = names(rows)) |>
    dplyr::bind_cols(tibble::as_tibble(d))
}

#' @keywords internal
#' @noRd
add_rule_above <- function(ft, rows, border_color = "black", border_width = 0.5) {
  if (length(rows) == 0) {
    return(ft)
  }
  first_column <- as.character(ft$body$dataset[[ft$col_keys[1]]])
  i <- which(first_column %in% rows)
  i <- i[i > 1]
  if (length(i) == 0) {
    return(ft)
  }
  flextable::hline(
    ft,
    i = i[1] - 1,
    border = flextable::fp_border_default(
      color = border_color,
      width = border_width
    ),
    part = "body"
  )
}
