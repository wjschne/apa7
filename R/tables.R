#' Add break columns
#'
#' @param d data.frame or tibble
#' @param ... Column name or tidyselect function. Select columns
#' @param .before insert break columns before selected columns (defaults to FALSE)
#' @param omit_first omit the first break column
#' @param omit_last omit the last break column
#'
#' @return data.frame or tibble
#' @export
#'
#' @examples
#' d <- data.frame(x_n = 3, x_mean = 4,
#'             y_n = 5, y_mean = 6,
#'             z_n = 4, z_mean = 4)
#' # Unquoted variable names
#' add_break_columns(d, x_mean)
#'
#' # Character vector
#' add_break_columns(d, c("y_n", "z_n"),  .before = TRUE)
#'
#' # Tidyselect function (contains, starts_with, ends_with,
#' # matches, num_range, all_of, any_of)
#' # Insert columns after all columns
#' # ending with "_mean" except the last instance
#' add_break_columns(d,
#'                   dplyr::ends_with("_mean"),
#'                   omit_last = TRUE)
add_break_columns <- function(
  d,
  ...,
  .before = FALSE,
  omit_first = FALSE,
  omit_last = FALSE
) {
  .dots <- rlang::exprs(...)

  if (all(purrr::map_lgl(as.list(.dots), is.character))) {
    where_names <- colnames(d)[colnames(d) %in% unlist(.dots)]
  } else {
    v <- deparse(substitute(...))
    if (all(v %in% colnames(d))) {
      where_names <- v
    } else {
      where_names <- colnames(dplyr::select(d, ...))
    }
  }

  if (omit_first && length(where_names) > 0) {
    where_names <- where_names[-1]
  }

  if (omit_last && length(where_names) > 0) {
    where_names <- where_names[-length(where_names)]
  }

  if (length(where_names) == 0) {
    return(d)
  }

  blank_names <- paste0("apa7breakcolumn", seq_along(where_names))

  where_numbers <- seq(1, ncol(d))[colnames(d) %in% where_names]
  shifter <- 1L - .before * 1L
  keys <- insert_at(
    colnames(d),
    ats = where_numbers + shifter,
    values = blank_names
  )
  d[, blank_names] <- NA
  d[, keys]
}

#' Insert values into a vector
#'
#' Like `R.utils::insert()`: each value is inserted before the element at the
#' matching position of the original vector (`length(x) + 1` appends).
#' @param x vector
#' @param ats positions in `x`, one per value
#' @param values values to insert
#' @keywords internal
#' @noRd
insert_at <- function(x, ats, values) {
  c(x, values)[order(c(seq_along(x), ats - 0.5))]
}

#' Make a column into a list column
#'
#' @param data data.frame or tibble
#' @param ... Column name or tidyselect function. Select columns. Default is first column
#' @param type list type. Can be "1" (numeric), "a" (lowercase alphabetical), or "ABC" (uppercase alphabetical), "i" (lowercase Roman numerals), "I" (uppercase Roman numerals)
#' @param sep separator
#' @param merge If `TRUE`, list columns will be united with their respective text columns.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' d <- data.frame(x = letters[1:5], y = letters[2:6])
#' # default is first column
#' add_list_column(d) |>
#'   apa_flextable()
#' # select any column
#' add_list_column(d, y) |>
#'   apa_flextable()
#' add_list_column(d, type = "a", sep = ") ") |>
#'  apa_flextable()
#' add_list_column(d, merge = TRUE)
add_list_column <- function(
  data,
  ...,
  type = c("1", "a", "A", "I", "i"),
  sep = ".\u00A0",
  merge = FALSE
) {
  type <- match.arg(type)
  # l <- rlang::list2(...)
  nn <- colnames(dplyr::select(data, ...))
  if (length(nn) == 0 && length(colnames(data))) {
    nn <- colnames(data)[1]
  }
  k <- nrow(data)
  vseq <- seq_len(k)

  if (type == "1") {
    vl <- paste0(vseq, sep)
  } else if (tolower(type) == "i") {
    rmn <- utils::as.roman(vseq)
    if (type == "i") {
      rmn <- tolower(rmn)
    }
    vl <- paste0(rmn, sep, recycle0 = TRUE)
  } else {
    vl <- paste0(rep_letters(vseq, type = type), sep, recycle0 = TRUE)
  }

  for (n in nn) {
    vn <- paste0(n, "apa7listcolumn")
    if (merge) {
      data[n] <- paste0(vl, data[[n]])
    } else {
      data[vn] <- vl
      data <- dplyr::relocate(
        data,
        dplyr::all_of(vn),
        .before = dplyr::all_of(n)
      )
    }
  }
  data
}

#' @noRd
#' @keywords internal
rep_letters <- function(i, type = c("a", "A")) {
  # https://stackoverflow.com/a/25881167/4513316
  if (any(i < 1)) {
    return(character(0))
  }
  type <- match.arg(type)
  if (type == "a") {
    letts <- letters
  } else {
    letts <- LETTERS
  }
  base10toA <- function(n, A) {
    stopifnot(n >= 0L)
    N <- length(A)
    j <- n %/% N
    if (j == 0L) A[n + 1L] else paste0(Recall(j - 1L, A), A[n %% N + 1L])
  }
  vapply(i - 1L, base10toA, character(1L), letts)
}

#' Adds stars next to a column based on p-values
#'
#' @param data data.frame or tibble
#' @param ... Column name or tidyselect function. Select columns
#' @param p Column name or tidyselect function. Select p-value column name
#' @param merge merge and balance columns (default: `FALSE`)
#' @param star text for making stars
#' @inheritParams p2stars
#'
#' @return data.frame
#' @export
#'
#' @examples
#' data.frame(b = c(1.4,2.2),
#'            p = c(.54, .02)) |>
#'  add_star_column(b, p)
add_star_column <- function(
  data,
  ...,
  p = "p",
  merge = FALSE,
  superscript = TRUE,
  star = "\\*",
  alpha = c(0.05, .01, .001),
  first_alpha_marginal = FALSE,
  add_trailing_space = FALSE,
  prefix = "\\"
) {
  p_names <- colnames(dplyr::select(data, {{ p }}))
  where_names <- colnames(dplyr::select(data, ...))

  if (length(p_names) == 0) {
    stop("There is no column of that name supplied to p") # nocov
  }

  if (length(where_names) == 0) {
    stop("There is no column of that name supplied to col") # nocov
  }

  star_names <- paste0(where_names, "apa7starcolumn")

  where_numbers <- seq(1, ncol(data))[colnames(data) %in% where_names]
  keys <- insert_at(
    colnames(data),
    ats = where_numbers + 1,
    values = star_names
  )
  p_stars <- p2stars(
    data[[p_names]],
    first_alpha_marginal = first_alpha_marginal,
    superscript = superscript,
    alpha = alpha,
    add_trailing_space = add_trailing_space,
    prefix = prefix
  )

  for (wn in where_names) {
    sn <- paste0(wn, "apa7starcolumn")
    data[, sn] <- ifelse(data[[wn]] == "" | is.na(data[[wn]]), "", p_stars)
  }

  data <- data[, keys]

  if (merge) {
    for (wn in where_names) {
      sn <- paste0(wn, "apa7starcolumn")
      data[, wn] <- star_balance(
        paste0(data[[wn]], data[[sn]])
      )
      data[sn] <- NULL
    }
  }
  data
}

#' Add columns that separate significance stars from numbers
#'
#' @param data data.frame or tibble
#' @param ... Column name or tidyselect function. Select columns
#' @param superscript make stars superscript
#' @param star character to use for stars (default: "\\*")
#' @param star_replace character to replace stars with (default: "\\\\*")
#'
#' @return data.frame or tibble
#' @export
#'
#' @examples
#' tibble::tibble(x = c(".45", ".58*", ".68**"),
#'                y = c(1,2,3),
#'                z = 4:6) |>
#'                separate_star_column(x)
separate_star_column <- function(
  data,
  ...,
  superscript = TRUE,
  star = "\\*",
  star_replace = "\\\\*"
) {
  .dots <- rlang::expr(...)
  apa7rownumber <- apa7starvalue <- apa7starname <- apa7starcolumn <- NULL

  if (missing(.dots)) {
    .dots <- colnames(data)
  }
  ss <- ifelse(superscript, "^", "")

  d <- data |>
    dplyr::mutate(dplyr::across({{ .dots }}, as.character)) |>
    dplyr::mutate(apa7rownumber = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = c({{ .dots }}, -apa7rownumber),
      names_to = "apa7starname",
      values_to = "apa7starvalue"
    ) |>
    dplyr::mutate(
      apa7starcolumn = stringr::str_extract(
        apa7starvalue,
        paste0("[", star, "]+")
      ) |>
        stringr::str_replace_all(star, star_replace),
      apa7starvalue = stringr::str_remove_all(apa7starvalue, star)
    ) |>
    dplyr::mutate(
      apa7starcolumn = ifelse(
        is.na(apa7starcolumn),
        apa7starcolumn,
        paste0(ss, apa7starcolumn, ss)
      )
    ) |>
    tidyr::pivot_wider(
      names_from = apa7starname,
      values_from = c(apa7starvalue, apa7starcolumn),
      names_glue = "{apa7starname}_{.value}",
      names_vary = "slowest"
    ) |>
    dplyr::select(-apa7rownumber) |>
    dplyr::rename_with(\(.x) stringr::str_remove(.x, "_apa7starvalue")) |>
    dplyr::rename_with(
      \(.x) stringr::str_replace(.x, "_apa7starcolumn", "apa7starcolumn")
    )

  # names of star columns
  star_keys <- d |>
    dplyr::select(dplyr::ends_with("apa7starcolumn")) |>
    colnames()

  # names of  variable with accompanying star columns
  r_keys <- stringr::str_remove(star_keys, "apa7starcolumn$")

  # all columns
  c_names <- colnames(data)

  # reordered columns with star columns
  c_names_reordered <- purrr::map(c_names, \(cn) {
    if (cn %in% r_keys) {
      paste0(cn, c("", "apa7starcolumn"))
    } else {
      cn
    }
  }) |>
    unlist()

  # Return reordered columns
  dplyr::select(d, dplyr::all_of(c_names_reordered))
}

#' Make contingency table with chi-square test of independence
#'
#' @param data A two-column data.frame or tibble
#' @param note Custom note (overrides automatic note.)
#' @inheritParams apa_flextable
#' @param suppress_warnings Suppress any warnings if true.
#'
#' @return flextable::flextable
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' apa_chisq(mtcars[, c("am", "gear")])
apa_chisq <- function(
  data,
  note = NULL,
  row_title_column = NULL,
  row_title_prefix = "",
  row_title_sep = " ",
  row_title_align = "center",
  row_title_border = list(
    color = "gray20",
    style = "solid",
    width = 1
  ),
  left_column_padding = 20,
  cwidth = .75,
  cheight = .25,
  separate_headers = TRUE,
  apa_style = TRUE,
  font_family = NULL,
  font_size = 12,
  text_color = "black",
  border_color = "black",
  border_width = 0.5,
  line_spacing = 2,
  horizontal_padding = 3,
  table_align = "left",
  layout = "autofit",
  table_width = 1,
  markdown = TRUE,
  markdown_header = markdown,
  markdown_body = markdown,
  auto_format_columns = TRUE,
  column_formats = NULL,
  pretty_widths = TRUE,
  suppress_warnings = TRUE,
  ...
) {
  if (is.null(font_family)) {
    font_family <- the$font_family
  }
  if (!inherits(data, "data.frame")) {
    stop("data must be a data.frame or tibble.")
  }
  if (ncol(data) != 2) {
    stop(
      'data must have 2 columns. Select the 2 variables you wish to test before passing them to this function. For example:\nx <- mtcars[, c("am", "cyl")]\nor\nx <- dplyr::select(mtcars, am, cyl)'
    )
  }

  tbl <- table(data)

  if (nrow(tbl) < 2 || ncol(tbl) < 2) {
    stop("The contingency table must be at least a 2 by 2 table.")
  }

  if (suppress_warnings) {
    fit <- suppressWarnings(stats::chisq.test(tbl))
  } else {
    fit <- stats::chisq.test(tbl)
  }

  ef <- effectsize::effectsize(fit, type = "cramers_v")

  dd <- tbl |>
    tibble::as_tibble() |>
    dplyr::mutate(
      `%` = scales::percent(.data$n / sum(.data$n), accuracy = .1),
      .by = dplyr::all_of(colnames(data)[1]),
      n = as.character(.data$n)
    ) |>
    tidyr::pivot_longer(c("n", "%")) |>
    tidyr::unite("mycols", dplyr::all_of(colnames(data)[1]), "name") |>
    tidyr::pivot_wider(names_from = "mycols", values_from = "value")
  col_keys <- colnames(dd)
  dd <- dd |>
    add_break_columns(
      dplyr::ends_with("_n"),
      .before = TRUE,
      omit_first = TRUE
    ) |>
    apa_flextable(
      row_title_column = row_title_column,
      row_title_prefix = row_title_prefix,
      row_title_sep = row_title_sep,
      row_title_align = row_title_align,
      row_title_border = row_title_border,
      left_column_padding = left_column_padding,
      col_keys = col_keys,
      cwidth = cwidth,
      cheight = cheight,
      separate_headers = separate_headers,
      apa_style = apa_style,
      font_family = font_family,
      font_size = font_size,
      text_color = text_color,
      border_color = border_color,
      border_width = border_width,
      line_spacing = line_spacing,
      horizontal_padding = horizontal_padding,
      table_align = table_align,
      layout = layout,
      table_width = table_width,
      markdown = markdown,
      markdown_header = markdown_header,
      markdown_body = markdown_body,
      auto_format_columns = auto_format_columns,
      column_formats = column_formats,
      pretty_widths = pretty_widths
    )

  if (!is.null(note)) {
    if (!is.na(note)) {
      dd <- dd |>
        flextable::add_footer_lines(
          ftExtra::as_paragraph_md(paste0("*Note*. ", note))
        )
    }
  } else {
    dd <- dd |>
      flextable::add_footer_lines(
        ftExtra::as_paragraph_md(
          paste0(
            "*Note*. *&chi;*^2&thinsp;^(",
            fit$parameter,
            ") = ",
            scales::number(fit$statistic, accuracy = .01),
            ", ",
            apa_p(fit$p.value, inline = TRUE),
            ", Adj. Cramer's *V* = ",
            signs::signs(
              ef$Cramers_v_adjusted,
              accuracy = .01,
              trim_leading_zeros = TRUE
            )
          )
        )
      )
  }
  dd |>
    flextable::align(part = "footer")
}

#' APA-formatted correlation table
#'
#' @param data data.frame or tibble with variables to be correlated, or the
#'   output of [correlation::correlation()] (see Details)
#' @param note Custom note to appear below table. (Overrides automatic note.)
#' @param p_value p-value needed to be flagged as significant
#' @param digits Number of digits for rounding
#' @param bold_significant bold significant correlations
#' @param star_significant start significant correlations
#' @param significance_note If TRUE, place note at bottom of table that significant correlations are bolded.
#' @param output output type. Can be "flextable" or "tibble"
#' @param summary_functions Any named list of functions that summarize data columns (e.g., `list(Mean = mean, SD = sd)`). Can also be a character vector of function names (e.g., `c("n", "M", "SD")`). Functions available in a character vector: IQR, Interquartile Range, Kurtosis, MAD, Median Absolute Deviation, M, Mean, Med, Median, n, N, Quantile, Range, SD, Skewness, Var, Variance
#' @param keep_empty_star_columns Keep remove empty star columns (Default: `TRUE`)
#' @param column_formats column_formats object
#' @inheritParams apa_style
#' @param ... <[`data-masking`][rlang::args_data_masking]> parameters passed to psych::corTest (not used for [correlation::correlation()] output)
#' @details
#' By default, `apa_cor()` computes Pearson correlations with
#' [psych::corTest()], and the stars are for unadjusted *p*-values.
#'
#' For other kinds of correlations (e.g., Spearman, Kendall, robust, or
#' partial correlations), compute them with [correlation::correlation()] and
#' pass the result as `data`. The table shows its correlations and
#' *p*-values, and the summary columns are computed from the data that
#' [correlation::correlation()] keeps with its results. The note says what
#' kind of correlations are shown (unless they are ordinary Pearson
#' correlations) and how the *p*-values were adjusted for multiple
#' comparisons: [correlation::correlation()] uses Holm's method by default,
#' so the stars are for adjusted *p*-values unless `p_adjust = "none"`.
#' Correlations computed by group, between two sets of variables, or with
#' Bayesian methods are not yet supported.
#' @importFrom rlang .data
#'
#' @return flextable::flextable
#' @export
#'
#' @examples
#' apa_cor(mtcars[, c("mpg", "am", "gear", "carb")])
#' apa_cor(mtcars[, c("mpg", "am", "gear", "carb")],
#'         output = "tibble",
#'         star_significant = FALSE)
#'
#' # Spearman correlations from the correlation package
#' if (requireNamespace("correlation", quietly = TRUE)) {
#'   correlation::correlation(trees, method = "spearman") |>
#'     apa_cor()
#' }
apa_cor <- function(
  data,
  note = NULL,
  p_value = c(.05, .01, .001),
  digits = 2,
  bold_significant = FALSE,
  star_significant = TRUE,
  significance_note = TRUE,
  output = c("flextable", "tibble"),
  font_family = NULL,
  font_size = 12,
  text_color = "black",
  border_color = "black",
  border_width = 0.5,
  line_spacing = 2,
  table_width = 6.5,
  keep_empty_star_columns = TRUE,
  summary_functions = list(M = mean, SD = stats::sd),
  column_formats = NULL,
  ...
) {
  value <- name <- Variable <- k <- group <- width <- NULL
  if (is.null(font_family)) {
    font_family <- the$font_family
  }
  output <- match.arg(output)
  if (is.null(column_formats)) {
    column_formats <- column_formats()
  }

  if (bold_significant) {
    star_significant <- FALSE
  }

  # Correlations computed by correlation::correlation()
  cor_sentences <- character(0)
  if (inherits(data, "easycorrelation")) {
    parts <- correlation_parts(data)
    ct <- parts$ct
    # apa_cor() writes its note itself, so citations are resolved here
    cor_sentences <- resolve_citations(parts$sentences, citations_mode())
    data <- parts$data
    if (is.null(data)) {
      # Without the data, there is nothing to summarize
      summary_functions <- NULL
      data <- tibble::as_tibble(
        stats::setNames(rep(list(numeric(0)), length(parts$vars)), parts$vars)
      )
    }
  } else {
    ct <- psych::corTest(data, ...) |>
      suppressWarnings()
  }
  v_names <- colnames(data)
  v_seq <- seq_len(ncol(data))

  R <- apply(
    X = ct$r,
    MARGIN = 2,
    FUN = signs::signs,
    accuracy = .1^digits,
    trim_leading_zeros = TRUE
  )

  if (bold_significant) {
    p <- ifelse(c(ct$p) <= p_value[1], "**", "")
    R <- matrix(paste0(p, c(R), p), nrow = length(v_names))
    colnames(R) <- v_names
    rownames(R) <- v_names
  }

  R[upper.tri(R)] <- NA
  diag(R) <- "\u2014"

  R <- tibble::as_tibble(R)

  star_names <- paste0(v_names, "apa7starcolumn")
  remove_star_columns <- star_names

  if (star_significant && !bold_significant) {
    my_p <- c(ct$p)
    p <- character(length = length(my_p))

    p_note <- character(length(p_value))
    ii <- 0
    for (pv in p_value) {
      ii <- ii + 1
      p_note[ii] <- paste0(
        "^",
        paste0(rep("\\*", ii), collapse = ""),
        "\u00A0^*p*&nbsp;<&nbsp;",
        stringr::str_remove(as.character(pv), "^0")
      )

      p[my_p <= pv] <- paste0(p[my_p <= pv], "\\*")
    }

    p[nchar(p) > 0] <- paste0("^", p[nchar(p) > 0], "^")

    Rstar <- matrix(p, nrow = length(v_names))

    colnames(Rstar) <- star_names
    rownames(Rstar) <- star_names

    Rstar[upper.tri(Rstar, diag = TRUE)] <- ""
    Rstar <- tibble::as_tibble(Rstar)

    k_p <- Rstar |>
      tidyr::pivot_longer(dplyr::everything()) |>
      dplyr::mutate(k = stringr::str_count(value, "\\*")) |>
      dplyr::arrange(k) |>
      dplyr::pull(k) |>
      unique()

    remove_star_columns <- Rstar |>
      dplyr::summarise(
        dplyr::across(dplyr::everything(), \(x) max(nchar(x)))
      ) |>
      tidyr::pivot_longer(dplyr::everything()) |>
      dplyr::filter(value == 0) |>
      dplyr::pull(name)

    if (length(star_names) != length(remove_star_columns)) {
      if (keep_empty_star_columns) {
        remove_star_columns <- ""
      }
    }
  } else {
    kP <- ct$p
    kP[upper.tri(kP)] <- NA
    k_p <- c(kP)
    k_p <- k_p[!is.na(k_p)]
    k_p <- k_p[k_p <= p_value[1]]
    k_p <- (length(k_p) > 0) * 1
  }

  if (length(summary_functions) > 0 && !all(is.na(summary_functions))) {
    if (is.character(summary_functions)) {
      summary_functions <- the$summary_functions[summary_functions]
    }
    d_msd <- tibble::enframe(summary_functions) |>
      dplyr::mutate(
        value = purrr::map(.data$value, \(fn) {
          new_fn <- purrr::possibly(\(x) fn(x, na.rm = TRUE))

          safe_fn <- function(x) {
            res <- new_fn(x)
            if (is.null(res)) {
              res <- fn(x) # nocov
            }
            res
          }

          data |>
            dplyr::summarise(
              dplyr::across(dplyr::everything(), safe_fn)
            )
        })
      ) |>
      tidyr::pivot_wider() |>
      dplyr::reframe(
        dplyr::across(
          dplyr::everything(),
          unlist
        )
      ) |>
      dplyr::mutate(Variable = v_names) |>
      dplyr::select(Variable, dplyr::everything()) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.double),
          \(x) signs::signs(x, accuracy = .1^digits)
        )
      ) |>
      dplyr::mutate(
        apa7listcolumn = paste0(
          dplyr::row_number(),
          ".\u2009"
        ),
        .before = Variable
      )
  } else {
    d_msd <- tibble::tibble(Variable = v_names) |>
      dplyr::mutate(
        apa7listcolumn = paste0(
          dplyr::row_number(),
          ".\u2009"
        ),
        .before = Variable
      )
  }

  colnames(R) <- v_seq

  if (star_significant && !bold_significant) {
    v_names_star <- rbind(
      v_seq,
      paste0(v_names, "apa7starcolumn")
    ) |>
      c()

    R <- cbind(R, Rstar) |>
      dplyr::select(dplyr::all_of(v_names_star))

    if (!keep_empty_star_columns && nchar(remove_star_columns) > 0) {
      R <- R |>
        dplyr::select(-dplyr::all_of(remove_star_columns))
    }
  }

  star_names <- star_names[!(star_names %in% remove_star_columns)]

  d_R <- dplyr::bind_cols(d_msd, R)

  attr(d_R, "cortest") <- ct

  if (output == "flextable") {
    cf <- column_formats()
    cf$Variable <- NULL
    d_R <- d_R |>
      apa_format_columns(column_formats = cf)
    d_tbl <- d_R

    d <- apa_flextable(
      d_R,
      font_family = font_family,
      font_size = font_size,
      text_color = text_color,
      border_color = border_color,
      border_width = border_width,
      line_spacing = line_spacing,
      auto_format_columns = FALSE
    )

    if (is.null(note)) {
      show_significance <- significance_note &&
        (bold_significant || star_significant) &&
        length(k_p) > 0
      cutoff <- formatC(p_value[1], digits = 2, format = "fg") |>
        gsub(pattern = "^0", replacement = "")
      # General note (e.g., the kind of correlation and the p-value
      # adjustment from correlation::correlation()), then probability notes
      paragraphs <- list()
      general <- if (length(cor_sentences) > 0) paste(cor_sentences, collapse = " ")
      if (bold_significant && show_significance) {
        if (is.null(general)) {
          paragraphs <- list(flextable::as_paragraph(
            flextable::as_i("Note. "),
            "Correlations significant at ",
            flextable::as_i("p"),
            " < ",
            cutoff,
            " are ",
            flextable::as_b("bolded"),
            "."
          ))
        } else {
          paragraphs <- list(ftExtra::as_paragraph_md(paste0(
            "*Note*. ", general,
            " Correlations significant at *p* < ", cutoff, " are **bolded**."
          )))
        }
      } else {
        if (!is.null(general)) {
          paragraphs <- c(paragraphs, list(ftExtra::as_paragraph_md(paste0("*Note*. ", general))))
        }
        if (show_significance && any(k_p > 0)) {
          paragraphs <- c(paragraphs, list(ftExtra::as_paragraph_md(
            paste0(p_note[k_p], collapse = ". ")
          )))
        }
      }

      if (length(paragraphs) > 0) {
        for (paragraph in paragraphs) {
          d <- flextable::add_footer_lines(d, values = paragraph)
        }
        d <- d |>
          flextable::font(
            part = "footer",
            fontname = font_family
          ) |>
          flextable::fontsize(
            part = "footer",
            size = font_size
          ) |>
          flextable::line_spacing(
            part = "footer",
            space = 1
          ) |>
          flextable::color(
            part = "footer",
            color = text_color
          )
      }
    } else {
      d <- flextable::add_footer_lines(
        x = d,
        values = ftExtra::as_paragraph_md(
          paste0("*Note*. ", note)
        )
      ) |>
        flextable::font(part = "footer", fontname = font_family) |>
        flextable::fontsize(part = "footer", size = font_size) |>
        flextable::line_spacing(part = "footer", space = 1) |>
        flextable::color(part = "footer", color = text_color)
    }

    # Summary columns (e.g., M and SD) hold plain numbers padded with figure
    # spaces to equal width, so centering them keeps the decimals aligned.
    # apa_flextable() instead left-aligns padded columns with a left padding
    # computed from the column width, which is off once the widths change
    # below, and in HTML, where browsers size columns to their contents.
    summary_keys <- setdiff(
      intersect(colnames(d_tbl), d$col_keys),
      c("apa7listcolumn", "Variable", as.character(v_seq), star_names)
    )
    if (length(summary_keys) > 0) {
      d <- d |>
        flextable::align(j = summary_keys, align = "center", part = "body") |>
        flextable::padding(
          j = summary_keys,
          padding.left = 3,
          padding.right = 3,
          part = "body"
        )
    }

    d_R <- d |>
      flextable::padding(
        j = "apa7listcolumn",
        padding.right = 0,
        padding.left = 0
      ) |>
      flextable::padding(j = "Variable", padding.left = 0) |>
      flextable::align(align = "left", j = "Variable", part = "body") |>
      flextable::align(align = "left", part = "footer") |>
      flextable::align(align = "right", j = "apa7listcolumn") |>
      flextable::set_header_labels(apa7listcolumn = "Variable") |>
      flextable::merge_h_range(
        j1 = "apa7listcolumn",
        j2 = "Variable",
        part = "header"
      ) |>
      flextable::autofit(add_w = 0, add_h = 0) |>
      pretty_widths()

    w <- d_R$body$colwidths |>
      tibble::enframe() |>
      dplyr::mutate(
        group = dplyr::case_when(
          !is.na(suppressWarnings(as.integer(name))) ~ "r",
          stringr::str_ends(name, "apa7starcolumn") ~ "apa7starcolumn",
          name == "apa7listcolumn" ~ "apa7listcolumn",
          name == "Variable" ~ "Variable",
          TRUE ~ name
        )
      ) |>
      dplyr::mutate(width = max(value, na.rm = TRUE), .by = group) |>
      dplyr::pull(width)

    w <- table_width * w / sum(w)

    w1 <- w[1]
    w[1] <- (.444 * font_size * max(nchar(d_msd[["apa7listcolumn"]])) + 4) / 72
    w[2] <- w[2] + w[1] - w1

    d_R <- flextable::width(d_R, width = w)
  }

  d_R
}

#' Convert data to flextable consistent with APA style
#'
#'
#' The `apa_flextable` function performs a number of formatting operations on the data before and after the data are sent to flextable. See Details.
#'
#'Roughly speaking, `apa_flextable` performs these operations by default:
#'
#' 1. Apply as_grouped_data and restructure row titles, if `row_title` is specified.
#' 2. Format data with apa_format_columns if `auto_format_columns = TRUE`
#' 3. Separate headers into multiple header rows if `separate_headers = TRUE`
#' 3. Apply `flextable::flextable`
#' 4. Apply `flextable::surround` to make borders to separate row groups, if any.
#' 5. Apply the `apa_style` function (table formatting and markdown conversion) if `apa_style = TRUE`
#' 6. Apply `pretty_widths` if `pretty_widths = TRUE`
#' @param data data.frame or tibble
#' @param row_title_column Column name or tidyselect function. column to group rows
#' @param row_title_align alignment of row title ("left", "center", "right")
#' @param row_title_prefix text to be added to each title
#' @param row_title_sep separator for prefix
#' @param row_title_border list of flextable styles
#' @param left_column_padding Number of points the left column is padded (only relevant when there is a `row_title_column` and `row_title_align = "left"`)
#' @param col_keys column keys passed to flextable (defaults data column names)
#' @param cwidth initial cell width in inches
#' @param cheight initial cell height in inches
#' @param apa_style apply `apa_style` function (default: `TRUE`)
#' @param separate_headers separate header rows (default: `TRUE`)
#' @param auto_format_columns if true, will attempt to format some columns automatically
#' @param column_formats a column_formats object
#' @param pretty_widths apply `pretty_widths` function
#' @param no_format_columns Column name or tidyselect function. selected columns are not formatted. Columns named in a `"no_format_columns"` attribute of `data` (set by functions such as [apa_loadings()]) are not formatted either. Likewise, a rule is drawn above the first body row whose first-column text matches a `"rule_above"` attribute of `data`.
#' @param add_breaks_between_spanners add breaks between spanners if TRUE
#' @param note table note placed below the table, after "*Note*.". The note is single-spaced. If `TRUE`, the note is written by [apa_note()] from what a table function such as [apa_anova()], [apa_parameters()], or [apa_loadings()] recorded. A character string is used as the note (markdown is allowed), and `NULL` or `FALSE` (default) adds no note. Use this for documents that are not made with apaquarto. With apaquarto, use the `apa-note` chunk option instead (e.g., `#| apa-note: !expr apa7::apa_note(tbl)`), so that the note is placed and styled by apaquarto and is not added twice.
#' @inheritParams apa_style
#' @param ... arguments passed to `apa_style`

#' @return flextable::flextable
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(flextable)
#' mtcars %>%
#'   dplyr::select(vs, am, gear, carb) |>
#'   tidyr::pivot_longer(-vs,  names_to = "Variable") |>
#'   dplyr::summarise(Mean = round(mean(value), 2),
#'                    SD = round(sd(value), 2),
#'                    .by = c(Variable,vs)) |>
#'   dplyr::mutate(vs = factor(vs, levels = 0:1, labels = c("Automatic", "Manual"))) |>
#'   apa_flextable(row_title_column= vs,  row_title_align = "center") |>
#'   align(j = 2:3, align = "center")
apa_flextable <- function(
  data,
  row_title_column = NULL,
  row_title_align = "left",
  row_title_prefix = "",
  row_title_sep = " ",
  row_title_border = list(
    color = "gray20",
    style = "solid",
    width = 1
  ),
  left_column_padding = 20,
  col_keys = colnames(data),
  cwidth = .75,
  cheight = .25,
  header_align_vertical = c("top", "middle", "bottom"),
  separate_headers = TRUE,
  apa_style = TRUE,
  font_family = NULL,
  font_size = 12,
  text_color = "black",
  border_color = "black",
  border_width = 0.5,
  line_spacing = 2,
  horizontal_padding = 3,
  table_align = "left",
  layout = "autofit",
  table_width = 1,
  markdown = TRUE,
  markdown_header = markdown,
  markdown_body = markdown,
  no_markdown_columns = NULL,
  no_markdown_columns_header = NULL,
  no_format_columns = NULL,
  auto_format_columns = TRUE,
  column_formats = NULL,
  pretty_widths = TRUE,
  add_breaks_between_spanners = TRUE,
  note = NULL,
  ...
) {
  # Prevent check warning
  column_n <- row_title <- value <- name <- newname <-
    cnames <- cnames_no_sep <- spanner_deckered <-
      deckered <- spanner <- name1 <- name <- value <-
        break_column <- NULL

  rtc <- rlang::enquo(row_title_column)

  rtc_val <- tryCatch(
    names(suppressWarnings(tidyselect::eval_select(rtc, data))),
    error = function(e) NULL
  )

  row_title_column <- dplyr::select(data, dplyr::all_of(rtc_val)) |>
    colnames()

  if (is.null(rtc_val)) {
    row_title_column <- dplyr::select(
      data,
      {{ row_title_column }}
    ) |>
      colnames()
  } else {}

  header_align_vertical <- match.arg(header_align_vertical)

  no_markdown_columns_header <- dplyr::select(
    data,
    {{ no_markdown_columns_header }}
  ) |>
    colnames()

  no_format_columns <- dplyr::select(data, {{ no_format_columns }}) |>
    colnames()

  # Tables such as apa_loadings() mark columns that must not be reformatted
  no_format_columns <- union(
    no_format_columns,
    intersect(attr(data, "no_format_columns", exact = TRUE), colnames(data))
  )

  # Get default column formats
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }

  if (row_title_prefix == "") {
    row_title_sep <- ""
  }

  # Use tidyselect to find column names
  numeric_columns <- dplyr::select(data, dplyr::where(is.numeric)) |> colnames()

  # if (!is.null(no_markdown_columns)) {
  no_markdown_columns <- dplyr::select(data, {{ no_markdown_columns }}) |>
    colnames()
  # } else {
  # no_markdown_columns <- character(0)
  # }
  no_markdown_columns <- unique(c(no_markdown_columns, numeric_columns))

  if (!is.null(no_markdown_columns_header)) {
    no_markdown_columns_header <- dplyr::select(
      data,
      {{ no_markdown_columns_header }}
    ) |>
      colnames()
  }

  # Insert breaks between column spanners
  if (add_breaks_between_spanners && separate_headers) {
    cf_names <- column_formats@get_column_names
    cf_names_no_sep <- stringr::str_replace_all(cf_names, "_", "apa7separator")
    names(cf_names_no_sep) <- cf_names

    brk_col <- tibble::tibble(
      cnames = colnames(data),
      cnames_no_sep = stringr::str_replace_all(cnames, cf_names_no_sep)
    ) |>
      # Split on last underscore
      dplyr::mutate(
        spanner_deckered = purrr::map(
          cnames_no_sep,
          \(x) stringr::str_split_fixed(x, "_(?=[^_]*$)", 2)
        )
      ) |>
      dplyr::mutate(
        spanner = purrr::map_chr(spanner_deckered, 1),
        deckered = purrr::map_chr(spanner_deckered, 2)
      ) |>
      dplyr::mutate(
        break_column = deckered != "" &
          dplyr::lag(deckered) != "" &
          spanner != dplyr::lag(spanner)
      ) |>
      dplyr::filter(break_column) |>
      dplyr::pull(cnames)

    if (length(brk_col) > 0) {
      data <- add_break_columns(data, dplyr::all_of(brk_col), .before = TRUE)
    }
  }

  # Convert data that are like integers into integers
  data <- dplyr::mutate(
    data,
    dplyr::across(
      dplyr::where(\(x) rlang::is_integerish(x) & !is.factor(x)),
      as.integer
    )
  )

  if (auto_format_columns) {
    cn_before <- colnames(data)
    data <- apa_format_columns(
      data,
      column_formats = column_formats,
      no_format_columns = no_format_columns
    )
    cn_after <- colnames(data)
    kv <- tibble::tibble(name = cn_before, newname = cn_after)

    col_keys <- tibble::tibble(name = col_keys) |>
      dplyr::left_join(kv, by = dplyr::join_by(name)) |>
      dplyr::mutate(newname = ifelse(is.na(newname), name, newname)) |>
      dplyr::pull(newname)
  }

  is_docx <- knitr::pandoc_to("docx")

  if (is_docx) {
    d <- data # nocov
  } else {
    d <- dplyr::select(data, -dplyr::starts_with("apa7breakcolumn"))
  }

  if (length(row_title_column) == 0) {
    ft <- flextable::flextable(
      d,
      col_keys = col_keys,
      cwidth = cwidth,
      cheight = cheight,
      theme_fun = flextable::theme_apa
    )
  } else {
    d <- flextable::as_grouped_data(d, groups = row_title_column)

    d$row_title <- d[, row_title_column]
    d <- tidyr::fill(d, !!row_title_column)

    d <- d |>
      dplyr::mutate(column_n = dplyr::n(), .by = !!row_title_column) |>
      dplyr::filter(column_n > 2 | is.na(row_title)) |>
      dplyr::select(-column_n)

    ft <- flextable::flextable(
      d,
      col_keys = col_keys[!(col_keys %in% row_title_column)],
      cwidth = cwidth,
      cheight = cheight,
      theme_fun = flextable::theme_apa
    ) |>
      flextable::mk_par(
        i = ~ !is.na(row_title),
        value = flextable::as_paragraph(
          row_title_prefix,
          row_title_sep,
          row_title
        )
      ) |>
      flextable::merge_h(i = ~ !is.na(row_title))

    if (!anyNA(row_title_border)) {
      ft <- ft |>
        flextable::surround(
          i = ~ !is.na(row_title),
          border.top = row_title_border
        )
    }
  }

  if (separate_headers) {
    ft <- flextable::separate_header(ft, split = "[_]")
  }
  if (!is_docx) {
    ft <- flextable::empty_blanks(ft)
  }

  ft <- flextable::align(ft, j = 1)
  center_me <- lapply(d, is_numeric_like) |> names()
  center_me <- center_me[center_me %in% ft$col_keys]
  if (length(center_me) > 0) {
    ft <- flextable::align(ft, j = center_me, align = "center")
  }
  cnames <- colnames(data)
  if (length(cnames) > 2) {
    if (cnames[1] == "Model" && cnames[2] == "Variable") {
      # Model is not a column when it is the row_title_column
      ft <- flextable::align(
        ft,
        j = intersect(c("Model", "Variable"), ft$col_keys),
        part = "all"
      )
    }
  }


  if (apa_style) {
    ft <- apa_style(
      ft,
      font_family = font_family,
      font_size = font_size,
      text_color = text_color,
      border_color = border_color,
      border_width = border_width,
      line_spacing = line_spacing,
      horizontal_padding = horizontal_padding,
      table_align = table_align,
      layout = layout,
      table_width = table_width,
      markdown = markdown,
      markdown_header = markdown_header,
      markdown_body = markdown_body,
      no_markdown_columns = no_markdown_columns,
      no_markdown_columns_header = no_markdown_columns_header,
      separate_headers = separate_headers,
      header_align_vertical = header_align_vertical,
      ...
    )
  }

  if ("row_title" %in% colnames(d)) {
    ft <- flextable::align(
      ft,
      i = ~ !is.na(row_title),
      align = row_title_align,
      part = "body"
    )
    if (row_title_align == "left") {
      ft <- flextable::padding(
        ft,
        j = 1,
        i = ~ is.na(row_title),
        padding.left = left_column_padding
      )
    }
  } else {
    ft <- flextable::align(ft, j = 1, part = "body")
  }
  if (pretty_widths) {
    ft <- pretty_widths(ft, table_width = table_width * 6.5)
  }

  if (is.null(font_family)) {
    font_family <- the$font_family
  }
  ft <- align_padded_columns(
    ft,
    d,
    font_family = font_family,
    font_size = font_size,
    horizontal_padding = horizontal_padding
  )

  # Tables such as apa_loadings() name rows that need a rule above them
  ft <- add_rule_above(
    ft,
    attr(data, "rule_above", exact = TRUE),
    border_color = border_color,
    border_width = border_width
  )

  # Added last so that the note does not affect column widths
  ft <- add_note_footer(
    ft,
    note_text(data, note),
    font_family = font_family,
    font_size = font_size,
    text_color = text_color,
    horizontal_padding = horizontal_padding
  )

  class(ft) <- unique(c("apaflextable", class(ft)))

  ft
}

#' Align padded columns on one side
#'
#' Columns padded by [align_chr()] line up only if every cell's text starts at
#' the same position. In a centered cell, that requires the text on both sides
#' of the alignment character to have equal widths, which is hard to achieve
#' with superscripts and other glyphs of irregular width. Instead, body cells
#' of padded columns are left-aligned, so only the left side's padding matters.
#' A uniform left padding centers the widest cell under the header. An
#' inaccurate width estimate shifts all rows equally, so it does not misalign
#' them. A nearly equal right padding keeps the block centered where columns
#' are sized to their contents (HTML).
#' @param ft flextable
#' @param d data used to make `ft`
#' @keywords internal
#' @noRd
align_padded_columns <- function(
  ft,
  d,
  font_family,
  font_size,
  horizontal_padding
) {
  # Spaces that align_chr and num_pad use for padding
  pad_re <- "[\u2002-\u200a]|&numsp;"

  keys <- intersect(ft$col_keys[-1], colnames(d))

  # Leave value columns paired with star or list columns as apa_style sets them
  star_keys <- ft$col_keys[grepl("apa7starcolumn$", ft$col_keys)]
  list_keys <- ft$col_keys[grepl("apa7listcolumn$", ft$col_keys)]
  paired <- c(
    ft$col_keys[match(star_keys, ft$col_keys) - 1],
    ft$col_keys[match(list_keys, ft$col_keys) + 1]
  )
  keys <- keys[!grepl("apa7(starcolumn|listcolumn)$|^apa7breakcolumn", keys)]
  keys <- setdiff(keys, paired)

  # Skip row-title rows, which span the table
  rows <- seq_len(nrow(d))
  if ("row_title" %in% colnames(d)) {
    rows <- which(is.na(d$row_title))
  }
  if (length(rows) == 0) {
    return(ft)
  }

  for (k in keys) {
    v <- as.character(d[[k]])[rows]
    if (!any(grepl(pad_re, v))) {
      next
    }
    block_width <- max(md_text_width(v, font_family)) * font_size
    col_width <- ft$body$colwidths[[match(k, ft$col_keys)]] * 72
    padding_left <- max(horizontal_padding, (col_width - block_width) / 2)
    # Browsers size HTML table columns to their cells, so a right padding is
    # needed to balance the left one. It is smaller by the horizontal padding,
    # so that an underestimated text width does not make cells wrap where
    # the column width is fixed (e.g., Word).
    padding_right <- max(horizontal_padding, padding_left - horizontal_padding)
    ft <- ft |>
      flextable::align(i = rows, j = k, align = "left", part = "body") |>
      flextable::padding(
        i = rows,
        j = k,
        padding.left = padding_left,
        padding.right = padding_right,
        part = "body"
      )
  }
  ft
}

#' format model parameters in APA style
#'
#' @param fit model fit object
#' @param predictor_parameters predictor parameters to display. If named vector, column names will be vector names. Columns not present in the model output (e.g., `t` for a model tested with *z*) are silently dropped. For lists of models, `NULL` (default) uses the default for each model's class. Add `"CI"` to show confidence intervals as \[*LL*, *UL*\] (headed, e.g., "95% CI"), and, for `glm`, `polr`, and `clm` models, `"Exp_CI"` to show confidence intervals of exponentiated coefficients (headed, e.g., "95% CI for *OR*"). Confidence intervals are not shown by default.
#' @param starred columns to star with significant p_values
#' @param bolded columns to bold, if significant
#' @param column_formats column_formats object to format columns. If NULL, the default column_formats is used.
#' @param t_with_df if TRUE, the t column will be displayed with degrees of freedom in parentheses. If FALSE, only the t value is displayed.
#' @param ... arguments passed to methods. For `lm`, `glm`, `polr`, and `clm` models, further arguments are passed to [parameters::model_parameters()]. For example, `vcov = "HC3"` gives heteroskedasticity-consistent (robust) standard errors, and the test statistics, *p*-values, and confidence intervals are based on them. `ci = .90` sets the confidence level.
#' @param performance_parameters for lists of models, model comparison metrics passed to [apa_performance_comparison()]. If `NULL`, no metrics are added. By default, `R2` and `deltaR2`; for lists of mixed models (`lme4`), the default also includes the conditional *R*^2^ and its change (`R2_conditional`, `deltaR2_conditional`), and `R2` is the marginal *R*^2^.
#'
#' @return tibble
#' @export
#'
#' @examples
#' lm(mpg ~ cyl + wt, data = mtcars) |>
#'    apa_parameters() |>
#'    apa_flextable()
#'
#' # Confidence intervals with robust (HC3) standard errors
#' if (requireNamespace("sandwich", quietly = TRUE)) {
#'   lm(mpg ~ cyl + wt, data = mtcars) |>
#'     apa_parameters(
#'       predictor_parameters = c("Parameter", "Coefficient", "SE", "CI", "t", "df_error", "p"),
#'       vcov = "HC3"
#'     ) |>
#'     apa_flextable()
#' }
apa_parameters <- function(
  fit,
  predictor_parameters = c(
    "Coefficient",
    "SE",
    "Std_Coefficient",
    "t",
    "df_error",
    "p"
  ),
  starred = NULL,
  bolded = NULL,
  column_formats = NULL,
  t_with_df = TRUE,
  ...
) {
  UseMethod("apa_parameters")
}

#' @export apa_parameters.lm
#' @export
#' @rdname apa_parameters
apa_parameters.lm <- function(
  fit,
  predictor_parameters = c(
    "Parameter",
    "Coefficient",
    "SE",
    "Std_Coefficient",
    "t",
    "df_error",
    "p"
  ),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  ...
) {
  # Prevent warnings
  Parameter <- Std_Coefficient <- NULL

  names(predictor_parameters) <- NULL

  # Get and format parameters
  d_b <- parameters::model_parameters(fit, ...) |>
    tibble::as_tibble() |>
    dplyr::mutate(Parameter = parameters::format_parameters(fit))

  # Get standardized parameters and merge them into other parameters
  if ("Std_Coefficient" %in% predictor_parameters) {
    d_std <- parameters::standardise_parameters(fit, method = "basic") |>
      tibble::as_tibble() |>
      dplyr::mutate(Parameter = parameters::format_parameters(fit)) |>
      dplyr::select(Parameter, Std_Coefficient)

    d_b <- d_b |>
      dplyr::left_join(d_std, by = dplyr::join_by(Parameter))
  }

  format_apa_parameters(
    d_b,
    predictor_parameters = predictor_parameters,
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    t_with_df = t_with_df,
    note = list(vcov = describe_vcov(list(...)$vcov))
  )
}

#' @param exponentiate if `NULL` (default), exponentiated coefficients are added when the model's link function is logit or log (i.e., odds ratios, risk ratios, or incidence rate ratios). If `TRUE`, exponentiated coefficients are always added. If `FALSE`, they are never added.
#' @details
#' For `glm` objects (including `MASS::glm.nb`), the test statistic is *z* for families with a fixed dispersion parameter (binomial, Poisson, negative binomial) and *t* for families with an estimated dispersion parameter (Gaussian, Gamma, inverse Gaussian, quasi families). Whichever statistic applies is displayed. The exponentiated coefficient column (`Exp_Coefficient`) is labeled *OR* for logit links, *IRR* for log links in count models (Poisson, quasi-Poisson, negative binomial), *RR* for log links in binomial models, and Exp(*B*) otherwise.
#' @export apa_parameters.glm
#' @export
#' @rdname apa_parameters
#' @examples
#' glm(am ~ wt + hp, data = mtcars, family = binomial) |>
#'    apa_parameters() |>
#'    apa_flextable()
apa_parameters.glm <- function(
  fit,
  predictor_parameters = c(
    "Parameter",
    "Coefficient",
    "SE",
    "Exp_Coefficient",
    "t",
    "z",
    "df_error",
    "p"
  ),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  exponentiate = NULL,
  ...
) {
  # Prevent warnings
  Parameter <- Std_Coefficient <- Coefficient <- NULL

  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }

  names(predictor_parameters) <- NULL

  link <- stats::family(fit)$link
  if (is.null(exponentiate)) {
    exponentiate <- link %in% c("logit", "log")
  }

  d_b <- parameters::model_parameters(fit, ...) |>
    tibble::as_tibble() |>
    dplyr::mutate(Parameter = parameters::format_parameters(fit))

  if (exponentiate) {
    d_b <- add_exp_columns(d_b, predictor_parameters)
    # Label exponentiated coefficients by link
    column_formats <- label_exp_columns(
      column_formats,
      exp_coefficient_label(fit)
    )
  }

  if ("Std_Coefficient" %in% predictor_parameters) {
    d_std <- parameters::standardise_parameters(fit, method = "basic") |>
      tibble::as_tibble() |>
      dplyr::mutate(Parameter = parameters::format_parameters(fit)) |>
      dplyr::select(Parameter, Std_Coefficient)

    d_b <- d_b |>
      dplyr::left_join(d_std, by = dplyr::join_by(Parameter))
  }

  # z tests have infinite df, which should not be displayed
  if ("df_error" %in% colnames(d_b) && all(is.infinite(d_b$df_error))) {
    d_b$df_error <- NULL
  }

  format_apa_parameters(
    d_b,
    predictor_parameters = predictor_parameters,
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    t_with_df = t_with_df,
    note = list(vcov = describe_vcov(list(...)$vcov))
  )
}

#' @param thresholds for ordinal models, if `TRUE` (default), threshold (cutpoint) rows are displayed below the predictors. For `lavaan` models, if `TRUE`, a section for the thresholds of ordinal indicators is added (`FALSE` by default).
#' @details
#' For ordinal regression models (`MASS::polr` and `ordinal::clm`), coefficients are tested with Wald *z* tests. (`parameters` reports *t* for `polr`, but the test is asymptotically normal, and `clm` reports *z* for the same model.) With a logit link, exponentiated coefficients are cumulative odds ratios, labeled *OR*. Threshold rows show only the estimate, its standard error, and its confidence interval, because their exponentiated values are not odds ratios, and testing whether a threshold equals 0 is not meaningful. Confidence intervals (`"CI"`, `"Exp_CI"`) are Wald *z* intervals, consistent with the *z* tests.
#' @export
#' @rdname apa_parameters
#' @examples
#' \donttest{
#' if (requireNamespace("MASS", quietly = TRUE) &&
#'     requireNamespace("carData", quietly = TRUE)) {
#'   MASS::polr(poverty ~ gender + age, data = carData::WVS, Hess = TRUE) |>
#'     apa_parameters()
#' }
#' }
apa_parameters.polr <- function(
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
  exponentiate = NULL,
  thresholds = TRUE,
  ...
) {
  link <- if (fit$method == "logistic") "logit" else fit$method
  apa_parameters_ordinal(
    fit,
    link = link,
    predictor_parameters = predictor_parameters,
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    t_with_df = t_with_df,
    exponentiate = exponentiate,
    thresholds = thresholds,
    ...
  )
}

#' @export
#' @rdname apa_parameters
apa_parameters.clm <- function(
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
  exponentiate = NULL,
  thresholds = TRUE,
  ...
) {
  apa_parameters_ordinal(
    fit,
    link = as.character(fit$link),
    predictor_parameters = predictor_parameters,
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    t_with_df = t_with_df,
    exponentiate = exponentiate,
    thresholds = thresholds,
    ...
  )
}

#' Parameters of ordinal cumulative link models
#' @param link link function ("logit", "probit", ...)
#' @keywords internal
#' @noRd
apa_parameters_ordinal <- function(
  fit,
  link,
  predictor_parameters,
  starred,
  bolded,
  column_formats,
  t_with_df,
  exponentiate,
  thresholds,
  ...
) {

  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }
  names(predictor_parameters) <- NULL

  if (is.null(exponentiate)) {
    exponentiate <- link == "logit"
  }

  d_b <- parameters::model_parameters(fit, ...) |>
    tibble::as_tibble() |>
    dplyr::mutate(Parameter = parameters::format_parameters(fit))

  # Thresholds are "alpha" (polr) or "intercept" (clm)
  is_threshold <- d_b$Component %in% c("alpha", "intercept")

  # Wald tests of ordinal models are asymptotically normal
  if ("t" %in% colnames(d_b)) {
    d_b <- dplyr::rename(d_b, z = "t")
    # Wald z intervals, consistent with the z test
    if (all(c("CI", "CI_low", "CI_high", "SE") %in% colnames(d_b))) {
      crit <- stats::qnorm((1 + d_b$CI) / 2)
      d_b$CI_low <- d_b$Coefficient - crit * d_b$SE
      d_b$CI_high <- d_b$Coefficient + crit * d_b$SE
    }
  }
  d_b$p <- 2 * stats::pnorm(-abs(d_b$z))
  d_b$df_error <- NULL

  if (exponentiate) {
    d_b <- add_exp_columns(d_b, predictor_parameters)
    exp_columns <- intersect(
      c("Exp_Coefficient", "Exp_CI_low", "Exp_CI_high"),
      colnames(d_b)
    )
    d_b[is_threshold, exp_columns] <- NA
    if (link == "logit") {
      column_formats <- label_exp_columns(
        column_formats,
        c(header = "*OR*", latex = "$OR$", abbreviation = "cumulative odds ratio")
      )
    }
  }

  # Testing whether a threshold equals 0 is not meaningful
  d_b$z[is_threshold] <- NA
  d_b$p[is_threshold] <- NA

  # Predictors first, then thresholds
  d_b <- d_b[order(is_threshold), ]
  if (!thresholds) {
    d_b <- d_b[!sort(is_threshold), ]
  }

  format_apa_parameters(
    d_b,
    predictor_parameters = predictor_parameters,
    starred = starred,
    bolded = bolded,
    column_formats = column_formats,
    t_with_df = t_with_df,
    note = list(
      vcov = describe_vcov(list(...)$vcov),
      thresholds = if (thresholds && any(is_threshold)) link
    )
  )
}

#' @param random for mixed models, if `TRUE` (default), random-effect standard deviations and correlations (and the residual standard deviation, for linear mixed models) are displayed below the fixed effects.
#' @param effect_headings for mixed models, if `TRUE` (default), the fixed and random effects are introduced by heading rows ("Fixed effects" and "Random effects"), with the effects indented below them. Headings are added only when random effects are displayed. The random-effects heading names the statistic of the *SD* (or variance) rows (e.g., "Random effects (*SD*)"), and each cluster (grouping factor) gets a subheading with its terms indented below it; correlation (or covariance) rows name their own statistic (e.g., "*r* (Constant, Days)"). Without headings, each random-effect label names its cluster and statistic (e.g., "Subject: *SD* (Days)").
#' @param random_scale for mixed models, `"sd"` (default) displays random effects as standard deviations (*SD*) and correlations (*r*), and `"variance"` displays them as variances (*Var*) and covariances (*Cov*).
#' @details
#' For mixed models fit with `lme4` (`lmer` and `glmer`), fixed effects are displayed as for `lm` and `glm` models. For linear mixed models, *t* tests and confidence intervals use Kenward-Roger degrees of freedom by default (`ci_method = "kenward"`, which requires the pbkrtest package); pass another `ci_method` (e.g., `"satterthwaite"`, which requires lmerTest, or `"wald"`) to change this. When the degrees of freedom differ across fixed effects, they are shown in a *df* column rather than in the *t* column header. Random-effect rows show only the estimated standard deviation (*SD*) or correlation (*r*), or with `random_scale = "variance"`, the variance (*Var*) or covariance (*Cov*), grouped by cluster (grouping factor) under a subheading for each cluster, with nested clusters named, e.g., "Classroom in School". The fixed and random effects are introduced by heading rows (`effect_headings = TRUE`). The random-effects heading names the statistic of the *SD* or variance rows (e.g., "Random effects (*SD*)"), and correlation or covariance rows name their own (e.g., "*r* (Constant, Days)"). [apa_note()] defines the symbols.
#' @export
#' @rdname apa_parameters
#' @examples
#' \donttest{
#' if (requireNamespace("lme4", quietly = TRUE) &&
#'     requireNamespace("pbkrtest", quietly = TRUE)) {
#'   lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy) |>
#'     apa_parameters()
#' }
#' }
apa_parameters.merMod <- function(
  fit,
  predictor_parameters = c(
    "Parameter",
    "Coefficient",
    "SE",
    "Exp_Coefficient",
    "t",
    "z",
    "df_error",
    "p"
  ),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  exponentiate = NULL,
  random = TRUE,
  effect_headings = TRUE,
  random_scale = c("sd", "variance"),
  ...
) {
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }
  names(predictor_parameters) <- NULL
  random_scale <- match.arg(random_scale)

  dots <- list(...)
  is_glmm <- inherits(fit, "glmerMod")
  if (!is_glmm && is.null(dots$ci_method)) {
    dots$ci_method <- "kenward"
  }

  d_b <- do.call(
    parameters::model_parameters,
    c(list(fit, effects = "fixed"), dots)
  ) |>
    tibble::as_tibble()

  # Format variable names here, because random-effect labels are built below
  name_formatter <- column_formats$Parameter@formatter
  d_b$Parameter <- name_formatter(parameters::format_parameters(fit))

  if (is_glmm) {
    if (is.null(exponentiate)) {
      exponentiate <- stats::family(fit)$link %in% c("logit", "log")
    }
    if (exponentiate) {
      d_b <- add_exp_columns(d_b, predictor_parameters)
      column_formats <- label_exp_columns(
        column_formats,
        exp_coefficient_label(fit)
      )
    }
  }

  # z tests have infinite df, which should not be displayed
  if ("df_error" %in% colnames(d_b) && all(is.infinite(d_b$df_error))) {
    d_b$df_error <- NULL
  }

  random_abbreviations <- NULL
  if (random) {
    d_r <- parameters::model_parameters(fit, effects = "random", ci = NULL) |>
      tibble::as_tibble()
    random_rows <- random_effect_rows(
      d_r,
      name_formatter,
      column_formats@accuracy,
      scale = random_scale,
      symbols_in_labels = !effect_headings
    )
    kinds <- attr(random_rows, "kinds")
    random_abbreviations <- random_effect_abbreviations(random_scale, kinds)
    d_b$Coefficient <- signs::signs(d_b$Coefficient, accuracy = column_formats@accuracy)
    d_b <- mixed_effect_sections(
      d_b,
      random_rows,
      headings = effect_headings,
      random_heading = random_effects_heading(random_scale, kinds)
    )
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
      df_method = if (!is_glmm) dots$ci_method,
      random_abbreviations = random_abbreviations
    )
  ) |>
    labels_formatted()
}

#' @details
#' For mixed models fit with `glmmTMB::glmmTMB()`, fixed effects are tested with *z* tests, and random effects are displayed as for `lme4` models. Coefficients of log and logit links are exponentiated by default (e.g., *IRR* for Poisson and negative binomial models). When the model has a zero-inflation part (`ziformula`) or a dispersion part (`dispformula`), each part is introduced by a heading row ("Conditional model", "Zero-inflation model", or "Dispersion model"), and only the conditional model's coefficients are exponentiated. A dispersion parameter that is not modeled (i.e., without a `dispformula`) is not displayed.
#' @export
#' @rdname apa_parameters
#' @examples
#' \donttest{
#' if (requireNamespace("glmmTMB", quietly = TRUE)) {
#'   data(Salamanders, package = "glmmTMB")
#'   glmmTMB::glmmTMB(
#'     count ~ mined + (1 | site),
#'     ziformula = ~ mined,
#'     family = poisson,
#'     data = Salamanders
#'   ) |>
#'     apa_parameters()
#' }
#' }
apa_parameters.glmmTMB <- function(
  fit,
  predictor_parameters = c(
    "Parameter",
    "Coefficient",
    "SE",
    "Exp_Coefficient",
    "t",
    "z",
    "df_error",
    "p"
  ),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  exponentiate = NULL,
  random = TRUE,
  effect_headings = TRUE,
  random_scale = c("sd", "variance"),
  ...
) {
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }
  names(predictor_parameters) <- NULL

  random_scale <- match.arg(random_scale)
  mp <- parameters::model_parameters(fit, effects = "fixed", ...)
  d_b <- tibble::as_tibble(mp)
  if (!("Component" %in% colnames(d_b))) {
    d_b$Component <- "conditional"
  }
  # A dispersion parameter that is not modeled has no standard error
  d_b <- d_b[!(d_b$Component == "dispersion" & is.na(d_b$SE)), ]

  # Format variable names here, because headings and random-effect labels are
  # built below
  name_formatter <- column_formats$Parameter@formatter
  pretty <- attr(mp, "pretty_names")
  labels <- unname(pretty[d_b$Parameter])
  labels[is.na(labels)] <- d_b$Parameter[is.na(labels)]
  d_b$Parameter <- name_formatter(labels)

  # Only the conditional model is exponentiated (e.g., IRR), because the
  # zero-inflation and dispersion parts have other links and meanings
  if (is.null(exponentiate)) {
    exponentiate <- stats::family(fit)$link %in% c("logit", "log")
  }
  if (exponentiate) {
    d_b <- add_exp_columns(d_b, predictor_parameters)
    not_conditional <- d_b$Component != "conditional"
    for (col in intersect(c("Exp_Coefficient", "Exp_CI_low", "Exp_CI_high"), colnames(d_b))) {
      d_b[[col]][not_conditional] <- NA
    }
    column_formats <- label_exp_columns(
      column_formats,
      exp_coefficient_label(fit)
    )
  }

  # z tests have infinite df, which should not be displayed
  if ("df_error" %in% colnames(d_b) && all(is.infinite(d_b$df_error))) {
    d_b$df_error <- NULL
  }

  d_r <- NULL
  random_abbreviations <- NULL
  if (random) {
    d_r <- parameters::model_parameters(fit, effects = "random", ci = NULL) |>
      tibble::as_tibble()
    if (!("Component" %in% colnames(d_r))) {
      d_r$Component <- "conditional"
    }
    d_b$Coefficient <- signs::signs(d_b$Coefficient, accuracy = column_formats@accuracy)
    # Rows are made for each part, because covariances use the SDs of the
    # same part
    d_r <- purrr::map(unique(d_r$Component), \(component) {
      part <- d_r[d_r$Component == component, ]
      rows <- random_effect_rows(
        part,
        name_formatter,
        column_formats@accuracy,
        scale = random_scale,
        symbols_in_labels = !effect_headings
      )
      kinds <- attr(rows, "kinds")
      rows$Component <- component
      rows$.kinds <- paste(kinds, collapse = " ")
      rows
    }) |>
      dplyr::bind_rows()
    random_abbreviations <- random_effect_abbreviations(
      random_scale,
      unlist(strsplit(unique(d_r$.kinds), " ", fixed = TRUE))
    )
  }

  # Each part of the model: fixed effects, then random effects
  components <- intersect(
    c("conditional", "zero_inflated", "dispersion"),
    c(d_b$Component, d_r$Component)
  )
  # Fixed and random effects get headings in every part when any part has
  # random effects
  any_random <- !is.null(d_r) && nrow(d_r) > 0
  pieces <- purrr::map(components, \(component) {
    fixed <- d_b[d_b$Component == component, ]
    fixed$Component <- NULL
    random_rows <- if (is.null(d_r)) NULL else d_r[d_r$Component == component, ]
    kinds <- character(0)
    if (!is.null(random_rows)) {
      kinds <- unlist(strsplit(unique(random_rows$.kinds), " ", fixed = TRUE))
      random_rows$Component <- NULL
      random_rows$.kinds <- NULL
    }
    mixed_effect_sections(
      fixed,
      random_rows,
      headings = effect_headings && any_random,
      always = TRUE,
      random_heading = random_effects_heading(random_scale, kinds)
    )
  })
  if (length(components) > 1) {
    headings <- c(
      conditional = "Conditional model",
      zero_inflated = "Zero-inflation model",
      dispersion = "Dispersion model"
    )
    pieces <- purrr::map2(pieces, components, \(piece, component) {
      piece$Parameter <- paste0("\u2003", piece$Parameter)
      dplyr::bind_rows(
        tibble::tibble(Parameter = headings[[component]]),
        piece
      )
    })
  }
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
      components = setdiff(components, "conditional"),
      random_abbreviations = random_abbreviations
    )
  ) |>
    labels_formatted()
}

#' Mark the first (label) column as already formatted
#'
#' Row labels such as "Subject: *SD* (Constant)" or headings such as
#' "Zero-inflation model" are formatted when the table is made. Formatting them
#' again in apa_flextable() would drop their punctuation and indentation.
#' @keywords internal
#' @noRd
labels_formatted <- function(d) {
  attr(d, "no_format_columns") <- union(
    attr(d, "no_format_columns", exact = TRUE),
    colnames(d)[1]
  )
  d
}

#' @details
#' For linear mixed models fit with `nlme::lme()`, fixed effects are tested with *t* tests whose degrees of freedom are computed by nlme from the grouping levels at which each effect varies, and random effects are displayed as for `lme4` models. Parameters of residual correlation structures (`correlation`, e.g., `nlme::corAR1()`) and variance functions (`weights`, e.g., `nlme::varIdent()`) are displayed below the residual *SD*, and [apa_note()] names the structures.
#' @export
#' @rdname apa_parameters
#' @examples
#' \donttest{
#' if (requireNamespace("nlme", quietly = TRUE)) {
#'   nlme::lme(
#'     distance ~ age + Sex,
#'     random = ~ 1 | Subject,
#'     correlation = nlme::corAR1(),
#'     data = nlme::Orthodont
#'   ) |>
#'     apa_parameters()
#' }
#' }
apa_parameters.lme <- function(
  fit,
  predictor_parameters = c(
    "Parameter",
    "Coefficient",
    "SE",
    "t",
    "df_error",
    "p"
  ),
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  random = TRUE,
  effect_headings = TRUE,
  random_scale = c("sd", "variance"),
  ...
) {
  if (inherits(fit, "glmmPQL")) {
    stop(
      "apa_parameters() does not support glmmPQL models. ",
      "Fit generalized linear mixed models with lme4::glmer() or glmmTMB::glmmTMB()."
    )
  }
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }
  names(predictor_parameters) <- NULL

  random_scale <- match.arg(random_scale)
  d_b <- parameters::model_parameters(fit, effects = "fixed", ...) |>
    tibble::as_tibble()

  # Format variable names here, because random-effect labels are built below
  name_formatter <- column_formats$Parameter@formatter
  d_b$Parameter <- name_formatter(parameters::format_parameters(fit))

  residual <- lme_residual_structures(
    fit,
    name_formatter,
    column_formats@accuracy,
    scale = random_scale
  )

  random_abbreviations <- NULL
  if (random) {
    d_r <- parameters::model_parameters(fit, effects = "random", ci = NULL) |>
      tibble::as_tibble()
    d_b$Coefficient <- signs::signs(d_b$Coefficient, accuracy = column_formats@accuracy)
    random_rows <- random_effect_rows(
      d_r,
      name_formatter,
      column_formats@accuracy,
      scale = random_scale,
      symbols_in_labels = !effect_headings
    )
    kinds <- attr(random_rows, "kinds")
    random_abbreviations <- random_effect_abbreviations(random_scale, kinds)
    # Residual correlations and variance functions are random-effect
    # parameters, too
    d_b <- mixed_effect_sections(
      d_b,
      dplyr::bind_rows(random_rows, residual$rows),
      headings = effect_headings,
      random_heading = random_effects_heading(random_scale, kinds)
    )
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
      df_method = "nlme",
      residual = residual$note,
      random_abbreviations = random_abbreviations
    )
  ) |>
    labels_formatted()
}

#' Rows and note sentences for the residual structures of an lme model
#'
#' @returns list with `rows` (a tibble of Parameter and formatted Coefficient)
#'   and `note` (character vector of sentences)
#' @keywords internal
#' @noRd
lme_residual_structures <- function(fit, formatter, accuracy, scale = "sd") {
  on_variance <- identical(scale, "variance")
  rows <- tibble::tibble(Parameter = character(0), Coefficient = character(0))
  note <- character(0)
  groups_text <- function(struct) {
    g <- nlme::getGroupsFormula(struct)
    if (is.null(g)) NULL else formatter(all.vars(g))
  }
  covariate_text <- function(struct) {
    v <- all.vars(nlme::getCovariateFormula(struct))
    if (length(v) == 0) "the fitted values" else paste(formatter(v), collapse = " and ")
  }

  # Correlation structure
  cs <- fit$modelStruct$corStruct
  if (!is.null(cs)) {
    cls <- class(cs)[1]
    values <- stats::coef(cs, unconstrained = FALSE)
    labels <- switch(
      cls,
      corAR1 = ,
      corCAR1 = "Residual autocorrelation (*\u03C6*)",
      corCompSymm = "Residual correlation (*\u03C1*)",
      paste0("Residual correlation: ", names(values))
    )
    rows <- dplyr::bind_rows(
      rows,
      tibble::tibble(
        Parameter = labels,
        Coefficient = signs::signs(values, accuracy = accuracy, trim_leading_zeros = TRUE)
      )
    )
    structure_name <- switch(
      cls,
      corAR1 = "a first-order autoregressive (AR(1))",
      corCAR1 = "a continuous-time first-order autoregressive (CAR(1))",
      corARMA = "an autoregressive moving-average (ARMA)",
      corCompSymm = "a compound-symmetric",
      corSymm = "an unstructured",
      corExp = "an exponential spatial",
      corGaus = "a Gaussian spatial",
      corLin = "a linear spatial",
      corRatio = "a rational quadratic spatial",
      corSpher = "a spherical spatial",
      paste0("a ", cls)
    )
    groups <- groups_text(cs)
    note <- c(
      note,
      paste0(
        "Residuals",
        if (length(groups) > 0) paste0(" within ", paste(groups, collapse = " and ")),
        " have ",
        structure_name,
        " correlation structure."
      )
    )
  }

  # Variance function
  vs <- fit$modelStruct$varStruct
  if (!is.null(vs)) {
    cls <- class(vs)[1]
    values <- stats::coef(vs, unconstrained = FALSE)
    if (cls == "varIdent") {
      all_values <- stats::coef(vs, unconstrained = FALSE, allCoef = TRUE)
      reference <- names(all_values)[!(names(all_values) %in% names(values))][1]
      rows <- dplyr::bind_rows(
        rows,
        tibble::tibble(
          Parameter = paste0(
            if (on_variance) "Residual variance ratio: " else "Residual *SD* ratio: ",
            names(values)
          ),
          Coefficient = signs::signs(if (on_variance) values^2 else values, accuracy = accuracy)
        )
      )
      groups <- groups_text(vs)
      what <- if (on_variance) "variance" else "*SD*"
      note <- c(
        note,
        paste0(
          "Each level of ",
          paste(groups, collapse = " and "),
          " has its own residual ", what, ". The residual ", what, " is that of ",
          reference,
          ", and the others are shown as ratios to it."
        )
      )
    } else if (length(values) > 0) {
      # On the variance scale, the power and exponent of the SD double
      labels <- switch(
        cls,
        varPower = if (on_variance) "Residual variance power (2*\u03B4*)" else "Residual *SD* power (*\u03B4*)",
        varExp = if (on_variance) "Residual variance exponent (2*\u03B4*)" else "Residual *SD* exponent (*\u03B4*)",
        paste0("Residual variance: ", names(values))
      )
      shown <- if (on_variance && cls %in% c("varPower", "varExp")) 2 * values else values
      rows <- dplyr::bind_rows(
        rows,
        tibble::tibble(
          Parameter = labels,
          Coefficient = signs::signs(shown, accuracy = accuracy)
        )
      )
    }
    note <- c(
      note,
      switch(
        cls,
        varIdent = NULL,
        varPower = paste0("Residual *SD*s are proportional to a power (*\u03B4*) of ", covariate_text(vs), "."),
        varExp = paste0("Residual *SD*s are an exponential function of ", covariate_text(vs), "."),
        varConstPower = paste0("Residual *SD*s are a constant plus a power of ", covariate_text(vs), "."),
        varFixed = paste0("Residual variances are proportional to ", covariate_text(vs), "."),
        paste0("Residual variances follow a ", cls, " variance function.")
      )
    )
  }
  list(rows = rows, note = note)
}

#' Fixed effects, then random effects, each under a heading row
#'
#' @param fixed rows of fixed effects
#' @param random rows of random effects (or NULL)
#' @param headings add "Fixed effects" and "Random effects" heading rows, with
#'   the effects indented below them
#' @param random_heading heading of the random effects (e.g., "Random effects
#'   (*SD*)")
#' @param always if `TRUE`, the fixed effects get their heading even when
#'   there are no random effects (for model parts without random effects in a
#'   table whose other parts have them)
#' @keywords internal
#' @noRd
mixed_effect_sections <- function(
  fixed,
  random = NULL,
  headings = TRUE,
  always = FALSE,
  random_heading = "Random effects"
) {
  has_random <- !is.null(random) && nrow(random) > 0
  if (!headings || (!has_random && !always)) {
    return(dplyr::bind_rows(fixed, random))
  }
  indent <- function(d) {
    d$Parameter <- paste0("\u2003", d$Parameter)
    d
  }
  pieces <- list(
    tibble::tibble(Parameter = "Fixed effects"),
    indent(fixed)
  )
  if (has_random) {
    pieces <- c(pieces, list(
      tibble::tibble(Parameter = random_heading),
      indent(random)
    ))
  }
  dplyr::bind_rows(pieces)
}

#' Rows for the random effects of a mixed model
#'
#' Converts `parameters` random-effect estimates (e.g., "SD (Days)" and
#' "Cor (Intercept~Days)" for group Subject, and "SD (Observations)" for the
#' residual) into labeled rows on the standard deviation or variance scale.
#' @param d_r random-effect parameters from parameters::model_parameters()
#' @param formatter formats term names (e.g., "(Intercept)" to "Constant")
#' @param accuracy accuracy of the coefficients
#' @param scale `"sd"` (standard deviations and correlations) or `"variance"`
#'   (variances and covariances)
#' @param symbols_in_labels if `TRUE` (no headings), each row is labeled with
#'   its cluster and statistic (e.g., "Subject: *SD* (Days)"). If `FALSE`,
#'   each cluster gets a subheading row with its terms indented below it
#'   (e.g., "Subject", then "Days"), and the heading above names the SD (or
#'   variance). Correlations (or covariances) always name their statistic
#'   (e.g., "*r* (Constant, Days)").
#' @returns tibble with `Parameter` and `Coefficient` (formatted), and a
#'   `kinds` attribute: the kinds of statistics in the rows (`"spread"` for
#'   SDs or variances, `"association"` for correlations or covariances)
#' @keywords internal
#' @noRd
random_effect_rows <- function(d_r, formatter, accuracy, scale = "sd", symbols_in_labels = FALSE) {
  terms <- stringr::str_match(d_r$Parameter, "^(SD|Cor) \\((.*)\\)$")
  type <- terms[, 2]
  inner <- terms[, 3]
  group <- d_r$Group
  value <- d_r$Coefficient
  is_cor <- !is.na(type) & type == "Cor"
  is_residual <- group == "Residual"
  symbols <- random_effect_symbols(scale)

  # parameters drops the parentheses around Intercept in these labels
  term_label <- function(x) {
    x[x == "Intercept"] <- "(Intercept)"
    formatter(x)
  }

  # Values on the requested scale: variances are squared SDs, and
  # covariances are correlations times the two SDs in the same cluster
  out_value <- value
  if (scale == "variance") {
    for (i in seq_len(nrow(d_r))) {
      if (is.na(type[i])) next # nocov
      if (is_cor[i]) {
        parts <- strsplit(inner[i], "~", fixed = TRUE)[[1]]
        sd_of <- function(term) {
          k <- which(!is_cor & group == group[i] & inner == term)
          if (length(k) == 0) NA_real_ else value[k[1]]
        }
        out_value[i] <- value[i] * sd_of(parts[1]) * sd_of(parts[2])
      } else {
        out_value[i] <- value[i]^2
      }
    }
  }

  # Labels without the cluster: "Days", "*r* (Constant, Days)", "Residual"
  term_text <- purrr::map_chr(seq_len(nrow(d_r)), \(i) {
    if (is.na(type[i])) {
      return(d_r$Parameter[i]) # nocov
    }
    if (is_residual[i]) {
      return(if (symbols_in_labels) paste0("Residual ", symbols[["spread"]]) else "Residual")
    }
    parts <- term_label(strsplit(inner[i], "~", fixed = TRUE)[[1]])
    if (is_cor[i]) {
      paste0(symbols[["association"]], " (", paste(parts, collapse = ", "), ")")
    } else if (symbols_in_labels) {
      paste0(symbols[["spread"]], " (", parts, ")")
    } else {
      parts
    }
  })

  # Correlations have no leading zero; the others are formatted like
  # coefficients
  coef_chr <- character(nrow(d_r))
  bounded <- is_cor & scale == "sd"
  coef_chr[bounded] <- signs::signs(out_value[bounded], accuracy = accuracy, trim_leading_zeros = TRUE)
  coef_chr[!bounded] <- signs::signs(out_value[!bounded], accuracy = accuracy)

  cluster <- purrr::map_chr(group, \(g) cluster_label(g, formatter))
  if (symbols_in_labels) {
    labels <- ifelse(is_residual, term_text, paste0(cluster, ": ", term_text))
    rows <- tibble::tibble(Parameter = labels, Coefficient = coef_chr)
  } else {
    # A subheading for each cluster, in order of appearance, with its terms
    # indented below it. The residual (and any residual structure added
    # later) is at the level of the clusters.
    clusters <- unique(group[!is_residual])
    pieces <- purrr::map(clusters, \(g) {
      k <- which(group == g & !is_residual)
      tibble::tibble(
        Parameter = c(cluster[k[1]], paste0("\u2003", term_text[k])),
        Coefficient = c(NA_character_, coef_chr[k])
      )
    })
    residual_rows <- tibble::tibble(
      Parameter = term_text[is_residual],
      Coefficient = coef_chr[is_residual]
    )
    rows <- dplyr::bind_rows(c(pieces, list(residual_rows)))
  }
  attr(rows, "kinds") <- c(if (any(!is_cor)) "spread", if (any(is_cor)) "association")
  rows
}

#' Name of a cluster (grouping factor), with nesting spelled out
#'
#' lme4 names nested clusters "inner:outer" (e.g., "Classroom:School" for
#' `(1 | School/Classroom)`), which becomes "Classroom in School".
#' @keywords internal
#' @noRd
cluster_label <- function(group, formatter) {
  parts <- strsplit(group, ":", fixed = TRUE)[[1]]
  paste(formatter(parts), collapse = " in ")
}

#' Symbols of random-effect statistics on each scale
#' @keywords internal
#' @noRd
random_effect_symbols <- function(scale = "sd") {
  if (scale == "variance") {
    c(spread = "*Var*", association = "*Cov*")
  } else {
    c(spread = "*SD*", association = "*r*")
  }
}

#' Definitions of the random-effect symbols in a table
#' @param kinds kinds of statistics shown (see random_effect_rows())
#' @keywords internal
#' @noRd
random_effect_abbreviations <- function(scale = "sd", kinds = c("spread", "association")) {
  symbols <- random_effect_symbols(scale)
  meanings <- if (scale == "variance") {
    c(spread = "variance", association = "covariance")
  } else {
    c(spread = "standard deviation", association = "correlation")
  }
  kinds <- intersect(c("spread", "association"), kinds)
  stats::setNames(unname(meanings[kinds]), unname(symbols[kinds]))
}

#' "Random effects (*SD*)"
#'
#' The heading names the statistic of the SD (or variance) rows. Correlation
#' (or covariance) rows name their own statistic.
#' @keywords internal
#' @noRd
random_effects_heading <- function(scale = "sd", kinds = c("spread", "association")) {
  if (!("spread" %in% kinds)) {
    return("Random effects") # nocov
  }
  paste0("Random effects (", random_effect_symbols(scale)[["spread"]], ")")
}

#' Add exponentiated coefficients and confidence limits
#' @keywords internal
#' @noRd
add_exp_columns <- function(d_b, predictor_parameters) {
  if ("Exp_Coefficient" %in% predictor_parameters) {
    d_b$Exp_Coefficient <- exp(d_b$Coefficient)
  }
  if (
    "Exp_CI" %in% predictor_parameters &&
      all(c("CI_low", "CI_high") %in% colnames(d_b))
  ) {
    d_b$Exp_CI_low <- exp(d_b$CI_low)
    d_b$Exp_CI_high <- exp(d_b$CI_high)
  }
  d_b
}

#' Label exponentiated columns (e.g., *OR*), unless the user customized them
#' @param label named character vector with `header`, `latex`, and
#'   (optionally) `abbreviation`
#' @keywords internal
#' @noRd
label_exp_columns <- function(column_formats, label) {
  if (
    !is.null(column_formats$Exp_Coefficient) &&
      identical(
        column_formats$Exp_Coefficient@header,
        the$columns$Exp_Coefficient@header
      )
  ) {
    column_formats$Exp_Coefficient@header <- label[["header"]]
    column_formats$Exp_Coefficient@latex <- label[["latex"]]
    if ("abbreviation" %in% names(label)) {
      column_formats$Exp_Coefficient@abbreviation <- label[["abbreviation"]]
    }
  }
  if (
    !is.null(column_formats$Exp_CI) &&
      identical(column_formats$Exp_CI@header, the$columns$Exp_CI@header)
  ) {
    column_formats$Exp_CI@header <- paste0(
      "{round(ci * 100)}% CI for ",
      label[["header"]]
    )
    column_formats$Exp_CI@latex <- paste0(
      "{round(ci * 100)}\\% CI for ",
      label[["latex"]]
    )
  }
  column_formats
}

#' @keywords internal
#' @noRd
exp_coefficient_label <- function(fit) {
  fam <- stats::family(fit)
  family <- tolower(fam$family)
  if (fam$link == "logit") {
    return(c(header = "*OR*", latex = "$OR$", abbreviation = "odds ratio"))
  }
  if (fam$link == "log") {
    if (grepl("poisson|negative binomial|nbinom|genpois|compois", family)) {
      return(c(
        header = "*IRR*",
        latex = "$IRR$",
        abbreviation = "incidence rate ratio"
      ))
    }
    if (grepl("binomial", family)) {
      return(c(header = "*RR*", latex = "$RR$", abbreviation = "risk ratio"))
    }
  }
  c(
    header = the$columns$Exp_Coefficient@header,
    latex = the$columns$Exp_Coefficient@latex
  )
}

#' Format a model parameter tibble in APA style
#'
#' Shared by `apa_parameters` methods. `d_b` holds unformatted parameters with
#' `Parameter` and `p` columns.
#' @keywords internal
#' @noRd
format_apa_parameters <- function(
  d_b,
  predictor_parameters,
  starred = NA,
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  note = list()
) {
  # Prevent warnings
  pstar <- df_error <- header <- name <- NULL

  # Get column formats
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }

  # Make sure Parameter is in the predictor_parameters
  if (!("Parameter" %in% predictor_parameters)) {
    predictor_parameters <- c("Parameter", predictor_parameters) # nocov
  }

  names(predictor_parameters) <- NULL

  d_b_new <- d_b

  # Put df into t column name, if all parameters have the same df
  if (t_with_df) {
    if (
      all(c("t", "df_error") %in% colnames(d_b)) &&
        !is.null(column_formats$CI) &&
        ("t" %in% predictor_parameters) &&
        length(unique(round(stats::na.omit(d_b$df_error), 2))) == 1
    ) {
      # e.g., t(29), or t(17.03) for fractional (e.g., Kenward-Roger) df
      df <- paste0(
        "(",
        align_chr(
          max(d_b$df_error, na.rm = TRUE),
          accuracy = the$accuracy,
          drop0trailing = TRUE
        ),
        ")"
      )
      column_formats$t@header <- paste0(column_formats$t@header, df)

      if (stringr::str_detect(column_formats$t@latex, "\\$")) {
        column_formats$t@latex <- stringr::str_replace(
          column_formats$t@latex,
          "\\$$",
          paste0(df, "$")
        )
      } else {
        column_formats$t@latex <- paste0(column_formats$t@latex, df) # nocov
      }

      d_b_new <- d_b_new |>
        dplyr::select(-df_error)
    }
  }

  # Combine lower and upper bounds into one interval column. The CI level
  # (e.g., .95) fills in the header template.
  ci_columns <- c("CI", "Exp_CI")
  ci_level <- 0.95
  if ("CI" %in% colnames(d_b) && is.numeric(d_b$CI) && !all(is.na(d_b$CI))) {
    ci_level <- d_b$CI[!is.na(d_b$CI)][1]
  }
  for (ci_col in ci_columns) {
    bounds <- paste0(ci_col, c("_low", "_high"))
    if (
      ci_col %in% predictor_parameters &&
        all(bounds %in% colnames(d_b)) &&
        !is.null(column_formats[[ci_col]])
    ) {
      d_b_new[[ci_col]] <- column_formats[[ci_col]]@formatter(
        d_b[[bounds[1]]],
        d_b[[bounds[2]]]
      )
      column_formats[[ci_col]]@header <- as.character(
        glue::glue(column_formats[[ci_col]]@header, ci = ci_level)
      )
      column_formats[[ci_col]]@latex <- as.character(
        glue::glue(column_formats[[ci_col]]@latex, ci = ci_level)
      )
    } else {
      # The CI column from parameters holds the CI level, not an interval
      d_b_new[[ci_col]] <- NULL
    }
  }

  # Format columns
  for (pr in setdiff(predictor_parameters, ci_columns)) {
    if (!is.null(column_formats[[pr]])) {
      d_b_new <- dplyr::mutate(
        d_b_new,
        dplyr::across(
          dplyr::any_of(pr),
          column_formats[[pr]]@formatter
        )
      )
    }
  }

  # add starred column
  starred <- starred[starred %in% colnames(d_b_new)]
  is_starred <- length(starred) > 0 && !anyNA(starred) && all(starred != "")
  if (is_starred) {
    pstar <- p2stars(d_b$p, superscript = TRUE)
    pstar[is.na(pstar)] <- ""
    d_b_new <- d_b_new |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(starred),
          \(x) align_chr(paste0(ifelse(is.na(x), "", x), pstar))
        )
      )
  }

  # bold star
  is_bolded <- length(bolded) > 0 && !anyNA(bolded) && !all(bolded == "") &&
    any(bolded %in% colnames(d_b_new))
  if (is_bolded) {
    my_bold <- ifelse(d_b$p <= 0.05, "**", "")
    d_b_new <- d_b_new |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(bolded),
          \(x) tagger(x, my_bold)
        )
      )
  }

  prs <- column_formats@get_tibble

  pr_selector <- tibble::tibble(name = predictor_parameters) |>
    dplyr::left_join(prs, by = "name") |>
    dplyr::select(header, name) |>
    dplyr::mutate(header = ifelse(is.na(header), name, header)) |>
    tibble::deframe()

  shown <- pr_selector[pr_selector %in% colnames(d_b_new)]
  d_b_new <- dplyr::select(d_b_new, dplyr::any_of(pr_selector))
  attr(d_b_new, "apa_note") <- parameters_note_record(
    note,
    headers = stats::setNames(names(shown), shown),
    abbreviations = column_abbreviations(unname(shown), column_formats),
    starred = is_starred,
    bolded = is_bolded
  )
  as_apa7_tbl(d_b_new)
}

#' @export
#' @rdname apa_parameters
apa_parameters.list <- function(
  fit,
  predictor_parameters = NULL,
  starred = "deltaR2",
  bolded = NA,
  column_formats = NULL,
  t_with_df = TRUE,
  performance_parameters = c("R2", "deltaR2"),
  ...
) {
  # Prevent warnings
  Model <- NULL

  # Mixed models also show the conditional R2 and its change by default
  if (
    missing(performance_parameters) &&
      all(purrr::map_lgl(fit, \(m) inherits(m, c("merMod", "glmmTMB", "lme"))))
  ) {
    performance_parameters <- c(
      "R2",
      "deltaR2",
      "R2_conditional",
      "deltaR2_conditional"
    )
  }

  # List names label the models ("Model 1", ... if unnamed)
  names(fit) <- model_names(fit)
  model_tables <- purrr::map(
    fit,
    \(f1) {
      args <- list(
        f1,
        column_formats = column_formats,
        t_with_df = FALSE,
        starred = starred,
        bolded = bolded,
        ...
      )
      # If NULL, use the default predictor_parameters of each model's method
      if (!is.null(predictor_parameters)) {
        args$predictor_parameters <- predictor_parameters
      }
      do.call(apa_parameters, args)
    }
  )
  d_fit <- dplyr::bind_rows(model_tables, .id = "Model")

  # Each model was aligned separately; re-align values across all models.
  # Skip Model and the variable-name column that follows it.
  value_columns <- setdiff(colnames(d_fit), colnames(d_fit)[1:2])
  ci_value_columns <- value_columns[purrr::map_lgl(d_fit[value_columns], is_ci_md)]
  d_fit <- d_fit |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(ci_value_columns), realign_ci),
      dplyr::across(
        dplyr::all_of(setdiff(value_columns, ci_value_columns)) &
          dplyr::where(is.character),
        align_chr
      )
    )

  show_performance <- !(is.null(performance_parameters) ||
    length(performance_parameters) == 0 ||
    all(is.na(performance_parameters)))
  note_formats <- column_formats
  if (show_performance) {
    comparison <- apa_performance_comparison(
      fit,
      metrics = performance_parameters,
      starred = starred
    )
    note_formats <- attr(comparison, "column_formats", exact = TRUE) %||% column_formats
    d_fit <- d_fit |>
      flextable::as_grouped_data(groups = "Model") |>
      dplyr::left_join(
        comparison,
        by = dplyr::join_by(Model)
      ) |>
      dplyr::mutate(dplyr::across(.cols = dplyr::where(is.character), \(x) dplyr::replace_values(x, from = NA, to = "")))
  }

  # Labels of each model's table were formatted when it was made
  if (any(purrr::map_lgl(model_tables, \(x) !is.null(attr(x, "no_format_columns", exact = TRUE))))) {
    attr(d_fit, "no_format_columns") <- colnames(d_fit)[2]
  }

  attr(d_fit, "apa_note") <- merge_parameters_note_records(
    purrr::map(model_tables, \(x) attr(x, "apa_note", exact = TRUE)),
    performance = if (show_performance) performance_parameters,
    performance_starred = show_performance && any(starred %in% performance_parameters),
    column_formats = note_formats
  )
  as_apa7_tbl(d_fit)
}

#' format model performance metrics in APA style
#'
#' @param fit model fit object
#' @param metrics performance metrics. Default is R2 and Sigma
#' @param column_formats column_formats object to format columns. If NULL, the default column_formats is used.
#'
#' @return tibble
#' @export
#'
#' @examples
#' lm(mpg ~ cyl + wt, data = mtcars) |>
#'    apa_performance() |>
#'    apa_flextable()
apa_performance <- function(
  fit,
  metrics = c("R2", "Sigma"),
  column_formats = NULL
) {
  UseMethod("apa_performance")
}

#' @export
#' @rdname apa_performance
apa_performance.lm <- function(
  fit,
  metrics = c("R2", "Sigma"),
  column_formats = NULL
) {
  if (any(metrics == "all")) {
    metrics <- c("AIC", "AICc", "BIC", "R2", "R2_adjusted", "RMSE", "Sigma")
  }

  header <- name <- NULL

  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }

  names(metrics) <- NULL

  d <- performance::model_performance(fit, metrics = "all") |>
    tibble::as_tibble()
  # Multinomial models report McFadden's R2 as R2
  if (inherits(fit, "multinom") && "R2" %in% colnames(d)) {
    d <- dplyr::rename(d, R2_McFadden = "R2")
  }

  # GLMs report a pseudo-R2 under its own name
  pr2 <- use_pseudo_r2(d, column_formats)
  d <- pr2$data
  column_formats <- pr2$column_formats

  prs <- column_formats@get_tibble

  d_new <- d

  for (pr in metrics) {
    if (!is.null(column_formats[[pr]])) {
      d_new <- dplyr::mutate(
        d_new,
        dplyr::across(dplyr::any_of(pr), column_formats[[pr]]@formatter)
      )
    }
  }

  pr_selector <- tibble::tibble(name = metrics) |>
    dplyr::left_join(prs, by = "name") |>
    dplyr::select(header, name) |>
    dplyr::mutate(
      header = ifelse(
        is.na(header),
        name,
        header
      )
    ) |>
    tibble::deframe()

  d_new |>
    dplyr::select(dplyr::any_of(pr_selector)) |>
    as_apa7_tbl()
}

#' @details
#' For `glm` objects, `R2` is the pseudo-*R*^2^ that [performance::r2()] selects for the model family (e.g., Tjur's *R*^2^ for logistic regression and Nagelkerke's *R*^2^ for count models). The header names the pseudo-*R*^2^ that was used. Because the residual standard deviation is not meaningful for most GLMs, the default metrics are `R2` and `AIC`.
#' @export
#' @rdname apa_performance
apa_performance.glm <- function(
  fit,
  metrics = c("R2", "AIC"),
  column_formats = NULL
) {
  apa_performance.lm(fit, metrics = metrics, column_formats = column_formats)
}

#' @export
#' @rdname apa_performance
apa_performance.polr <- apa_performance.glm

#' @export
#' @rdname apa_performance
apa_performance.clm <- apa_performance.glm

#' @details
#' For mixed models (`lme4::lmer`, `lme4::glmer`, `glmmTMB::glmmTMB`, and `nlme::lme`), the default metrics are the marginal *R*^2^ (variance explained by the fixed effects), the conditional *R*^2^ (fixed and random effects), and the intraclass correlation (ICC), as computed by [performance::model_performance()].
#' @export
#' @rdname apa_performance
apa_performance.merMod <- function(
  fit,
  metrics = c("R2_marginal", "R2_conditional", "ICC"),
  column_formats = NULL
) {
  apa_performance.lm(fit, metrics = metrics, column_formats = column_formats)
}

#' @export
#' @rdname apa_performance
apa_performance.glmmTMB <- apa_performance.merMod

#' @export
#' @rdname apa_performance
apa_performance.lme <- apa_performance.merMod

#' @details
#' For a list of models, each model's metrics are displayed in a row, labeled in a `Model` column by the list's names (or "Model 1", "Model 2", ... for unnamed elements). If `metrics` is `NULL` (default), each model's default metrics are used.
#' @export
#' @rdname apa_performance
#' @examples
#' list(
#'   `Cylinders` = lm(mpg ~ cyl, data = mtcars),
#'   `Cylinders and weight` = lm(mpg ~ cyl + wt, data = mtcars)
#' ) |>
#'   apa_performance()
apa_performance.list <- function(
  fit,
  metrics = NULL,
  column_formats = NULL
) {
  Model <- NULL
  names(fit) <- model_names(fit)
  tables <- purrr::map(
    fit,
    \(f1) {
      args <- list(f1, column_formats = column_formats)
      if (!is.null(metrics)) {
        args$metrics <- metrics
      }
      do.call(apa_performance, args)
    }
  )
  records <- purrr::map(tables, \(x) attr(x, "apa_note", exact = TRUE))
  d <- purrr::map(tables, tibble::as_tibble) |>
    dplyr::bind_rows(.id = "Model")

  # Each model was aligned separately; re-align values across models
  value_columns <- setdiff(colnames(d), "Model")
  ci_columns <- value_columns[purrr::map_lgl(d[value_columns], is_ci_md)]
  d <- d |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(ci_columns), realign_ci),
      dplyr::across(
        dplyr::all_of(setdiff(value_columns, ci_columns)) & dplyr::where(is.character),
        align_chr
      )
    )

  # Fit-index tables of lavaan models keep their note
  if (length(records) > 0 && all(purrr::map_lgl(records, \(r) identical(r$table, "fit")))) {
    attr(d, "apa_note") <- merge_fit_note_records(records)
  }
  as_apa7_tbl(d)
}

#' Use a pseudo-R2 as the R2 column
#'
#' `performance` names pseudo-R2 columns by type (e.g., `R2_Tjur`). Copies the
#' first one found into `R2` and labels the `R2` and `deltaR2` headers with the
#' type, unless `R2` already exists or the headers were customized.
#' @keywords internal
#' @noRd
use_pseudo_r2 <- function(data, column_formats) {
  pseudo <- c(
    R2_Tjur = "Tjur",
    R2_Nagelkerke = "Nagelkerke",
    R2_McFadden = "McFadden",
    R2_CoxSnell = "Cox\u2013Snell",
    # Mixed models: variance explained by the fixed effects
    R2_marginal = "marginal"
  )
  found <- intersect(names(pseudo), colnames(data))
  if ("R2" %in% colnames(data) || length(found) == 0) {
    return(list(data = data, column_formats = column_formats))
  }
  found <- found[1]
  data$R2 <- data[[found]]
  label <- pseudo[[found]]
  for (cl in c("R2", "deltaR2")) {
    if (
      !is.null(column_formats[[cl]]) &&
        identical(column_formats[[cl]]@header, the$columns[[cl]]@header)
    ) {
      column_formats[[cl]]@header <- paste0(
        column_formats[[cl]]@header,
        "~",
        label,
        "~"
      )
      column_formats[[cl]]@latex <- sub(
        "\\$$",
        paste0("_{\\\\text{", label, "}}$"),
        column_formats[[cl]]@latex
      )
    }
  }
  # Define the pseudo-R2 in table notes, unless a definition was given
  definitions <- c(
    R2_Tjur = paste0("Tjur's coefficient of discrimination ", cite("tjurCoefficientsDeterminationLogistic2009")),
    R2_Nagelkerke = paste0("Nagelkerke's pseudo-*R*^2^ ", cite("nagelkerkeNoteGeneralDefinition1991")),
    R2_McFadden = paste0("McFadden's pseudo-*R*^2^ ", cite("mcfaddenConditionalLogitAnalysis1974"))
  )
  if (
    found %in% names(definitions) &&
      !is.null(column_formats$R2) &&
      length(column_formats$R2@abbreviation) == 0
  ) {
    column_formats$R2@abbreviation <- definitions[[found]]
  }
  # The change in R2 names the same pseudo-R2
  if (
    !is.null(column_formats$deltaR2) && !is.null(column_formats$R2) &&
      identical(column_formats$deltaR2@abbreviation, the$columns$deltaR2@abbreviation)
  ) {
    column_formats$deltaR2@abbreviation <- paste0("change in ", column_formats$R2@header)
  }
  list(data = data, column_formats = column_formats)
}

#' Does a model have a fixed dispersion parameter?
#'
#' Nested models with fixed dispersion (binomial, Poisson, negative binomial,
#' and ordinal cumulative link models) are compared with likelihood ratio tests
#' rather than F tests.
#' @keywords internal
#' @noRd
has_fixed_dispersion <- function(fit) {
  # Nested mixed models are also compared with likelihood ratio tests
  if (inherits(fit, c("polr", "clm", "multinom", "zeroinfl", "hurdle", "merMod", "glmmTMB", "lme"))) {
    return(TRUE)
  }
  if (!inherits(fit, "glm")) {
    return(FALSE)
  }
  family <- stats::family(fit)$family
  family %in% c("binomial", "poisson") ||
    startsWith(family, "Negative Binomial")
}

#' format model comparison metrics in APA style
#'
#' @param ... model fit objects, or a list of them. Models are labeled by the list's names (or the argument names), and "Model 1", "Model 2", ... for unnamed models.
#' @param metrics performance metrics. If `NULL` (default), R2, deltaR2, F, and p for models with an estimated dispersion parameter (e.g., `lm`, Gaussian or quasi-family `glm`), and R2, deltaR2, Chi2, df, and p for GLMs with a fixed dispersion parameter (binomial, Poisson, negative binomial). Use "all" for all available metrics.
#' @param starred columns to star with significant p_values
#' @details
#' Nested models are compared with *F* tests ([performance::test_wald()]) unless every model is a GLM with a fixed dispersion parameter, in which case likelihood ratio tests ([performance::test_likelihoodratio()]) are used. For GLMs, `R2` is the pseudo-*R*^2^ that [performance::r2()] selects for the model family, and the header names it (e.g., *R*^2^~Tjur~).
#'
#' Mixed models (`lme4::lmer` and `lme4::glmer`) are compared with likelihood ratio tests. `R2` is the marginal *R*^2^ (variance explained by the fixed effects), and by default the conditional *R*^2^ (fixed and random effects) and its change (`R2_conditional`, `deltaR2_conditional`) are shown too, because adding random effects changes the conditional but not the marginal *R*^2^.
#'
#' Nested `lavaan` models (e.g., the configural, metric, and scalar models of a measurement invariance analysis) are compared each with the one before it, so they should be listed from the least to the most constrained. Each row shows the fit of the model (any of the metrics of [apa_performance.lavaan()]), followed, under a "Change from previous model" spanner, by the chi-square difference test from [lavaan::lavTestLRT()] (`"deltaChi2"`, `"deltaChi2_df"`, `"p_deltaChi2"`) and the changes in fit indices (`"deltaCFI"`, `"deltaTLI"`, `"deltaRMSEA"`, `"deltaSRMR"`), shown with three decimals. The default metrics are `"Chi2"`, `"Chi2_df"`, `"CFI"`, `"RMSEA"`, `"RMSEA_CI"`, `"SRMR"`, `"deltaChi2"`, `"deltaChi2_df"`, `"p_deltaChi2"`, `"deltaCFI"`, and `"deltaRMSEA"`. With a robust estimator (e.g., MLR or WLSMV), the difference test is scaled (e.g., the Satorra-Bentler scaled difference test), and the changes in fit indices are computed from their robust versions. [apa_note()] names the difference test. `starred` is ignored for `lavaan` models.
#' @param column_formats column_formats object to format columns. If NULL, the default column_formats is used.
#'
#' @return tibble
#' @export
#'
#' @examples
#' m1 <- lm(mpg ~ cyl, data = mtcars)
#' m2 <- lm(mpg ~ cyl + wt, data = mtcars)
#' apa_performance_comparison(list(`Model 1` =m1, `Model 3` =m2)) |>
#'    apa_flextable()
#'
#' # Measurement invariance across schools
#' \donttest{
#' if (requireNamespace("lavaan", quietly = TRUE)) {
#'   model <- "visual =~ x1 + x2 + x3
#'             textual =~ x4 + x5 + x6
#'             speed =~ x7 + x8 + x9"
#'   hs <- lavaan::HolzingerSwineford1939
#'   apa_performance_comparison(
#'     Configural = lavaan::cfa(model, data = hs, group = "school"),
#'     Metric = lavaan::cfa(model, data = hs, group = "school",
#'                          group.equal = "loadings"),
#'     Scalar = lavaan::cfa(model, data = hs, group = "school",
#'                          group.equal = c("loadings", "intercepts"))
#'   )
#' }
#' }
apa_performance_comparison <- function(
  ...,
  metrics = NULL,
  starred = NA,
  column_formats = NULL
) {
  R2 <- Model <- pstar <- df <- df_diff <- Name <- NULL

  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }

  .dots <- rlang::list2(...)

  # Models may be passed individually or as a single (unclassed) list
  models <- .dots
  if (length(.dots) == 1 && is.list(.dots[[1]]) && !is.object(.dots[[1]])) {
    models <- .dots[[1]]
  }
  if (length(models) > 0 && all(purrr::map_lgl(models, \(m) inherits(m, "lavaan")))) {
    return(lavaan_performance_comparison(models, metrics, column_formats))
  }
  use_lrt <- all(purrr::map_lgl(models, has_fixed_dispersion))
  # Mixed models have marginal and conditional R2
  is_mixed <- all(purrr::map_lgl(models, \(m) inherits(m, c("merMod", "glmmTMB", "lme"))))
  conditional_r2 <- if (is_mixed) c("R2_conditional", "deltaR2_conditional")

  if (is.null(metrics)) {
    metrics <- if (use_lrt) {
      c("R2", "deltaR2", conditional_r2, "Chi2", "df", "p")
    } else {
      c("R2", "deltaR2", "F", "p")
    }
  }

  if (any(metrics == "all")) {
    metrics <- c(
      "R2",
      "deltaR2",
      conditional_r2,
      "R2_adjusted",
      "RMSE",
      # Sigma is fixed at 1 when dispersion is fixed
      if (!use_lrt) "Sigma",
      "AIC",
      "AICc",
      "BIC",
      if (use_lrt) "Chi2" else "F",
      "df",
      "p"
    )
  }

  if (length(.dots[[1]]) > 1) {

    cp <- performance::compare_performance(..., metrics = "all") |>
      tibble::as_tibble()
    # Multinomial models report McFadden's R2 as R2
    if (all(purrr::map_lgl(models, \(m) inherits(m, "multinom"))) && "R2" %in% colnames(cp)) {
      cp <- dplyr::rename(cp, R2_McFadden = "R2")
    }

    pr2 <- use_pseudo_r2(cp, column_formats)
    cp <- pr2$data
    column_formats <- pr2$column_formats

    # Likelihood ratio tests for fixed-dispersion GLMs, F tests otherwise
    t_nested <- if (use_lrt) {
      performance::test_likelihoodratio(...)
    } else {
      performance::test_wald(...)
    }
    t_nested <- tibble::as_tibble(t_nested) |>
      dplyr::select(-dplyr::any_of("Criterion"))

    if ("deltaR2" %in% metrics) {
      cp <- cp |>
        dplyr::mutate(deltaR2 = ifelse(is.na(dplyr::lag(R2)), R2, R2 - dplyr::lag(R2)))
    }
    if (
      "deltaR2_conditional" %in% metrics &&
        "R2_conditional" %in% colnames(cp)
    ) {
      r2c <- cp$R2_conditional
      cp$deltaR2_conditional <- ifelse(
        is.na(dplyr::lag(r2c)),
        r2c,
        r2c - dplyr::lag(r2c)
      )
    }
    cp <- cp |>
      dplyr::left_join(t_nested, by = dplyr::join_by(Name, Model))
  } else {
    cp <- performance::performance(.dots[[1]], metrics = "all")
  }

  starred <- starred[starred %in% colnames(cp)]
  if (length(starred > 0) && !is.na(starred) && (starred != "")) {
    p_empty <- is.na(cp$p)
    cp <- cp |>
      dplyr::mutate(pstar = p2stars(cp$p, superscript = TRUE)) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(starred),
          column_formats[[starred]]@formatter
        )
      ) |>
      tidyr::unite(
        {{ starred }},
        c({{ starred }}, pstar),
        na.rm = TRUE,
        sep = ""
      )
    cp[p_empty, starred] <- NA
    # Leave every change column blank where there is no test of the change
    delta_columns <- intersect(c("deltaR2", "deltaR2_conditional"), colnames(cp))
    if (any(starred %in% delta_columns)) {
      cp[p_empty, delta_columns] <- NA
    }
  }

  if ("Chi2" %in% colnames(cp) && all(c("df", "df_diff") %in% colnames(cp))) {
    # Chi-square tests report only the df of the difference
    cp$df <- align_chr(abs(cp$df_diff), accuracy = the$accuracy)
    cp$df[is.na(cp$df_diff)] <- NA
    cp$df_diff <- NULL
  } else if (all(c("df", "df_diff") %in% colnames(cp))) {
    cp <- cp |>
      dplyr::mutate(
        dplyr::across(c(df_diff, df), .fns = \(x) {
          xx <- align_chr(abs(x), accuracy = the$accuracy)
          xx[is.na(x)] <- NA
          xx
        })
      ) |>
      dplyr::mutate(df = paste0("(", df_diff, ", ", df, ")"))

    cp$df[is.na(cp$df_diff)] <- NA
    cp$df_diff <- NULL
  }


  # Use the names of a named list (or of named arguments) as model labels
  model_labels <- model_names(models)
  if (length(model_labels) != nrow(cp)) {
    model_labels <- paste("Model", seq_len(nrow(cp))) # nocov
  }

  out <- cp |>
    dplyr::select(Model = Name, dplyr::any_of(metrics)) |>
    dplyr::mutate(Model = model_labels) |>
    apa_format_columns(column_formats) |>
    as_apa7_tbl()
  # The column formats used (e.g., with the pseudo-R2 named), for the notes of
  # apa_parameters() tables of lists of models
  attr(out, "column_formats") <- column_formats
  out
}

#' Labels for a list of models
#'
#' Uses the list's names, and "Model 1", "Model 2", ... for unnamed elements.
#' @keywords internal
#' @noRd
model_names <- function(models) {
  default <- paste("Model", seq_along(models))
  labels <- names(models)
  if (is.null(labels)) {
    return(default)
  }
  unnamed <- is.na(labels) | labels == ""
  labels[unnamed] <- default[unnamed]
  labels
}

# apa_style <- function(x,
#                       font_family = NULL,
#                       font_size = 12,
#                       text_color = "black",
#                       border_color = "black",
#                       border_width = 0.5,
#                       line_spacing = 2,
#                       horizontal_padding = 3,
#                       table_align = "left",
#                       header_align_vertical = c("top", "middle", "bottom"),
#                       layout = "autofit",
#                       table_width = 0,
#                       markdown = TRUE,
#                       markdown_header = markdown,
#                       markdown_body = markdown,
#                       no_markdown_columns = NULL,
#                       no_markdown_columns_header = no_markdown_columns,
#                       separate_headers = TRUE
# ) {
#   UseMethod("apa_style")
# }

# apa_style.gt_tbl <- function(
#     x,
#     font_family = NULL,
#     font_size = 12,
#     text_color = "black",
#     border_color = "black",
#     border_width = 0.5,
#     line_spacing = 2,
#     horizontal_padding = 3,
#     table_align = "left",
#     header_align_vertical = c("top", "middle", "bottom"),
#     layout = "autofit",
#     table_width = 0,
#     markdown = TRUE,
#     markdown_header = markdown,
#     markdown_body = markdown,
#     separate_headers = TRUE) {
#   if (is.null(font_family)) {
#     font_family <- the$font_family
#     }
#
#   pd <- (line_spacing - 1) * font_size * 2 / 3
#   k <- nrow(x$`_data`)
#
#   x |>
#     gt::tab_style(
#       gt::cell_borders(
#         style = "solid",
#         sides = c("bottom", "top"),
#         color = border_color,
#         weight = gt::px(border_width)),
#       locations = gt::cells_column_labels()) |>
#     gt::tab_style(
#       style = gt::cell_borders(
#         style = "hidden",
#         sides = "bottom"),
#       locations = gt::cells_body()) |>
#     gt::tab_options(
#       heading.border.bottom.color = border_color,
#       column_labels.border.bottom.color = border_color,
#       column_labels.border.bottom.width = border_width,
#       column_labels.border.top.color = border_color,
#       column_labels.border.top.width = border_width,
#       table.border.bottom.width = 0,
#       table.border.top.width = 0,
#       table_body.border.bottom.width = 0,
#       table_body.border.top.width = 0,
#       table_body.hlines.width = 0,
#       stub.border.width = 0,
#       row_group.border.top.width = 0,
#       heading.border.bottom.width = 0,
#       summary_row.border.width = 0,
#       footnotes.border.bottom.width = 0,
#       row_group.border.bottom.width = 0,
#       source_notes.border.bottom.width = 0,
#       stub_row_group.border.width = 0,
#       grand_summary_row.border.width = 0,
#       quarto.disable_processing = TRUE
#     ) |>
#     gt::tab_style(
#       style = gt::cell_borders(
#         sides = "bottom",
#         color = border_color,
#         style = "solid",
#         weight = gt::px(border_width)),
#       locations = gt::cells_body(rows = k)) |>
#     gt::opt_table_font(
#       font = font_family,
#       color = text_color,
#       size = gt::px(font_size * 4 / 3)) |>
#     gt::tab_options(
#       data_row.padding = gt::px(pd),
#       heading.padding = gt::px(pd),
#       column_labels.padding = gt::px(pd),
#       footnotes.padding = gt::px(pd),
#       row_group.padding = gt::px(pd),
#       summary_row.padding  = gt::px(pd),
#       grand_summary_row.padding = gt::px(pd),
#       source_notes.padding = gt::px(pd),
#       data_row.padding.horizontal = gt::px(
#         horizontal_padding * 4 / 3),
#       heading.padding.horizontal = gt::px(
#         horizontal_padding * 4 / 3),
#       column_labels.padding.horizontal = gt::px(
#         horizontal_padding * 4 / 3),
#       footnotes.padding.horizontal = gt::px(
#         horizontal_padding * 4 / 3),
#       row_group.padding.horizontal = gt::px(
#         horizontal_padding * 4 / 3),
#       summary_row.padding.horizontal = gt::px(
#         horizontal_padding * 4 / 3),
#       grand_summary_row.padding.horizontal = gt::px(
#         horizontal_padding * 4 / 3),
#       source_notes.padding.horizontal = gt::px(
#         horizontal_padding * 4 / 3),
#       ) |>
#     gt::sub_missing(missing_text = "")
# }

#' Style `flextable::flextable` object according to APA style
#'
#' @param x object
#' @param font_family font family
#' @param font_size font size
#' @param text_color text color
#' @param border_color border color
#' @param border_width border width in pixels
#' @param line_spacing spacing between lines of the header and body. Table
#'   notes are single-spaced.
#' @param horizontal_padding horizontal padding (in pixels)
#' @param markdown apply markdown formatting to header and body
#' @param markdown_header apply markdown formatting to header
#' @param markdown_body apply markdown formatting to body
#' @param table_align table alignment ("left", "center", "right")
#' @param header_align_vertical vertical alignment of headers. Can be "top", "middle", or "bottom"
#' @param layout table layout ("autofit", "fixed")
#' @param table_width table width (in pixels, 0 for auto)
#' @param no_markdown_columns body columns that should not be treated as markdown
#' @param no_markdown_columns_header column headers that should not be treated as markdown
#' @param separate_headers separate headers into column spanner labels
#' @name apa_style
#' @return object
#' @export
#'
#' @examples
#' d <- data.frame(x = 1:3, y = 4:6)
#' flextable::flextable(d) |>
#'   apa_style()
apa_style <- function(
  x,
  font_family = NULL,
  font_size = 12,
  text_color = "black",
  border_color = "black",
  border_width = 0.5,
  line_spacing = 2,
  horizontal_padding = 3,
  table_align = "left",
  header_align_vertical = c("top", "middle", "bottom"),
  layout = "autofit",
  table_width = 0,
  markdown = TRUE,
  markdown_header = markdown,
  markdown_body = markdown,
  no_markdown_columns = NULL,
  no_markdown_columns_header = no_markdown_columns,
  separate_headers = TRUE
) {
  header_align_vertical <- match.arg(header_align_vertical)

  if (is.null(font_family)) {
    font_family <- the$font_family
  }

  myborder <- list(
    color = border_color,
    width = border_width,
    style = "solid"
  )

  pd <- (line_spacing - 1) * font_size * 2 / 3

  # Typst sizes a line from cap height to baseline (about 0.66em) instead of
  # ascender to descender (about 1.15em in Word), so add the difference.
  if (isTRUE(knitr::pandoc_to("typst"))) {
    pd <- pd + font_size * 0.245
  }

  break_keys <- x$col_keys[grepl("^apa7breakcolumn", x$col_keys)]
  break_labels <- rep(" ", length(break_keys))
  names(break_labels) <- break_keys

  star_keys <- x$col_keys[grepl("apa7starcolumn$", x$col_keys)]
  star_match <- x$col_keys %in% star_keys
  r_match <- c(star_match[-1], star_match[1])
  r_keys <- x$col_keys[r_match]

  listcolumn_keys <- x$col_keys[grepl("apa7listcolumn$", x$col_keys)]
  listcolumn_match <- x$col_keys %in% listcolumn_keys
  l_match <- rev(c(rev(listcolumn_match)[-1], rev(listcolumn_match)[1]))
  l_keys <- x$col_keys[l_match]
  listcolumn_labels <- l_keys |> `names<-`(listcolumn_keys)

  if (flextable::nrow_part(x, "header") == 1 && separate_headers) {
    x <- flextable::separate_header(x, split = "[_]")
  }

  x <- x |>
    flextable::font(part = "all", fontname = font_family) |>
    flextable::color(color = text_color, part = "all") |>
    flextable::border(
      border.top = myborder,
      border.bottom = myborder,
      part = "header"
    ) |>
    flextable::hline_bottom(part = "body", border = myborder) |>
    flextable::hline_bottom(part = "header", border = myborder) |>
    flextable::fontsize(part = "all", size = font_size) |>
    flextable::valign(part = "all", valign = "top") |>
    flextable::align(part = "header", align = "center") |>
    flextable::line_spacing(space = 1, part = "all") |>
    flextable::padding(
      padding.top = pd,
      padding.bottom = pd,
      padding.left = horizontal_padding,
      padding.right = horizontal_padding,
      part = "all"
    ) |>
    flextable::padding(
      padding.top = 2,
      padding.bottom = 0,
      padding.left = horizontal_padding,
      padding.right = horizontal_padding,
      part = "footer"
    ) |>
    flextable::padding(j = star_keys, padding.left = 0, part = "body") |>
    flextable::padding(j = r_keys, padding.right = 0, part = "body") |>
    flextable::padding(j = r_keys, padding.right = 0, part = "header") |>
    flextable::padding(j = listcolumn_keys, padding.right = 0, part = "body") |>
    flextable::padding(
      j = listcolumn_keys,
      padding.right = 0,
      part = "header"
    ) |>
    flextable::padding(j = l_keys, padding.left = 0, part = "body") |>
    flextable::padding(j = l_keys, padding.left = 0, part = "header") |>
    flextable::align(j = star_keys, align = "left") |>
    flextable::align(j = listcolumn_keys, align = "right") |>
    flextable::align(
      j = r_keys,
      align = "right",
      part = "body"
    ) |>
    flextable::align(
      j = r_keys,
      align = "right",
      part = "header"
    ) |>
    flextable::align(
      j = l_keys,
      align = "left",
      part = "header"
    ) |>
    flextable::align(
      j = l_keys,
      align = "left",
      part = "body"
    )

  if (length(break_labels) > 0) {
    x <- flextable::void(x, j = break_keys, part = "body") |>
      flextable::labelizor(
        part = "header",
        labels = break_labels
      )
  }

  if (length(listcolumn_keys) > 0) {
    x <- flextable::void(
      x,
      j = listcolumn_keys,
      part = "header"
    )
  }

  if (length(star_keys) > 0) {
    x <- flextable::void(
      x,
      j = star_keys,
      part = "header"
    )
  }

  x <- flextable::set_table_properties(
    x,
    layout = layout,
    align = table_align,
    width = table_width,
    opts_word = list(keep_with_next = TRUE)
  )

  if (markdown_header) {
    # Make sure content is not already formatted
    dt <- x$header$content$data
    hh <- character(0)
    md_keys <- x$col_keys[
      !(x$col_keys %in%
        unique(c(listcolumn_keys, break_keys, no_markdown_columns_header)))
    ]
    for (ch in md_keys) {
      if (!(inherits(dt$header$content$data[[1, ch]], "chunk"))) {
        hh <- c(hh, ch)
      }
    }
    if (length(hh) > 0) {
      x <- ftExtra::colformat_md(
        x,
        j = dplyr::all_of(!!hh),
        part = "header"
      )
    }
  }
  if (markdown_body) {
    dt <- x$body$content$data
    hh <- character(0)
    md_keys <- x$col_keys[
      !(x$col_keys %in%
        unique(c(listcolumn_keys, break_keys, no_markdown_columns)))
    ]
    for (ch in md_keys) {
      if (!(inherits(dt$body$content$data[[1, ch]], "chunk"))) {
        hh <- c(hh, ch)
      }
    }
    if (length(hh) > 0) {
      x <- ftExtra::colformat_md(
        x,
        j = dplyr::all_of(!!hh),
        part = "body"
      )
    }
  }
  if (x$col_keys[1] == "Variable" || x$col_keys[1] == "Predictor") {
    x <- flextable::align(x, j = 1, align = "left", part = "body") |>
      flextable::valign(valign = header_align_vertical, part = "header")
  }
  x
}

#' Use flextable::dim_pretty to fit column widths
#'
#' @param x flextable
#' @param table_width width of table
#' @param min_width minimum width of columns
#' @param unit Can be `in`, `cm`, or `mm`
#' @return flextable::flextable
#' @export
pretty_widths <- function(
  x,
  min_width = 0.05,
  unit = c("in", "cm", "mm"),
  table_width = 6.5
) {
  unit <- match.arg(unit, choices = c("in", "cm", "mm"))
  pwidth <- flextable::dim_pretty(x, unit = unit)$widths
  pwidth[pwidth < min_width] <- min_width
  if (is.null(table_width) || is.na(table_width) || table_width == 0) {} else {
    # Are there breaks and lists?
    break_keys <- grepl("^apa7breakcolumn", x$col_keys)
    listcolumn_keys <- x$col_keys[grepl("apa7listcolumn$", x$col_keys)]
    if (length(listcolumn_keys) > 0) {
      listcolumn_match <- match(listcolumn_keys, x$col_keys)
      l_match <- listcolumn_match + 1
      l_keys <- x$col_keys[l_match]
      listcolumn_width_before <- pwidth[listcolumn_match]
    }

    nbreaks <- sum(break_keys)
    pwidth[!break_keys] <- pwidth[!break_keys] /
      sum(pwidth[!break_keys]) *
      (table_width - nbreaks * min_width)
    pwidth[break_keys] <- min_width

    if (length(listcolumn_keys) > 0) {
      # Give extra width to column right of list columns
      listcolumn_width_after <- pwidth[listcolumn_match]
      pwidth[listcolumn_match] <- listcolumn_width_before
      pwidth[l_match] <- pwidth[l_match] +
        listcolumn_width_after -
        listcolumn_width_before
    }
  }
  x_props <- x$properties
  x_props$layout <- "fixed"
  x_props$width <- 1
  rlang::inject(flextable::set_table_properties(x, !!!x_props)) |>
    flextable::width(
      width = pwidth,
      unit = unit
    )
}

#' A wrapper for tidyr::pivot_wider that creates columns names as `name_variable` instead of `variable_name`
#'
#' The default for `names_vary` is "slowest" instead of the usual "fastest".
#' @inheritParams tidyr::pivot_wider
#' @return data.frame
#' @export
#' @examples
#' d <- data.frame(Model = paste0("Model","_", 1:2), t = 1:2, p = c(.05, .45))
#' pivot_wider_name_first(d, names_from = Model, values_from = c(t, p))
#' # For comparison:
#' tidyr::pivot_wider(d, names_from = Model, values_from = c(t, p))
pivot_wider_name_first <- function(
  data,
  ...,
  id_cols = NULL,
  id_expand = FALSE,
  names_from = name,
  names_prefix = "",
  names_sep = "_",
  names_sort = FALSE,
  names_vary = "slowest",
  names_expand = FALSE,
  names_repair = "check_unique",
  values_from = value,
  values_fill = NULL,
  values_fn = NULL,
  unused_fn = NULL
) {
  name <- value <- NULL
  nf <- tidyselect::eval_select(data = data, rlang::enquo(names_from)) |>
    names()

  if (length(nf) > 1) {
    stop("Only one names_from variable is allowed.")
  }

  tidyr::pivot_wider(
    data = data,
    ...,
    id_cols = {{ id_cols }},
    id_expand = id_expand,
    names_from = {{ names_from }},
    names_prefix = names_prefix,
    names_sep = names_sep,
    names_glue = paste0("{", nf, "}", "_{.value}"),
    names_sort = names_sort,
    names_vary = "slowest",
    names_expand = names_expand,
    names_repair = names_repair,
    values_from = {{ values_from }},
    values_fill = values_fill,
    values_fn = values_fn,
    unused_fn = unused_fn
  )
}
