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
  shifter <- ifelse(.before, 0, 1)
  keys <- R.utils::insert(
    colnames(d),
    ats = where_numbers + shifter,
    values = blank_names
  )
  d[, blank_names] <- NA
  d[, keys]
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
  keys <- R.utils::insert(
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
#' @param data data.frame or tibble with variables to be
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
#' @param ... <[`data-masking`][rlang::args_data_masking]> parameters passed to psych::corTest
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
  v_names <- colnames(data)
  v_seq <- seq(ncol(data))
  if (is.null(column_formats)) {
    column_formats <- column_formats()
  }

  if (bold_significant) {
    star_significant <- FALSE
  }

  ct <- psych::corTest(data, ...) |>
    suppressWarnings()

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
      if (
        significance_note &&
          (bold_significant || star_significant) &&
          length(k_p) > 0
      ) {
        if (bold_significant) {
          my_paragraph <- flextable::as_paragraph(
            flextable::as_i("Note. "),
            "Correlations significant at ",
            flextable::as_i("p"),
            " < ",
            formatC(p_value[1], digits = 2, format = "fg") |>
              gsub(pattern = "^0", replacement = ""),
            " are ",
            flextable::as_b("bolded"),
            "."
          )
        } else {
          my_paragraph <- ftExtra::as_paragraph_md(
            paste0(p_note[k_p], collapse = ". ")
          )
        }

        d <- d |>
          flextable::add_footer_lines(values = my_paragraph) |>
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
            space = line_spacing
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
        flextable::line_spacing(part = "footer", space = line_spacing) |>
        flextable::color(part = "footer", color = text_color)
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
#' @param no_format_columns Column name or tidyselect function. selected columns are not formatted
#' @param add_breaks_between_spanners add breaks between spanners if TRUE
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

    if (all(!is.na(row_title_border))) {
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
#' them.
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
  pad_re <- "[ - ]|&numsp;"

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
    ft <- ft |>
      flextable::align(i = rows, j = k, align = "left", part = "body") |>
      flextable::padding(
        i = rows,
        j = k,
        padding.left = padding_left,
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
#' lm(mpg ~ cyl + wt, data = mtcars) |>
#'    apa_parameters(
#'      predictor_parameters = c("Parameter", "Coefficient", "SE", "CI", "t", "df_error", "p"),
#'      vcov = "HC3"
#'    ) |>
#'    apa_flextable()
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
    t_with_df = t_with_df
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
    t_with_df = t_with_df
  )
}

#' @param thresholds for ordinal models, if `TRUE` (default), threshold (cutpoint) rows are displayed below the predictors.
#' @details
#' For ordinal regression models (`MASS::polr` and `ordinal::clm`), coefficients are tested with Wald *z* tests. (`parameters` reports *t* for `polr`, but the test is asymptotically normal, and `clm` reports *z* for the same model.) With a logit link, exponentiated coefficients are cumulative odds ratios, labeled *OR*. Threshold rows show only the estimate, its standard error, and its confidence interval, because their exponentiated values are not odds ratios, and testing whether a threshold equals 0 is not meaningful. Confidence intervals (`"CI"`, `"Exp_CI"`) are Wald *z* intervals, consistent with the *z* tests.
#' @export
#' @rdname apa_parameters
#' @examples
#' MASS::polr(poverty ~ gender + age, data = carData::WVS, Hess = TRUE) |>
#'    apa_parameters()
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
        c(header = "*OR*", latex = "$OR$")
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
    t_with_df = t_with_df
  )
}

#' @param random for mixed models, if `TRUE` (default), random-effect standard deviations and correlations (and the residual standard deviation, for linear mixed models) are displayed below the fixed effects.
#' @details
#' For mixed models fit with `lme4` (`lmer` and `glmer`), fixed effects are displayed as for `lm` and `glm` models. For linear mixed models, *t* tests and confidence intervals use Kenward–Roger degrees of freedom by default (`ci_method = "kenward"`, which requires the pbkrtest package); pass another `ci_method` (e.g., `"satterthwaite"`, which requires lmerTest, or `"wald"`) to change this. When the degrees of freedom differ across fixed effects, they are shown in a *df* column rather than in the *t* column header. Random-effect rows show only the estimated standard deviation (*SD*) or correlation (*r*), labeled by grouping factor.
#' @export
#' @rdname apa_parameters
#' @examples
#' if (requireNamespace("lme4", quietly = TRUE) &&
#'     requireNamespace("pbkrtest", quietly = TRUE)) {
#'   lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy) |>
#'     apa_parameters()
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
  ...
) {
  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }
  names(predictor_parameters) <- NULL

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

  if (random) {
    d_r <- parameters::model_parameters(fit, effects = "random", ci = NULL) |>
      tibble::as_tibble()
    is_cor <- startsWith(d_r$Parameter, "Cor ")
    # Correlations have no leading zero; SDs are formatted like coefficients
    coef_chr <- character(nrow(d_r))
    coef_chr[is_cor] <- signs::signs(
      d_r$Coefficient[is_cor],
      accuracy = column_formats@accuracy,
      trim_leading_zeros = TRUE
    )
    coef_chr[!is_cor] <- signs::signs(
      d_r$Coefficient[!is_cor],
      accuracy = column_formats@accuracy
    )
    fixed_chr <- signs::signs(d_b$Coefficient, accuracy = column_formats@accuracy)
    d_b$Coefficient <- fixed_chr
    d_b <- dplyr::bind_rows(
      d_b,
      tibble::tibble(
        Parameter = random_effect_labels(d_r$Parameter, d_r$Group, name_formatter),
        Coefficient = coef_chr
      )
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
    t_with_df = t_with_df
  )
}

#' Labels for random-effect parameters
#'
#' Converts `parameters` labels such as "SD (Days)", "Cor (Intercept~Days)",
#' and "SD (Observations)" into "Subject: *SD* (Days)",
#' "Subject: *r* (Constant, Days)", and "Residual *SD*".
#' @param parameter random-effect parameter labels from `parameters`
#' @param group grouping factor of each parameter
#' @param formatter formats term names (e.g., "(Intercept)" to "Constant")
#' @keywords internal
#' @noRd
random_effect_labels <- function(parameter, group, formatter) {
  terms <- stringr::str_match(parameter, "^(SD|Cor) \\((.*)\\)$")
  type <- terms[, 2]
  inner <- terms[, 3]
  purrr::pmap_chr(
    list(type, inner, group, parameter),
    \(type, inner, group, parameter) {
      if (is.na(type)) {
        return(parameter) # nocov
      }
      if (group == "Residual") {
        return("Residual *SD*")
      }
      parts <- strsplit(inner, "~", fixed = TRUE)[[1]]
      # parameters drops the parentheses around Intercept in these labels
      parts[parts == "Intercept"] <- "(Intercept)"
      parts <- formatter(parts)
      symbol <- if (type == "SD") "*SD*" else "*r*"
      paste0(
        formatter(group),
        ": ",
        symbol,
        " (",
        paste(parts, collapse = ", "),
        ")"
      )
    }
  )
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
#' @param label named character vector with `header` and `latex`
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
    return(c(header = "*OR*", latex = "$OR$"))
  }
  if (fam$link == "log") {
    if (grepl("poisson|negative binomial", family)) {
      return(c(header = "*IRR*", latex = "$IRR$"))
    }
    if (grepl("binomial", family)) {
      return(c(header = "*RR*", latex = "$RR$"))
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
  t_with_df = TRUE
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
  if ("CI" %in% colnames(d_b) && is.numeric(d_b$CI) && any(!is.na(d_b$CI))) {
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
  if (length(starred) > 0 && all(!is.na(starred)) && all(starred != "")) {
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
  if (length(bolded) > 0 && all(!is.na(bolded)) && all(bolded != "")) {
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

  d_b_new |>
    dplyr::select(dplyr::any_of(pr_selector)) |>
    as_apa7_tbl()
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
  # Mixed models also show the conditional R2 and its change by default
  if (
    missing(performance_parameters) &&
      all(purrr::map_lgl(fit, \(m) inherits(m, "merMod")))
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
  d_fit <- purrr::map_df(
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
    },
    .id = "Model"
  )

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

  if (
    !(is.null(performance_parameters) ||
      length(performance_parameters) == 0 ||
      all(is.na(performance_parameters)))
  ) {
    d_fit <- d_fit |>
      flextable::as_grouped_data(groups = "Model") |>
      dplyr::left_join(
        apa_performance_comparison(
          fit,
          metrics = performance_parameters,
          starred = starred
        ),
        by = dplyr::join_by(Model)
      ) |>
      dplyr::mutate(dplyr::across(.cols = dplyr::where(is.character), \(x) dplyr::replace_values(x, from = NA, to = "")))



  }
  as_apa7_tbl(d_fit)
}

#' print loadings
#'
#' @inheritParams apa_parameters
#' @param sort_loading sort table using `psych::fa.sort`
#' @param min_loading minimum loading to display
#' @param complexity print complexity column in factor analysis table
#' @param uniqueness print uniqueness column in factor analysis table
#'
#' @returns tibble
#' @export
#' @examples
#' fit <- psych::fa(Harman74.cor$cov, 4, fm = "pa",rotate = "varimax")
#' apa_loadings(fit) |>
#'   `colnames<-`(c("Tests", "Verbal", "Spatial", "Quantitative", "Memory")) |>
#'   apa_flextable()
apa_loadings <- function(
  fit,
  sort_loading = TRUE,
  min_loading = 0.2,
  column_formats = NULL,
  complexity = FALSE,
  uniqueness = FALSE
) {
  Variable <- NULL
  x <- fit |>
    parameters::parameters(sort = sort_loading) |>
    tibble::as_tibble()

  if (!complexity) {
    x$Complexity <- NULL
  }

  if (!uniqueness) {
    x$Uniqueness <- NULL
  }

  x |>
    dplyr::mutate(
      dplyr::across(-Variable, \(x) {
        new_x <- align_chr(x, trim_leading_zeros = TRUE)
        if (!is.na(min_loading)) {
          new_x[abs(x) < min_loading] <- NA
        }

        new_x
      })
    )
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
#' For mixed models (`lme4::lmer` and `lme4::glmer`), the default metrics are the marginal *R*^2^ (variance explained by the fixed effects), the conditional *R*^2^ (fixed and random effects), and the intraclass correlation (ICC), as computed by [performance::model_performance()].
#' @export
#' @rdname apa_performance
apa_performance.merMod <- function(
  fit,
  metrics = c("R2_marginal", "R2_conditional", "ICC"),
  column_formats = NULL
) {
  apa_performance.lm(fit, metrics = metrics, column_formats = column_formats)
}

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
  d <- purrr::map_df(
    fit,
    \(f1) {
      args <- list(f1, column_formats = column_formats)
      if (!is.null(metrics)) {
        args$metrics <- metrics
      }
      tibble::as_tibble(do.call(apa_performance, args))
    },
    .id = "Model"
  )
  # Each model was aligned separately; re-align values across models
  d |>
    dplyr::mutate(
      dplyr::across(
        !dplyr::all_of("Model") & dplyr::where(is.character),
        align_chr
      )
    ) |>
    as_apa7_tbl()
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
    R2_CoxSnell = "Cox–Snell",
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
  if (inherits(fit, c("polr", "clm", "merMod"))) {
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
  use_lrt <- all(purrr::map_lgl(models, has_fixed_dispersion))
  # Mixed models have marginal and conditional R2
  is_mixed <- all(purrr::map_lgl(models, \(m) inherits(m, "merMod")))
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

  cp |>
    dplyr::select(Model = Name, dplyr::any_of(metrics)) |>
    dplyr::mutate(Model = model_labels) |>
    apa_format_columns(column_formats) |>
    as_apa7_tbl()
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
#' @param line_spacing spacing between lines
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
    flextable::line_spacing(space = line_spacing, part = "footer") |>
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
      if (!("chunk" %in% class(dt$header$content$data[[1, ch]]))) {
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
      if (!("chunk" %in% class(dt$body$content$data[[1, ch]]))) {
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
