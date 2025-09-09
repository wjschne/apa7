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
add_break_columns <- function(d,
                              ...,
                              .before = FALSE,
                              omit_first = FALSE,
                              omit_last = FALSE) {

  .dots <- rlang::exprs(...)
  if (all(purrr::map_lgl(.dots, is.character))) {
    where_names <- colnames(d)[colnames(d) %in% unlist(.dots)]
  } else {
    where_names <- colnames(dplyr::select(d, ...))
  }



  if (omit_first && length(where_names) > 0) {
    where_names <- where_names[-1]
  }

  if (omit_last && length(where_names) > 0) {
    where_names <- where_names[-length(where_names)]
  }

  if (length(where_names) == 0) return(d)



  blank_names <- paste0("apa7breakcolumn", seq_along(where_names))

  where_numbers <- seq(1, ncol(d))[colnames(d) %in% where_names]
  shifter <- ifelse(.before, 0, 1)
  keys <- R.utils::insert(colnames(d),
                          ats = where_numbers + shifter,
                          values = blank_names)
  d[,blank_names] <- NA
  d[,keys]

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
add_list_column <- function(data, ..., type = c("1", "a", "A", "I", "i"), sep = ".\u00A0", merge = FALSE) {
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
    if (type == "i") rmn <- tolower(rmn)
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
      data <- dplyr::relocate(data, vn, .before = n)
    }

  }
  data
}

#' @noRd
#' @keywords internal
rep_letters <- function(i, type = c("a", "A")) {
  # https://stackoverflow.com/a/25881167/4513316
  if (any(i < 1)) return(character(0))
  type <- match.arg(type)
  if (type == "a") letts <- letters else letts <- LETTERS
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
    prefix = "\\") {

    p_names <-  colnames(dplyr::select(data, {{ p }}))
    where_names <- colnames(dplyr::select(data, ...))


  if (length(p_names) == 0) {
    stop("There is no column of that name supplied to p")
  }

  if (length(where_names) == 0) {
    stop("There is no column of that name supplied to col")
  }

  star_names <- paste0(where_names, "apa7starcolumn")

  where_numbers <- seq(1, ncol(data))[colnames(data) %in% where_names]
  keys <- R.utils::insert(colnames(data),
                          ats = where_numbers + 1,
                          values = star_names)
  p_stars <- p2stars(data[[p_names]], first_alpha_marginal = first_alpha_marginal, superscript = superscript, alpha = alpha, add_trailing_space = add_trailing_space, prefix = prefix)

  for (wn in where_names) {
    sn <- paste0(wn, "apa7starcolumn")
    data[,sn] <- ifelse(data[[wn]] == "" | is.na(data[[wn]]), "", p_stars)
  }

  data <- data[,keys]

  if (merge) {
    for (wn in where_names) {
      sn <- paste0(wn, "apa7starcolumn")
      data[, wn] <- star_balance(
        paste0(data[[wn]],
               data[[sn]]))
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
    star_replace = "\\\\*") {
  .dots <- rlang::expr(...)
  apa7rownumber <- apa7starvalue <- apa7starname <- apa7starcolumn <- NULL

  if (missing(.dots)) .dots <- colnames(data)
  ss <- ifelse(superscript, "^", "")

  d <- data |>
    dplyr::mutate(dplyr::across({{ .dots }}, as.character)) |>
    dplyr::mutate(apa7rownumber = dplyr::row_number()) |>
    tidyr::pivot_longer(cols = c({{ .dots }}, -apa7rownumber),
                        names_to = "apa7starname",
                        values_to = "apa7starvalue") |>
    dplyr::mutate(
      apa7starcolumn = stringr::str_extract(
        apa7starvalue,
        paste0("[",star, "]+")) |>
        stringr::str_replace_all(star, star_replace),
      apa7starvalue = stringr::str_remove_all(apa7starvalue, star)) |>
    dplyr::mutate(apa7starcolumn = ifelse(
      is.na(apa7starcolumn),
      apa7starcolumn,
      paste0(ss, apa7starcolumn, ss))) |>
    tidyr::pivot_wider(names_from = apa7starname,
                       values_from = c(apa7starvalue,
                                       apa7starcolumn),
                       names_glue = "{apa7starname}_{.value}",
                       names_vary = "slowest") |>
    dplyr::select(-apa7rownumber) |>
    dplyr::rename_with(\(.x) stringr::str_remove(.x, "_apa7starvalue")) |>
    dplyr::rename_with(\(.x) stringr::str_replace(.x, "_apa7starcolumn", "apa7starcolumn"))

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
apa_chisq <- function(data,
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
  if (is.null(font_family)) font_family <- the$font_family
  if (!inherits(data, "data.frame")) stop("data must be a data.frame or tibble.")
  if (ncol(data) != 2) stop('data must have 2 columns. Select the 2 variables you wish to test before passing them to this function. For example:\nx <- mtcars[, c("am", "cyl")]\nor\nx <- dplyr::select(mtcars, am, cyl)')

  tbl <- table(data)

  if (nrow(tbl) < 2 || ncol(tbl) < 2) stop("The contingency table must be at least a 2 by 2 table.")

  if (suppress_warnings) {
    fit <- suppressWarnings(stats::chisq.test(tbl))
  } else {
    fit <- stats::chisq.test(tbl)
  }

  ef <- effectsize::effectsize(fit, type = "cramers_v")

  dd <- tbl |>
    tibble::as_tibble() |>
    dplyr::mutate(`%` = scales::percent(.data$n / sum(.data$n), accuracy = .1), .by = dplyr::all_of(colnames(data)[1]),
                  n = as.character(.data$n)) |>
    tidyr::pivot_longer(c(.data$n, .data$`%`)) |>
    tidyr::unite("mycols", dplyr::all_of(colnames(data)[1]), .data$name) |>
    tidyr::pivot_wider(names_from = .data$mycols, values_from = .data$value)
  col_keys <- colnames(dd)
  dd <- dd |>
    add_break_columns(dplyr::ends_with("_n"), .before = TRUE, omit_first = TRUE) |>
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
      dd <- dd |> flextable::add_footer_lines(ftExtra::as_paragraph_md(paste0("*Note*. ", note)))
    }

  } else {
    dd <- dd |> flextable::add_footer_lines(ftExtra::as_paragraph_md(
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
    ))
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
apa_cor <- function(data,
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
                    summary_functions = list(M = mean,
                                             SD = stats::sd),
                    column_formats = NULL,
                    ...

) {
  value <- name <- Variable <- k <- group <- width <- NULL
  if (is.null(font_family)) font_family <- the$font_family
  output <- match.arg(output)
  v_names <- colnames(data)
  v_seq <- seq(ncol(data))
  if (is.null(column_formats)) column_formats <- column_formats()

  if (bold_significant) {
    star_significant <- FALSE
  }

  ct <- psych::corTest(data, ...) |>
    suppressWarnings()

  R <- apply(X = ct$r,
             MARGIN = 2,
             FUN = signs::signs,
             accuracy = .1 ^ digits,
             trim_leading_zeros = TRUE)

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
        stringr::str_remove(as.character(pv), "^0"))

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
        dplyr::across(dplyr::everything(), \(x) max(nchar(x)))) |>
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
      dplyr::mutate(value = purrr::map(.data$value, \(fn) {
        new_fn <- purrr::possibly(\(x) fn(x, na.rm = TRUE))

        safe_fn <- function(x) {
          res <- new_fn(x)
          if (is.null(res)) {
            res <- fn(x)
          }
          res
        }

        data |>
          dplyr::summarise(
            dplyr::across(dplyr::everything(),
                          safe_fn))
      })) |>
      tidyr::pivot_wider() |>
      dplyr::reframe(
        dplyr::across(
          dplyr::everything(),
          unlist)) |>
      dplyr::mutate(Variable = v_names) |>
      dplyr::select(Variable,
                    dplyr::everything()) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.double),
          \(x) signs::signs(x, accuracy = .1 ^ digits))) |>
      dplyr::mutate(
        apa7listcolumn = paste0(
          dplyr::row_number(),
          ".\u2009"), .before = Variable)
  } else {
    d_msd <- tibble::tibble(Variable = v_names) |>
      dplyr::mutate(
        apa7listcolumn = paste0(
          dplyr::row_number(),
          ".\u2009"), .before = Variable)
  }

  colnames(R) <- v_seq

  if (star_significant && !bold_significant) {
    v_names_star <- rbind(
      v_seq,
      paste0(v_names,
             "apa7starcolumn")) |>
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

    d <- apa_flextable(d_R, font_family = font_family, font_size = font_size, text_color = text_color, border_color = border_color, border_width = border_width, line_spacing = line_spacing, auto_format_columns = FALSE)

    if (is.null(note)) {
      if (significance_note &&
          (bold_significant || star_significant) &&
          length(k_p) > 0) {

        if (bold_significant) {
          my_paragraph <- flextable::as_paragraph(
            flextable::as_i("Note. "),
            "Correlations significant at ",
            flextable::as_i("p"),
            " < ",
            formatC(p_value[1],
                    digits = 2,
                    format = "fg") |>
              gsub(pattern = "^0",
                   replacement = ""),
            " are ",
            flextable::as_b("bolded"),
            "."
          )

        } else {
          my_paragraph <- ftExtra::as_paragraph_md(paste0(p_note[k_p], collapse = ". "))
        }

        d <- d |>
          flextable::add_footer_lines(values = my_paragraph) |>
          flextable::font(
            part = "footer",
            fontname = font_family) |>
          flextable::fontsize(
            part = "footer",
            size = font_size) |>
          flextable::line_spacing(
            part = "footer",
            space = line_spacing) |>
          flextable::color(
            part = "footer",
            color = text_color)
      }

    } else {
      d <- flextable::add_footer_lines(
        x = d,
        values = ftExtra::as_paragraph_md(
          paste0("*Note*. ", note))) |>
        flextable::font(part = "footer",
                        fontname = font_family) |>
        flextable::fontsize(part = "footer",
                            size = font_size) |>
        flextable::line_spacing(part = "footer",
                                space = line_spacing) |>
        flextable::color(part = "footer",
                         color = text_color)
    }

    d_R <- d |>
      flextable::padding(j = "apa7listcolumn", padding.right = 0, padding.left = 0) |>
      flextable::padding(j = "Variable", padding.left = 0) |>
      flextable::align(align = "left", j = "Variable", part = "body") |>
      flextable::align(align = "left", part = "footer") |>
      flextable::align(align = "right", j = "apa7listcolumn") |>
      flextable::set_header_labels(apa7listcolumn = "Variable") |>
      flextable::merge_h_range(j1 = "apa7listcolumn", j2 = "Variable", part = "header") |>
      flextable::autofit(add_w = 0, add_h = 0) |>
      pretty_widths()


    w <- d_R$body$colwidths |>
      tibble::enframe() |>
      dplyr::mutate(group = dplyr::case_when(
        !is.na(suppressWarnings(as.integer(name))) ~ "r",
        stringr::str_ends(name, "apa7starcolumn") ~ "apa7starcolumn",
        name == "apa7listcolumn" ~ "apa7listcolumn",
        name == "Variable" ~ "Variable",
        TRUE ~ name
      )) |>
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
apa_flextable <- function(data,
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
                          ...) {
  # Prevent check warning
  column_n <- row_title <- value <- name <- newname <-
    cnames <- cnames_no_sep <- spanner_deckered <-
    deckered <- spanner <- name1 <- name <- value <-
    break_column <- NULL

  row_title_column <- dplyr::select(data, {{ row_title_column }}) |> colnames()

  header_align_vertical <- match.arg(header_align_vertical)

  no_markdown_columns_header <- dplyr::select(data, {{ no_markdown_columns_header }}) |> colnames()

  no_format_columns <- dplyr::select(data, {{ no_format_columns }}) |> colnames()



  # Get default column formats
  if (is.null(column_formats)) column_formats <- the$column_formats

  if (row_title_prefix == "")  {
    row_title_sep <- ""
  }

  # Use tidyselect to find column names
  numeric_columns <- dplyr::select(data, dplyr::where(is.numeric)) |> colnames()

  # if (!is.null(no_markdown_columns)) {
    no_markdown_columns <- dplyr::select(data, {{ no_markdown_columns }}) |> colnames()
  # } else {
    # no_markdown_columns <- character(0)
  # }
  no_markdown_columns <- unique(no_markdown_columns, numeric_columns)

  if (!is.null(no_markdown_columns_header)) {
  no_markdown_columns_header <- dplyr::select(data, {{ no_markdown_columns_header }} ) |> colnames()
  }

  # Insert breaks between column spanners
  if (add_breaks_between_spanners && separate_headers) {
    cf_names <- column_formats@get_column_names
    cf_names_no_sep <- stringr::str_replace_all(cf_names, "_", "apa7separator")
    names(cf_names_no_sep) <- cf_names

    brk_col <- tibble::tibble(cnames = colnames(data),
           cnames_no_sep = stringr::str_replace_all(cnames, cf_names_no_sep)) |>
      # Split on last underscore
      dplyr::mutate(spanner_deckered = purrr::map(cnames_no_sep, \(x) stringr::str_split_fixed(x, "_(?=[^_]*$)", 2))) |>
      dplyr::mutate(spanner = purrr::map_chr(spanner_deckered, 1),
                    deckered = purrr::map_chr(spanner_deckered, 2)) |>
      dplyr::mutate(break_column = deckered != "" & dplyr::lag(deckered) != "" & spanner != dplyr::lag(spanner)) |>
      dplyr::filter(break_column) |>
      dplyr::pull(cnames)

    if (length(brk_col) > 0) {
      data <- add_break_columns(data, brk_col, .before = TRUE)
    }
  }

  # Convert data that are like integers into integers
  data <- dplyr::mutate(
    data,
    dplyr::across(dplyr::where(\(x) rlang::is_integerish(x) & !is.factor(x)), as.integer))

  if (auto_format_columns) {
    cn_before <- colnames(data)
    data <- apa_format_columns(data, column_formats = column_formats, no_format_columns = no_format_columns)
    cn_after <- colnames(data)
    kv <- tibble::tibble(name = cn_before,
                         newname = cn_after)

    col_keys <- tibble::tibble(name = col_keys) |>
      dplyr::left_join(kv, by = dplyr::join_by(name)) |>
      dplyr::mutate(newname = ifelse(is.na(newname), name, newname)) |>
      dplyr::pull(newname)
  }

  is_docx <- knitr::pandoc_to("docx")

  if (is_docx) {
    d <- data
  } else {
    d <- dplyr::select(data, -dplyr::starts_with("apa7breakcolumn"))
  }



  if (length(row_title_column) == 0) {
    ft <- flextable::flextable(
      d,
      col_keys = col_keys,
      cwidth = cwidth,
      cheight = cheight,
      theme_fun = flextable::theme_apa)
  } else {
    d <- flextable::as_grouped_data(d, groups = row_title_column)

    d$row_title <- d[, row_title_column]
    d <- tidyr::fill(d, !!row_title_column)

    d <- d |>
      dplyr::mutate(column_n = dplyr::n(), .by = !!row_title_column) |>
      dplyr::filter(column_n > 2 | is.na(row_title )) |>
      dplyr::select(-column_n)

    ft <- flextable::flextable(
      d,
      col_keys = col_keys[!(col_keys %in% row_title_column)],
      cwidth = cwidth,
      cheight = cheight,
      theme_fun = flextable::theme_apa) |>
      flextable::mk_par(
        i = ~ !is.na(row_title),
        value = flextable::as_paragraph(row_title_prefix,
                                        row_title_sep , row_title)
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

  if (separate_headers) ft <- flextable::separate_header(ft,split = "[_]")
  if (!is_docx) {
    ft <- flextable::empty_blanks(ft)
  }

  ft <- flextable::align(ft, j = 1)
  center_me <- lapply(d, is_numeric_like) |> names()
  center_me <- center_me[center_me %in% ft$col_keys]
  if (length(center_me) > 0) {
    ft <- flextable::align(ft, j = center_me, align = "center")
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
    ft <- flextable::align(ft,
                           i = ~ !is.na(row_title),
                           align = row_title_align,
                           part = "body")
    if (row_title_align == "left") {
      ft <- flextable::padding(ft,
                               j = 1,
                               i = ~ is.na(row_title),
                               padding.left = left_column_padding
                               )
    }

  } else(
    ft <- flextable::align(ft, j = 1, part = "body"
    )
  )
  if (pretty_widths) {
    ft <- pretty_widths(ft, table_width = table_width * 6.5)
  }

  class(ft) <- unique(c("apaflextable", class(ft)))

  ft
}

#' format model parameters in APA style
#'
#' @param fit model fit object
#' @param predictor_parameters predictor parameters to display. If named vector, column names will be vector names
#' @param starred columns to star with significant p_values
#' @param bolded columns to bold, if significant
#' @param column_formats column_formats object to format columns. If NULL, the default column_formats is used.
#' @param t_with_df if TRUE, the t column will be displayed with degrees of freedom in parentheses. If FALSE, only the t value is displayed.
#'
#' @return tibble
#' @export
#'
#' @examples
#' lm(mpg ~ cyl + wt, data = mtcars) |>
#'    apa_parameters() |>
#'    apa_flextable()
apa_parameters <- function(
    fit,
    predictor_parameters = c("Coefficient",
                             "SE",
                             "Std_Coefficient",
                             "t",
                             "df_error",
                             "p"),
    starred = NULL,
    bolded = NULL,
    column_formats = NULL,
    t_with_df = TRUE) {
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
      "p"),
   starred = NA,
   bolded = NA,
   column_formats = NULL,
   t_with_df = TRUE
   ) {
  # Prevent warnings
  pstar <- Parameter <- Std_Coefficient <- df_error <- header <- name <- NULL

  # Get column formats
  if (is.null(column_formats)) column_formats <- the$column_formats

  # Make sure Parameter is in the predictor_parameters
  if (!("Parameter" %in% predictor_parameters)) {
    predictor_parameters <- c("Parameter", predictor_parameters)
  }

  names(predictor_parameters) <- NULL

  # Get and format parameters
  d_b <- parameters::model_parameters(fit) |>
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

  d_b_new <- d_b

  # Put df into t column name
  if (t_with_df) {

  if (all(c("t", "df_error") %in% colnames(d_b)) &&
      !is.null(column_formats$CI) && ("t" %in% predictor_parameters)) {
    df <- paste0("(", align_chr(max(d_b$df_error, na.rm = TRUE), accuracy = the$accuracy), ")")
    column_formats$t@header <- paste0(column_formats$t@header, df)

    if (stringr::str_detect(column_formats$t@latex, "\\$")) {
      column_formats$t@latex <- stringr::str_replace(
        column_formats$t@latex,
        "\\$$",
        paste0(df, "$"))
    } else {
      column_formats$t@latex <- paste0(column_formats$t@latex, df)
    }

    d_b_new <- d_b_new |>
      dplyr::select(-df_error)
  }}

  # Format columns
  for (pr in predictor_parameters) {
    if (!is.null(column_formats[[pr]])) {
      d_b_new <- dplyr::mutate(
        d_b_new,
        dplyr::across(
          dplyr::any_of(pr),
          column_formats[[pr]]@formatter))
    }
  }

  # add starred column
  if (length(starred > 0) && !is.na(starred) && all(starred != "")) {
    d_b_new <- d_b_new |>
      dplyr::mutate(pstar = p2stars(d_b$p, superscript = TRUE)) |>
      tidyr::unite({{ starred }}, c({{ starred}}, pstar),
                   na.rm = TRUE,
                   sep = "") |>
      dplyr::mutate(dplyr::across(dplyr::any_of(starred), align_chr))
  }

  # bold star
  if (length(bolded > 0) & !is.na(bolded) & (bolded != "")) {
    my_bold <- ifelse(d_b$p <= 0.05, "**", "")
    d_b_new <- d_b_new |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(bolded),
          \(x) tagger(x, my_bold)))
  }

  prs <- column_formats@get_tibble

  pr_selector <- tibble::tibble(name = predictor_parameters) |>
    dplyr::left_join(prs, by = "name") |>
    dplyr::select(header, name) |>
    dplyr::mutate(header = ifelse(is.na(header), name, header)) |>
    tibble::deframe()

  d_b_new |>
    dplyr::select(dplyr::any_of(pr_selector))
}

#' @export
#' @rdname apa_parameters
apa_parameters.list <- function(
    fit,
    predictor_parameters = c(
      "Parameter",
      "Coefficient",
      "SE",
      "Std_Coefficient",
      "t",
      "df_error",
      "p"),
    starred = NA,
    bolded = NA,
    column_formats = NULL,
    t_with_df = TRUE) {
  if (is.null(names(fit))) {
    names(fit) <- paste("Model", seq(1, length(fit)))
  }
  purrr::map_df(fit, \(f1) apa_parameters(f1, predictor_parameters = predictor_parameters, column_formats = column_formats, t_with_df = FALSE), .id = "Model")
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
    dplyr::mutate(dplyr::across(-Variable, \(x) {
      new_x <- align_chr(x, trim_leading_zeros = TRUE)
      if (!is.na(min_loading)) {
        new_x[abs(x) < min_loading] <- NA
      }

      new_x
    }))
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
    column_formats = NULL) {
  UseMethod("apa_performance")
}

#' @export
#' @rdname apa_performance
apa_performance.lm <- function(
    fit,
    metrics = c("R2", "Sigma"),
    column_formats = NULL) {
  if (any(metrics == "all")) {
    metrics <- c("AIC", "AICc", "BIC",
                "R2", "R2_adjusted",
                "RMSE",
                "Sigma")
  }

  header <- name <- NULL

  if (is.null(column_formats)) column_formats <- the$column_formats

  names(metrics) <- NULL

  prs <- column_formats@get_tibble

  d <- performance::model_performance(fit, metrics = "all") |>
    tibble::as_tibble()

  d_new <- d

  for (pr in metrics) {
    if (!is.null(column_formats[[pr]])) {
      d_new <- dplyr::mutate(
        d_new,
        dplyr::across(dplyr::any_of(pr),
                      column_formats[[pr]]@formatter))
    }
  }

  pr_selector <- tibble::tibble(name = metrics) |>
    dplyr::left_join(prs, by = "name") |>
    dplyr::select(header, name) |>
    dplyr::mutate(header = ifelse(
      is.na(header),
      name,
      header)) |>
    tibble::deframe()

  d_new |>
    dplyr::select(dplyr::any_of(pr_selector))
}

#' format model comparison metrics in APA style
#'
#' @param ... model fit objects
#' @param metrics performance metrics. Default is R2, deltaR2, F, and p
#' @param starred columns to star with significant p_values
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
    metrics = c("R2", "deltaR2", "F", "p"),
    starred = NA,
    column_formats = NULL) {

  if (any(metrics == "all")) {
    metrics <- c("R2", "deltaR2","R2_adjusted", "RMSE", "Sigma", "AIC", "AICc", "BIC", "F", "df","p")
  }

  R2 <- Model <- pstar <- df <- df_diff <- Name <- NULL

  if (is.null(column_formats)) {
    column_formats <- the$column_formats
    }

  .dots <- rlang::list2(...)

 t_wald <- performance::test_wald(...) |>
   tibble::as_tibble()

 cp <- performance::compare_performance(..., metrics = "all") |>
   tibble::as_tibble()

 if ("deltaR2" %in% metrics) {
   cp <- cp |>
     dplyr::mutate(
       deltaR2 = ifelse(
         is.na(dplyr::lag(R2)),
         R2,
         R2 - dplyr::lag(R2)))
 }

 cp <- cp |>
   dplyr::left_join(t_wald, by = dplyr::join_by(Name, Model))

   if (length(starred > 0) & !is.na(starred) & (starred != "")) {
     p_empty <- is.na(cp$p)
     cp <- cp |>
       dplyr::mutate(pstar = p2stars(cp$p, superscript = TRUE)) |>
       dplyr::mutate(
         dplyr::across(dplyr::any_of(starred),
                       column_formats[[starred]]@formatter)) |>
       tidyr::unite(
         {{ starred }},
         c({{ starred }}, pstar),
         na.rm = TRUE,
         sep = "")
     cp[p_empty, starred] <- NA
   }

   if (all(c("df", "df_diff") %in% colnames(cp))) {
     cp <- cp |>
       dplyr::mutate(dplyr::across(c(df_diff, df), .fns = \(x) {
         xx <- align_chr(abs(x), accuracy = the$accuracy)
         xx[is.na(x)] <- NA
         xx
       })) |>
       dplyr::mutate(df = paste0("(", df_diff,", ",  df, ")"))

     cp$df[is.na(cp$df_diff)] <- NA
     cp$df_diff <- NULL   }

 cp |>
   dplyr::select(Model = Name, dplyr::any_of(metrics)) |>
   apa_format_columns(column_formats)
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
    separate_headers = TRUE) {
  header_align_vertical <- match.arg(header_align_vertical)

  if (is.null(font_family)) {
    font_family <- the$font_family
    }

  myborder <- list(
    color = border_color,
    width = border_width,
    style = "solid")

  pd <- (line_spacing - 1) * font_size * 2 / 3

  break_keys <- x$col_keys[grepl("^apa7breakcolumn", x$col_keys)]
  break_labels <- rep(" ", length(break_keys))
  names(break_labels) <- break_keys

  star_keys <- x$col_key[grepl("apa7starcolumn$", x$col_keys)]
  star_match <- x$col_key %in% star_keys
  r_match <- c(star_match[-1], star_match[1])
  r_keys <- x$col_key[r_match]

  listcolumn_keys <- x$col_key[grepl("apa7listcolumn$", x$col_keys)]
  listcolumn_match <- x$col_key %in% listcolumn_keys
  l_match <- rev(c(rev(listcolumn_match)[-1], rev(listcolumn_match)[1]))
  l_keys <- x$col_key[l_match]
  listcolumn_labels <- l_keys |> `names<-`(listcolumn_keys)

  if (flextable::nrow_part(x, "header") == 1 && separate_headers) {
    x <- flextable::separate_header(x, split = "[_]")
  }

  x <- x |>
    flextable::font(part = "all",
                    fontname = font_family) |>
    flextable::color(color = text_color,
                     part = "all") |>
    flextable::border(border.top = myborder,
                      border.bottom = myborder,
                      part = "header") |>
    flextable::hline_bottom(part = "body",
                            border = myborder) |>
    flextable::hline_bottom(part = "header",
                            border = myborder) |>
    flextable::fontsize(part = "all",
                        size = font_size) |>
    flextable::valign(part = "all",
                      valign = "top") |>
    flextable::align(part = "header",
                     align = "center") |>
    flextable::line_spacing(space = 1,
                            part = "all") |>
    flextable::line_spacing(space = line_spacing,
                            part = "footer") |>
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
    flextable::padding(j = star_keys,
                       padding.left = 0,
                       part = "body") |>
    flextable::padding(j = r_keys,
                       padding.right = 0,
                       part = "body") |>
    flextable::padding(j = r_keys,
                       padding.right = 0,
                       part = "header") |>
    flextable::padding(j = listcolumn_keys,
                       padding.right = 0,
                       part = "body") |>
    flextable::padding(j = listcolumn_keys,
                       padding.right = 0,
                       part = "header") |>
    flextable::padding(j = l_keys,
                       padding.left = 0,
                       part = "body") |>
    flextable::padding(j = l_keys,
                       padding.left = 0,
                       part = "header") |>
    flextable::align(j = star_keys, align = "left") |>
    flextable::align(j = listcolumn_keys, align = "right") |>
    flextable::align(
      j = r_keys,
      align = "right",
      part = "body") |>
    flextable::align(
      j = r_keys,
      align = "right",
      part = "header") |>
    flextable::align(
      j = l_keys,
      align = "left",
      part = "header") |>
    flextable::align(
      j = l_keys,
      align = "left",
      part = "body")

  if (length(break_labels) > 0) {
    x <- flextable::void(x,
                         j = break_keys,
                         part = "body") |>
      flextable::labelizor(
        part = "header",
        labels = break_labels)
  }

  if (length(listcolumn_keys) > 0) {
    x <- flextable::void(
      x,
      j = listcolumn_keys,
      part = "header")
  }

  if (length(star_keys) > 0) {
    x <- flextable::void(
      x,
      j = star_keys,
      part = "header")

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
    md_keys <- x$col_keys[!(x$col_keys %in% unique(c(listcolumn_keys, break_keys, no_markdown_columns_header)))]
    for (ch in md_keys) {
      if (!("chunk" %in% class(dt$header$content$data[[1,ch]]))) {
        hh <- c(hh, ch)
      }
    }
    if (length(hh) > 0) {
      x <- ftExtra::colformat_md(
        x,
        j = dplyr::all_of(!!hh),
        part = "header")

      }
    }
  if (markdown_body) {
    dt <- x$body$content$data
    hh <- character(0)
    md_keys <- x$col_keys[!(x$col_keys %in% unique(c(listcolumn_keys, break_keys, no_markdown_columns)))]
    for (ch in md_keys) {
      if (!("chunk" %in% class(dt$body$content$data[[1,ch]]))) {
        hh <- c(hh, ch)
      }
    }
    if (length(hh) > 0) {
      x <- ftExtra::colformat_md(
        x,
        j = dplyr::all_of(!!hh),
        part = "body")
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
    table_width = 6.5) {
  unit <- match.arg(unit, choices = c("in", "cm", "mm"))
  pwidth <- flextable::dim_pretty(x, unit = unit)$widths
  pwidth[pwidth < min_width] <- min_width
  if (is.null(table_width) || is.na(table_width) || table_width == 0) {

  } else {
    # Are there breaks and lists?
    break_keys <- grepl("^apa7breakcolumn", x$col_keys)
    listcolumn_keys <-  x$col_keys[grepl("apa7listcolumn$", x$col_keys)]
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
      pwidth[l_match] <- pwidth[l_match] + listcolumn_width_after - listcolumn_width_before


    }

  }
  x_props <- x$properties
  x_props$layout <- "fixed"
  x_props$width <- 1
  rlang::inject(flextable::set_table_properties(x, !!!x_props)) |>
    flextable::width(
      width = pwidth,
      unit = unit)

}

#' A wrapper for tidyr::pivot_wider that creates columns names as `name_variable` instead of `variable_name`
#'
#' The default for `names_vary` is "slowest" instead of the usual "fastest".
#' @inheritParams tidyr::pivot_wider
#' @return data.frame
#' @export
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
    unused_fn = NULL) {
  name <- value <- NULL
  nf <- tidyselect::eval_select(data = data, rlang::enquo(names_from)) |> names()

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

