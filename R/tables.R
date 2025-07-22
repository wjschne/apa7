#' Add break columns
#'
#' @param d data.frame or tibble
#' @param ... unquoted variable names, an expression using a tidyselect function (contains, start_with, ends_with, matches, num_range, all_of, any_of), or a character vector with variable names
#' @param .before insert break columns before selected columns (defaults to FALSE)
#' @param omit_first omit the first break column
#' @param omit_last omit the last break column
#'
#' @returns data.frame or tibble
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

  .dots <- rlang::expr(...)
  if (all(sapply(.dots, is.character))) {
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

#' Add columns that separate significance stars from numbers
#'
#' @param data data.frame or tibble
#' @param ... column names or tidyselect function
#' @param superscript make stars superscript
#' @param star character to use for stars (default: "\\*")
#' @param star_replace character to replace stars with (default: "\\\\*")
#'
#' @returns data.frame or tibble
#' @export
#'
#' @examples
#' tibble::tibble(x = c(".45", ".58*", ".68**"),
#'                y = c(1,2,3),
#'                z = 4:6) |>
#'                add_star_column(x)
add_star_column <- function(data, ..., superscript = TRUE, star = "\\*", star_replace = "\\\\*") {
  .dots <- rlang::expr(...)
  apa7rownumber <- apa7starvalue <- apa7starname <- apa7starcolumn <- NULL

  if (missing(.dots)) .dots = colnames(data)
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
#' @param x A two-column data.frame or tibble
#' @param note Custom note (overrides automatic note.)
#' @inheritParams apa_style
#' @param suppress_warnings Suppress any warnings if true.
#'
#' @returns flextable
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' apa_chisq(mtcars[, c("am", "gear")])
apa_chisq <- function(x,
                      note = NULL,
                      font_family = NULL,
                      font_size = 12,
                      text_color = "black",
                      line_spacing = 2,
                      border_color = "black",
                      border_width = .5,
                      suppress_warnings = TRUE
) {
  if (is.null(font_family)) font_family = the$font_family
  if (!inherits(x, "data.frame")) stop("x must be a data.frame or tibble.")
  if (ncol(x) != 2) stop('x must have 2 columns. Select the 2 variables you wish to test before passing them to this function. For example:\nx <- mtcars[, c("am", "cyl")]\nor\nx <- dplyr::select(mtcars, am, cyl)')

  tbl <- table(x)

  if (nrow(tbl) < 2 || ncol(tbl) < 2) stop("The contingency table must be at least a 2 by 2 table.")

  if (suppress_warnings) {
    fit <- suppressWarnings(stats::chisq.test(tbl))
  } else {
    fit <- stats::chisq.test(tbl)
  }

  ef <- effectsize::effectsize(fit, type = "cramers_v")

  dd <- tbl |>
    tibble::as_tibble() |>
    dplyr::mutate(`%` = scales::percent(.data$n / sum(.data$n), accuracy = .1), .by = dplyr::all_of(colnames(x)[1]),
                  n = as.character(.data$n)) |>
    tidyr::pivot_longer(c(.data$n, .data$`%`)) |>
    tidyr::unite("mycols", dplyr::all_of(colnames(x)[1]), .data$name) |>
    tidyr::pivot_wider(names_from = .data$mycols, values_from = .data$value)
  my_keys <- purrr::imap(colnames(dd), \(xx, idx) {
    if (stringr::str_ends(xx, "_%")) {
      xx <- c(xx, paste0("mybreak", idx))
    }
    xx
  }) |>
    unlist()
  my_keys <- my_keys[-length(my_keys)]
  my_headers <- list()
  my_headers[my_keys] <- c("", rep(colnames(x)[1], length(my_keys) - 1))
  my_border <- flextable::fp_border_default(color = border_color, width = border_width)

  dd <- dd |>
    flextable::flextable(col_keys = my_keys) |>
    flextable::valign(part = "all", valign = "center") |>
    flextable::separate_header() |>
    flextable::align(part = "all", align = "center") |>
    flextable::add_header(values = my_headers) |>
    flextable::merge_h(i = 1, part = "header") |>
    flextable::border_remove() |>
    flextable::hline(i = 1, j = 2:3, part = "header", border = my_border) |>
    flextable::empty_blanks() |>
    flextable::hline_bottom(border = my_border) |>
    flextable::hline_bottom(part = "header", border = my_border) |>
    flextable::hline(i = 2, part = "header", border = my_border) |>
    flextable::hline_top(part = "header", border = my_border) |>
    flextable::italic(i = 3, j = seq(2, length(my_keys), 3), italic = TRUE, part = "header")

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
    flextable::font(part = "all",fontname = font_family) |>
    flextable::color(color = text_color, part = "all") |>
    flextable::fontsize(part = "all", size = font_size) |>
    flextable::line_spacing(space = line_spacing, part = "all") |>
    flextable::padding(padding.top = 0, padding.bottom = 0, part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::autofit(add_w = 0, add_h = 0, part = "footer")
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
#' @param output output type. Can be "flextable" "gt" or "tibble"
#' @param summary_functions A named list of functions that summarize data columns (e.g., mean, sd)
#' @param keep_empty_star_columns Keep remove empty star columns (Default: `TRUE`)
#' @inheritParams apa_style
#' @param ... <[`data-masking`][rlang::args_data_masking]> parameters passed to psych::corTest
#' @importFrom rlang .data
#'
#' @returns flextable, gt, or tibble
#' @export
#'
#' @examples
#' apa_cor(mtcars[, c("mpg", "am", "gear", "carb")], output = "flextable")
#' apa_cor(mtcars[, c("mpg", "am", "gear", "carb")], output = "gt")
#' apa_cor(mtcars[, c("mpg", "am", "gear", "carb")], output = "tibble")
apa_cor <- function(data,
                    note = NULL,
                    p_value = c(.05, .01, .001),
                    digits = 2,
                    bold_significant = TRUE,
                    star_significant = FALSE,
                    significance_note = TRUE,
                    output = c("flextable", "gt", "tibble"),
                    font_family = NULL,
                    font_size = 12,
                    text_color = "black",
                    border_color = "black",
                    border_width = 0.5,
                    line_spacing = 2,
                    keep_empty_star_columns = TRUE,
                    summary_functions = list(M = mean,
                                             SD = stats::sd),
                    ...

) {
  value <- name <- Variable <- NULL
  if (is.null(font_family)) font_family = the$font_family
  output <- match.arg(output)
  v_names <- colnames(data)
  v_seq <- seq(ncol(data))

  if (star_significant) {
    bold_significant <- FALSE
  }

  ct <- psych::corTest(data, ...) |>
    suppressWarnings()

  R <- apply(X = ct$r,
             MARGIN = 2,
             FUN = signs::signs,
             accuracy = .1 ^ digits,
             trim_leading_zeros = TRUE)

  max_char_R <- max(nchar(R))

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

  if (star_significant) {
    my_p <- c(ct$p)
    p <- character(length = length(my_p))

    p_note <- character(length(p_value))
    ii <- 0
    for (pv in p_value) {
      ii <- ii + 1
      p_note[ii] <- paste0(
        "^",
        paste0(rep("\\*",ii), collapse = ""),
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
        Variable = paste0(
          dplyr::row_number(),
          ". ",
          .data$Variable))
  } else {
    d_msd <- tibble::tibble(Variable = v_names) |>
      dplyr::mutate(
        Variable = paste0(
          dplyr::row_number(),
          ". ",
          .data$Variable))
  }

  colnames(R) <- v_seq

  if (star_significant) {
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

  max_char_summary_data <- apply(d_msd, 2, \(x) max(nchar(x)))
  max_char_summary_columns <- nchar(colnames(d_msd))
  max_char_summary <- rbind(max_char_summary_data,
                            max_char_summary_columns) |>
    apply(2,max)

  max_char <- c(max_char_summary,
                rep(max_char_R, ncol(R)))

  max_char[max_char < 3] <- 3
  column_percent <- 100 * max_char / sum(max_char)

  k_functions <- ncol(d_msd)
  d_msd <- apa_format_columns(d_msd)

  d_R <- dplyr::bind_cols(d_msd, R) |>
    dplyr::mutate(
      Variable = stringr::str_replace(
        Variable, "\\. ", ".\u00A0"))

  attr(d_R, "cortest") <- ct

  if (output == "flextable") {
    flextable::set_flextable_defaults(font.family = font_family)
    d <-
      flextable::flextable(d_R) |>
      apa_style(font_family = font_family,
                font_size = font_size,
                text_color = text_color,
                border_color = border_color,
                border_width = border_width,
                line_spacing = line_spacing) |>
      flextable::align(align = "center",
                       part = "all") |>
      flextable::align(j = "Variable",
                       align = "left",
                       part = "all") |>
      flextable::align(j = -1,
                       align = "right",
                       part = "all") |>
      flextable::align(j = star_names,
                       align = "left",
                       part = "all")

    if (star_significant) {
      d <- d |>
        flextable::mk_par(j = star_names,
                          value = flextable::as_paragraph(""),
                          part = "header") |>
        flextable::padding(j = star_names,
                           padding.left = 0) |>
        flextable::padding(j = as.character(v_seq),
                           padding.right = 0)
    }

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

    d_R <- flextable::autofit(d, add_w = 0, add_h = 0)

  }

  if (output == "gt") {
    lstarname <- rep("", length(star_names)) |>
      `names<-`(star_names) |>
      as.list()

    d <-  gt::gt(d_R) |>
      apa_style(
        font_family = font_family,
        font_size = font_size,
        text_color = text_color,
        border_color = border_color,
        border_width = border_width,
        line_spacing = line_spacing
      ) |>
      gt::cols_align(align = "right", columns = -"Variable") |>
      gt::cols_align(align = "left", columns = star_names)

    d <- rlang::inject(gt::cols_label(d, !!!lstarname)) |>
      gt::tab_style(
        locations = gt::cells_column_labels(colnames(d_msd)[-1]),
        style = gt::cell_text(style = "italic")
      )  |>
      gt::fmt_markdown(columns = -1)

    if (is.null(note)) {
      if (significance_note && any(ct$p <= p_value)) {
        my_note <- paste0(
          "*Note*. Correlations significant at *p* < ",
          gsub(
            pattern = "^0",
            replacement = "",
            x = formatC(p_value, digits = 2, format = "fg")
          ),
          " are **bolded**."
        )
        d <- gt::tab_footnote(d, gt::md(my_note))
      }
    } else {
      my_note <- paste0("*Note*. ", note)
      d <- gt::tab_footnote(d, gt::md(my_note))
    }

    d <- d |>
      gt::tab_style(
        style = gt::cell_text(
          font = font_family,
          size = gt::px(font_size * 4 / 3),
          color = text_color),
        locations = gt::cells_footnotes()
      ) |>
      gt::tab_style(
        style = gt::cell_text(
          indent = gt::px(5),
          color = text_color),
        locations = list(
          gt::cells_column_labels(columns = "Variable"),
          gt::cells_body(columns = "Variable"),
          gt::cells_footnotes()
        )
      ) |>
      gt::tab_style(
        style = gt::cell_text(
          font = font_family,
          size = gt::px(font_size * 4 / 3),
          color = text_color),
        locations = list(
          gt::cells_column_labels(),
          gt::cells_body(),
          gt::cells_footnotes()
        ))

    if (star_significant) {
      d <- d |>
        gt::tab_style(
          style = "padding-left:0px",
          locations = gt::cells_body(star_names)) |>
        gt::tab_style(
          style = "padding-right:0px",
          locations = gt::cells_body(
            as.character(v_seq[-length(v_seq)])))
    }

    k <- nrow(d$`_boxhead`)

    d$`_boxhead`[seq(1, k),
                 "column_width"] <- gt::pct(column_percent)
    d_R <- d
  }

  d_R
}

#' Make flextable with merged row titles according to selected column
#'
#' @param data data.frame or tibble
#' @param row_title_column quoted column name to group rows
#' @param row_title_prefix text to be added to each title
#' @param row_title_sep separator for prefix
#' @param row_title_align alignment of row title ("left", "center", "right")
#' @param row_title_border list of flextable styles
#' @param col_keys column keys passed to flextable (defaults data column names)
#' @param cwidth initial cell width in inches
#' @param cheight initial cell height in inches
#' @param apastyle apply `apa_style` function (default: `TRUE`)
#' @param separate_headers separate header rows (default: `TRUE`)
#' @param auto_format_columns if true, will attempt to format some columns automatically
#' @param parameter_formatter an apa_parameter_formatter object
#' @inheritParams apa_style
#' @param ... arguments passed to `apa_style`

#' @returns flextable
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
#'   apa_flextable(row_title_column= "vs",  row_title_align = "center") |>
#'   align(j = 2:3, align = "center")
#'
apa_flextable <- function(data,
                          row_title_column = NULL,
                          row_title_prefix = "",
                          row_title_sep = " ",
                          row_title_align = "center",
                          row_title_border = list(
                            color = "gray20",
                            style = "solid",
                            width = 1
                          ),
                          col_keys = colnames(data),
                          cwidth = .75,
                          cheight = .25,
                          separate_headers = TRUE,
                          apastyle = TRUE,
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
                          parameter_formatter = NULL,
                          ...) {
  column_n <- row_title <- value <- name <- newname <- NULL

  if (is.null(parameter_formatter)) parameter_formatter <- the$parameter_formatter

  if (row_title_prefix == "")  {
    row_title_sep <- ""
  }

  data <- dplyr::mutate(
    data,
    dplyr::across(dplyr::where(\(x) rlang::is_integerish(x) & !is.factor(x)), as.integer))

  if (auto_format_columns) {
    cn_before <- colnames(data)
    data <- apa_format_columns(data, parameter_formatter = parameter_formatter)
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

  if (is.null(row_title_column)) {
    ft <- flextable::flextable(
      d,
      col_keys = col_keys[!(col_keys %in% row_title_column)],
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

  if (apastyle) {
    ft <- apa_style(ft,
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
                    ...)
  }

  if ("row_title" %in% colnames(d)) {
    ft <- flextable::align(ft, i = ~ !is.na(row_title),
                           align = row_title_align,
                           part = "body")
  } else(
    ft <- flextable::align(ft, j = 1, part = "body"
    )
  )

  ft
}

#' format model parameters in APA style
#'
#' @param fit model fit object
#' @param predictor_parameters predictor parameters to display. If named vector, column names will be vector names
#' @param starred columns to star with significant p_values
#' @param bolded columns to bold, if significant
#' @param parameter_formatter parameter_formatter object to format columns. If NULL, the default parameter_formatter is used.
#' @param t_with_df if TRUE, the t column will be displayed with degrees of freedom in parentheses. If FALSE, only the t value is displayed.
#'
#' @returns tibble
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
    parameter_formatter = NULL,
    t_with_df = TRUE) {
  UseMethod("apa_parameters")
}

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
      "p"),
   starred = NA,
   bolded = NA,
   parameter_formatter = NULL,
   t_with_df = TRUE
   ) {
  pstar <- NULL
  Parameter <- Std_Coefficient <- df_error <- header <- name <- NULL
  if (is.null(parameter_formatter)) parameter_formatter <- the$parameter_formatter

  if (!("Parameter" %in% predictor_parameters)) {
    predictor_parameters <- c("Parameter", predictor_parameters)
  }

  names(predictor_parameters) <- NULL

  d_b <- parameters::model_parameters(fit) |>
    tibble::as_tibble() |>
    dplyr::mutate(Parameter = parameters::format_parameters(fit))

  if ("Std_Coefficient" %in% predictor_parameters) {
    d_std <- parameters::standardise_parameters(fit, method = "basic") |>
      tibble::as_tibble() |>
      dplyr::mutate(Parameter = parameters::format_parameters(fit)) |>
      dplyr::select(Parameter, Std_Coefficient)

    d_b <- d_b |>
      dplyr::left_join(d_std, by = dplyr::join_by(Parameter))
  }

  d_b_new <- d_b

  if (t_with_df) {

  if (all(c("t", "df_error") %in% colnames(d_b)) &&
      !is.null(parameter_formatter$CI) && ("t" %in% predictor_parameters)) {
    df <- paste0("(", align_chr(max(d_b$df_error, na.rm = TRUE), accuracy = the$accuracy), ")")
    parameter_formatter$t@header <- paste0(parameter_formatter$t@header, df)

    if (stringr::str_detect(parameter_formatter$t@latex, "\\$")) {
      parameter_formatter$t@latex <- stringr::str_replace(
        parameter_formatter$t@latex,
        "\\$$",
        paste0(df, "$"))
    } else {
      parameter_formatter$t@latex <- paste0(parameter_formatter$t@latex, df)
    }

    d_b_new <- d_b_new |>
      dplyr::select(-df_error)
  }}

  for (pr in predictor_parameters) {
    if (!is.null(parameter_formatter[[pr]])) {
      # formals(mean)
      d_b_new <- dplyr::mutate(
        d_b_new,
        dplyr::across(
          dplyr::any_of(pr),
          parameter_formatter[[pr]]@formatter))
    }
  }

  if (length(starred > 0) & !is.na(starred) & (starred != "")) {
    d_b_new <- d_b_new |>
      dplyr::mutate(pstar = p2stars(d_b$p, superscript = T)) |>
      tidyr::unite({{ starred }}, c({{ starred}}, pstar), na.rm = T, sep = "") |>
      dplyr::mutate(dplyr::across(dplyr::any_of(starred), align_chr))
  }

  if (length(bolded > 0) & !is.na(bolded) & (bolded != "")) {
    my_bold <- ifelse(d_b$p <= 0.05, "**", "")
    d_b_new <- d_b_new |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(bolded),
          \(x) tagger(x, my_bold)))
  }

  prs <- parameter_formatter@get_tibble

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
      "p"),
    starred = NA,
    bolded = NA,
    parameter_formatter = NULL,
    t_with_df = TRUE) {
  if (is.null(names(fit))) {
    names(fit) <- paste("Model", seq(1, length(fit)))
  }
  purrr::map_df(fit, \(f1) apa_parameters(f1, predictor_parameters = predictor_parameters, parameter_formatter = parameter_formatter), .id = "Model")
}

#' format model performance metrics in APA style
#'
#' @param fit model fit object
#' @param metrics performance metrics. Default is R2 and Sigma
#' @param parameter_formatter parameter_formatter object to format columns. If NULL, the default parameter_formatter is used.
#'
#' @returns tibble
#' @export
#'
#' @examples
#' lm(mpg ~ cyl + wt, data = mtcars) |>
#'    apa_performance() |>
#'    apa_flextable()
apa_performance <- function(
    fit,
    metrics = c("R2", "Sigma"),
    parameter_formatter = NULL) {
  UseMethod("apa_performance")
}

#' @export
#' @rdname apa_performance
apa_performance.lm <- function(
    fit,
    metrics = c("R2", "Sigma"),
    parameter_formatter = NULL) {
  header <- name <- NULL

  if (is.null(parameter_formatter)) parameter_formatter <- the$parameter_formatter

  names(metrics) <- NULL

  prs <- parameter_formatter@get_tibble

  d <- performance::model_performance(fit) |>
    tibble::as_tibble()

  d_new <- d

  for (pr in metrics) {
    if (!is.null(parameter_formatter[[pr]])) {
      d_new <- dplyr::mutate(
        d_new,
        dplyr::across(dplyr::any_of(pr),
                      parameter_formatter[[pr]]@formatter))
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
#' @param parameter_formatter parameter_formatter object to format columns. If NULL, the default parameter_formatter is used.
#'
#' @returns tibble
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
    parameter_formatter = NULL) {
  R2 <- Model <- pstar <- df <- df_diff <- Name <- NULL

  if (is.null(parameter_formatter)) {
    parameter_formatter <- the$parameter_formatter
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
       dplyr::mutate(pstar = p2stars(cp$p, superscript = T)) |>
       dplyr::mutate(
         dplyr::across(dplyr::any_of(starred),
                       parameter_formatter[[starred]]@formatter)) |>
       tidyr::unite(
         {{ starred }},
         c({{ starred }}, pstar),
         na.rm = T,
         sep = "")
     cp[p_empty, starred] <- NA
   }

   if (all(c("df", "df_diff") %in% colnames(cp))) {
     cp <- cp |>
       dplyr::mutate(dplyr::across(c(df, df_diff), .fns = \(x) {
         xx <- align_chr(abs(x), accuracy = the$accuracy)
         xx[is.na(x)] <- NA
         xx

       })) |>
       tidyr::unite(df, c(df, df_diff), sep = ",")
   }

 cp |>
   dplyr::select(Model = Name, dplyr::any_of(metrics)) |>
   apa_format_columns(parameter_formatter)
}

#' Style object with APA style
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
#' @param layout table layout ("autofit", "fixed")
#' @param table_width table width (in pixels, 0 for auto)
#' @name apa_style
#' @returns object
#' @export
#'
#' @examples
#' d <- data.frame(x = 1:3, y = 4:6)
#' flextable::flextable(d) |>
#'   apa_style()
#' gt::gt(d) |>
#'   apa_style()
apa_style <- function(x,
                      font_family = NULL,
                      font_size = 12,
                      text_color = "black",
                      border_color = "black",
                      border_width = 0.5,
                      line_spacing = 2,
                      horizontal_padding = 3,
                      table_align = "left",
                      layout = "autofit",
                      table_width = 0,
                      markdown = TRUE,
                      markdown_header = markdown,
                      markdown_body = markdown) {
  UseMethod("apa_style")
}

#' @export
#' @rdname apa_style
apa_style.gt_tbl <- function(
    x,
    font_family = NULL,
    font_size = 12,
    text_color = "black",
    border_color = "black",
    border_width = 0.5,
    line_spacing = 2,
    horizontal_padding = 3,
    table_align = "left",
    layout = "autofit",
    table_width = 0,
    markdown = TRUE,
    markdown_header = markdown,
    markdown_body = markdown) {
  if (is.null(font_family)) {
    font_family = the$font_family
    }

  pd <- (line_spacing - 1) * font_size * 2 / 3
  k <- nrow(x$`_data`)

  x |>
    gt::tab_style(
      gt::cell_borders(
        style = "solid",
        sides = c("bottom", "top"),
        color = border_color,
        weight = gt::px(border_width)),
      locations = gt::cells_column_labels()) |>
    gt::tab_style(
      style = gt::cell_borders(
        style = "hidden",
        sides = "bottom"),
      locations = gt::cells_body()) |>
    gt::tab_options(
      heading.border.bottom.color = border_color,
      column_labels.border.bottom.color = border_color,
      column_labels.border.bottom.width = border_width,
      column_labels.border.top.color = border_color,
      column_labels.border.top.width = border_width,
      table.border.bottom.width = 0,
      table.border.top.width = 0,
      table_body.border.bottom.width = 0,
      table_body.border.top.width = 0,
      table_body.hlines.width = 0,
      stub.border.width = 0,
      row_group.border.top.width = 0,
      heading.border.bottom.width = 0,
      summary_row.border.width = 0,
      footnotes.border.bottom.width = 0,
      row_group.border.bottom.width = 0,
      source_notes.border.bottom.width = 0,
      stub_row_group.border.width = 0,
      grand_summary_row.border.width = 0,
      quarto.disable_processing = TRUE
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom",
        color = border_color,
        style = "solid",
        weight = gt::px(border_width)),
      locations = gt::cells_body(rows = k)) |>
    gt::opt_table_font(
      font = font_family,
      color = text_color,
      size = gt::px(font_size * 4 / 3)) |>
    gt::tab_options(
      data_row.padding = gt::px(pd),
      heading.padding = gt::px(pd),
      column_labels.padding = gt::px(pd),
      footnotes.padding = gt::px(pd),
      row_group.padding = gt::px(pd),
      summary_row.padding  = gt::px(pd),
      grand_summary_row.padding = gt::px(pd),
      source_notes.padding = gt::px(pd),
      data_row.padding.horizontal = gt::px(
        horizontal_padding * 4 / 3),
      heading.padding.horizontal = gt::px(
        horizontal_padding * 4 / 3),
      column_labels.padding.horizontal = gt::px(
        horizontal_padding * 4 / 3),
      footnotes.padding.horizontal = gt::px(
        horizontal_padding * 4 / 3),
      row_group.padding.horizontal = gt::px(
        horizontal_padding * 4 / 3),
      summary_row.padding.horizontal = gt::px(
        horizontal_padding * 4 / 3),
      grand_summary_row.padding.horizontal = gt::px(
        horizontal_padding * 4 / 3),
      source_notes.padding.horizontal = gt::px(
        horizontal_padding * 4 / 3),
      ) |>
    gt::sub_missing(missing_text = "")
}

#' @export
#' @rdname apa_style
apa_style.flextable <- function(
    x,
    font_family = NULL,
    font_size = 12,
    text_color = "black",
    border_color = "black",
    border_width = 0.5,
    line_spacing = 2,
    horizontal_padding = 3,
    table_align = "left",
    layout = "autofit",
    table_width = 0,
    markdown = TRUE,
    markdown_header = markdown,
    markdown_body = markdown) {

  if (is.null(font_family)) {
    font_family = the$font_family
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

  if (flextable::nrow_part(x, "header") == 1) {
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
                       padding.left = 0) |>
    flextable::padding(j = r_keys,
                       padding.right = 0) |>
    flextable::padding(j = r_keys,
                       padding.right = 0,
                       part = "header") |>
    flextable::align(j = star_keys, align = "left") |>
    flextable::align(
      j = r_keys,
      align = "right") |>
    flextable::align(
      j = r_keys,
      align = "right",
      part = "header")

  if (length(break_labels) > 0) {
    x <- flextable::void(x,
                         j = break_keys,
                         part = "body") |>
      flextable::labelizor(
        part = "header",
        labels = break_labels)
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
    for (ch in x$col_keys) {
      if (!("chunk" %in% class(dt$header$content$data[[1,ch]]))) {
        hh <- c(hh, ch)
      }
    }
    if (length(hh) > 0) {
      x <- ftExtra::colformat_md(x, j = dplyr::all_of(!!hh), part = "header")
      }
    }
  if (markdown_body) {
    dt <- x$body$content$data
    hh <- character(0)
    for (ch in x$col_keys) {
      if (!("chunk" %in% class(dt$body$content$data[[1,ch]]))) {
        hh <- c(hh, ch)
      }
    }
    if (length(hh) > 0) x <- ftExtra::colformat_md(x, j = dplyr::all_of(!!hh), part = "body")
    }
  if (x$col_keys[1] == "Variable" || x$col_keys[1] == "Predictor") {
    x <- flextable::align(x, j = 1, align = "left", part = "body")
  }
  x
}

#' Use flextable::dim_pretty to fit column widths
#'
#' @param x flextable
#' @param table_width width of table
#' @param min_width minimum width of columns
#' @param unit Can be `in`, `cm`, or `mm`
#' @returns flextable
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
    pwidth <- pwidth / sum(pwidth) * table_width
  }
  flextable::set_table_properties(x, layout = "fixed", width = 1) |>
    flextable::width(
      width = pwidth,
      unit = unit)

}
