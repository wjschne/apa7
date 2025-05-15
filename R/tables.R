
#' Style object with APA style
#'
#' @param x object
#' @param family font family
#' @param font_size font size
#' @param text_color text color
#' @param border_color border color
#' @param border_width border width in pixels
#' @param line_spacing spacing between lines
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
                      family = "Times New Roman",
                      font_size = 12,
                      text_color = "black",
                      border_color = "black",
                      border_width = 0.5,
                      line_spacing = 2) {
  UseMethod("apa_style")
}

#' @export
#' @rdname apa_style
apa_style.gt_tbl <- function(x ,
                             family = "Times New Roman",
                             font_size = 12,
                             text_color = "black",
                             border_color = "black",
                             border_width = 0.5,
                             line_spacing = 2) {
  pd <- (line_spacing - 1) * font_size * 2 / 3
  d <- x |>
  gt::opt_table_lines("none") |>
    gt::tab_options(
      table.border.top.style = "solid",
      table.border.top.color = border_color,
      table.border.top.width = border_width,

      table_body.border.bottom.style = "solid",
      table_body.border.bottom.color = border_color,
      table_body.border.bottom.width = border_width,

      heading.border.bottom.style = "solid",
      heading.border.bottom.color = border_color,
      heading.border.bottom.width = border_width,

      table_body.border.top.style = "solid",
      table_body.border.top.color = border_color,
      table_body.border.top.width = border_width,

    ) |>
    gt::opt_table_font(font = family, color = text_color, size = gt::px(font_size * 4 / 3)) |>
    gt::tab_options(data_row.padding = gt::px(pd),
                    heading.padding = gt::px(pd),
                    column_labels.padding = gt::px(pd),
                    footnotes.padding = gt::px(pd),
                    row_group.padding = gt::px(pd),
                    summary_row.padding  = gt::px(pd),
                    grand_summary_row.padding = gt::px(pd),
                    source_notes.padding = gt::px(pd),
                    data_row.padding.horizontal = gt::px(2),
                    heading.padding.horizontal = gt::px(2),
                    column_labels.padding.horizontal = gt::px(2),
                    footnotes.padding.horizontal = gt::px(2),
                    row_group.padding.horizontal = gt::px(2),
                    summary_row.padding.horizontal = gt::px(2),
                    grand_summary_row.padding.horizontal = gt::px(2),
                    source_notes.padding.horizontal = gt::px(2),
                    ) |>
    gt::sub_missing(missing_text = "")
}

#' @export
#' @rdname apa_style
apa_style.flextable <- function(x ,
                                family = "Times New Roman",
                                font_size = 12,
                                text_color = "black",
                                border_color = "black",
                                border_width = 0.5,
                                line_spacing = 2) {
  myborder <- list(color = border_color, width = border_width, style = "solid")
  x |>
    flextable::font(part = "all",fontname = family) |>
    flextable::color(color = text_color, part = "all") |>
    flextable::border(border.top = myborder,
                      border.bottom = myborder,
                      part = "header") |>
    flextable::hline_bottom(part = "all", border = myborder) |>
    flextable::fontsize(part = "all", size = font_size) |>
    flextable::valign(part = "all", valign = "center") |>
    flextable::line_spacing(space = line_spacing, part = "all")


}


#' APA-formatted correlation table
#'
#' @param data data.frame or tibble with variables to be
#' @param p_value p-value needed to be flagged as significant
#' @param digits Number of digits for rounding
#' @param bold_significant bold significant correlations
#' @param significance_note If TRUE, place note at bottom of table that significant correlations are bolded.
#' @param output output type. Can be "flextable" "gt" or "tibble"
#' @param summary_functions A named list of functions that summarize data columns (e.g., mean, sd)
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
                    p_value = .05,
                    digits = 2,
                    bold_significant = TRUE,
                    significance_note = TRUE,
                    output = c("flextable", "gt", "tibble"),
                    family = "Times New Roman",
                    font_size = 12,
                    text_color = "black",
                    border_color = "black",
                    border_width = 0.5,
                    line_spacing = 2,
                    summary_functions = list(M = mean, SD = stats::sd),
                    ...

) {
  output <- match.arg(output)
  v_names <- colnames(data)
  v_seq <- seq(ncol(data))


  ct <- psych::corTest(data, ...)

  R <- ct$r |>
    apply(2, signs::signs, accuracy = .1 ^ digits, trim_leading_zeros = TRUE)

  max_char_R <- max(nchar(R))



  if (bold_significant) {
    p <- ifelse(c(ct$p) <= p_value, "**", "")
    R <- matrix(paste0(p, c(R), p), nrow = length(v_names))
    colnames(R) <- v_names
    rownames(R) <- v_names
  }


  R[upper.tri(R)] <- NA
  diag(R) <- "\u2014"

  R <- R |>
    tibble::as_tibble()

  if (length(summary_functions) > 0) {
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
          dplyr::summarise(dplyr::across(dplyr::everything(), safe_fn))
      })) |>
      tidyr::pivot_wider() |>
      dplyr::reframe(dplyr::across(dplyr::everything(), unlist)) |>
      dplyr::mutate(Variable = v_names) |>
      dplyr::select(.data$Variable, dplyr::everything()) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.double), \(x) signs::signs(x, accuracy = .1 ^ digits))) |>
      dplyr::mutate(Variable = paste0(dplyr::row_number(), ". ", .data$Variable))
  } else {
    d_msd <- tibble::tibble(Variable = v_names) |>
      dplyr::mutate(Variable = paste0(dplyr::row_number(), ". ", .data$Variable))
  }

  max_char_summary_data <- apply(d_msd, 2, \(x) max(nchar(x)))
  max_char_summary_columns <- nchar(colnames(d_msd))
  max_char_summary <- rbind(max_char_summary_data,
                            max_char_summary_columns) |>
    apply(2,max)

  max_char <- c(max_char_summary, rep(max_char_R, ncol(data)))
  max_char[max_char < 3] <- 3
  column_percent <- 100 * max_char / sum(max_char)

  k_functions <- ncol(d_msd)
  colnames(R) <- v_seq

  d_R <- dplyr::bind_cols(d_msd, R)

  attr(d_R, "cortest") <- ct

  if (output == "flextable") {
    flextable::set_flextable_defaults(font.family = family)
    d <-
      flextable::flextable(d_R) |>
      apa_style(family = family,
                font_size = font_size,
                text_color = text_color,
                border_color = border_color,
                border_width = border_width,
                line_spacing = line_spacing) |>
      flextable::align(j = "Variable", align = "left", part = "all") |>
      flextable::align(j = -1, align = "right", part = "all") |>
      flextable::italic(j = colnames(d_msd)[-1], part = "header", italic = TRUE) |>
      ftExtra::colformat_md(j = -"Variable")

    if (significance_note && any(ct$p <= p_value)) {
      my_paragraph <- flextable::as_paragraph(flextable::as_i("Note. "), "Correlations signficant at ", flextable::as_i("p"), " < ", formatC(p_value, digits = 2, format = "fg") |> gsub(pattern = "^0", replacement = ""), " are ", flextable::as_b("bolded"), ".")

      d <- d |>
        flextable::add_footer_lines(values = my_paragraph) |>
        flextable::font(part = "footer", fontname = family) |>
        flextable::fontsize(part = "footer", size = font_size) |>
        flextable::line_spacing(part = "footer", space = line_spacing) |>
        flextable::color(part = "footer", color = text_color)

    }


    d_R <- flextable::autofit(d, add_w = 0, add_h = 0)
  }

  if (output == "gt") {
    d <-  gt::gt(d_R) |>
      apa_style(
        family = family,
        font_size = font_size,
        text_color = text_color,
        border_color = border_color,
        border_width = border_width,
        line_spacing = line_spacing
      ) |>
      gt::cols_align(align = "right", columns = -"Variable") |>
      gt::tab_style(
        locations = gt::cells_column_labels(colnames(d_msd)[-1]),
        style = gt::cell_text(style = "italic")
      )  |>
      gt::fmt_markdown(columns = -1)
    if (significance_note && any(ct$p <= p_value)) {
      d <- d |>
        gt::tab_footnote(gt::md(
          paste0(
            "*Note*. Correlations significant at *p* < ",
            gsub(
              pattern = "^0",
              replacement = "",
              x = formatC(p_value, digits = 2, format = "fg")
            ),
            " are **bolded**."
          )
        )) |>
        gt::tab_style(
          style = gt::cell_text(font = family, size = gt::px(font_size * 4 / 3)),
          locations = gt::cells_footnotes()
        ) |>
        gt::tab_style(
          style = gt::cell_text(indent = gt::px(5)),
          locations = list(
            gt::cells_column_labels(columns = "Variable"),
            gt::cells_body(columns = "Variable"),
            gt::cells_footnotes()
          )
        ) |>
        gt::tab_style(style = "padding-right:5px",
                      locations = list(gt::cells_column_labels(columns = ncol(d_R)),
                                       gt::cells_body(columns = ncol(d_R))))
    }

    k <- nrow(d$`_boxhead`)

    d$`_boxhead`[seq(1, k), "column_width"] <- gt::pct(column_percent)
    d_R <- d

  }


  d_R
}


#' Make contingency table with chi-square test of independence
#'
#' @param x A two-column data.frame or tibble
#' @param suppress_warnings Suppress any warnings if true.
#' @inheritParams apa_style
#'
#' @returns flextable
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' apa_chisq(mtcars[, c("am", "gear")])
apa_chisq <- function(x, suppress_warnings = TRUE, family = "Times New Roman", font_size = 12, text_color = "black", line_spacing = 2, border_color = "black", border_width = .5) {
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


    dd |>
    flextable::flextable(col_keys = my_keys) |>
      apa_style(family = family, font_size = font_size, text_color = text_color, border_color = border_color, border_width = border_width, line_spacing = line_spacing ) |>
    flextable::separate_header() |>
    flextable::align(part = "all", align = "center") |>
      flextable::add_header(values = my_headers) |>
      flextable::merge_h(i = 1, part = "header") |>
      flextable::border_remove() |>
      flextable::hline(i = 1, j = 2:3, part = "header") |>
      flextable::empty_blanks() |>
      flextable::hline_bottom() |>
      flextable::hline_bottom(part = "header") |>
      flextable::hline(i = 2, part = "header") |>
      flextable::hline_top(part = "header") |>
      flextable::italic(i = 3, j = seq(2, length(my_keys), 3), italic = TRUE, part = "header") |>
      flextable::add_footer_lines(values = flextable::as_paragraph( flextable::as_equation(paste0("Note.~\\chi^2\\left(",fit$parameter,"\\right)=", scales::number(fit$statistic, accuracy = .01), ",\\,", apa_p(fit$p.value, inline = TRUE, plain = TRUE), ",\\,\\text{Adj. Cramer's}\\,V=", signs::signs(ef$Cramers_v_adjusted, accuracy = .01, trim_leading_zeros = TRUE)), props = flextable::fp_text_default(font.size = font_size * .85, font.family = family)))) |>
      flextable::autofit(add_w = 0, add_h = 0, part = "footer")
}


