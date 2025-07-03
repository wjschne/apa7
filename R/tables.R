
#' Style object with APA style
#'
#' @param x object
#' @param family font family
#' @param font_size font size
#' @param text_color text color
#' @param border_color border color
#' @param border_width border width in pixels
#' @param line_spacing spacing between lines
#' @param horizontal_padding horizontal padding (in pixels)
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
                      line_spacing = 2,
                      horizontal_padding = 3,
                      table_align = "left",
                      layout = "autofit",
                      table_width = 0) {
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
                             line_spacing = 2,
                             horizontal_padding = 3) {
  pd <- (line_spacing - 1) * font_size * 2 / 3
  k <- nrow(x$`_data`)

  x |>
    gt::tab_style(gt::cell_borders(style = "solid", sides = c("bottom", "top"),color = border_color, weight = gt::px(border_width)), locations = gt::cells_column_labels()) |>
    gt::tab_style(style = gt::cell_borders(style = "hidden", sides = "bottom"), locations = gt::cells_body()) |>
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
    gt::tab_style(style = gt::cell_borders(sides = "bottom", color = border_color, style = "solid", weight = gt::px(border_width)), locations = gt::cells_body(rows = k)) |>
    gt::opt_table_font(font = family, color = text_color, size = gt::px(font_size * 4 / 3)) |>
    gt::tab_options(data_row.padding = gt::px(pd),
                    heading.padding = gt::px(pd),
                    column_labels.padding = gt::px(pd),
                    footnotes.padding = gt::px(pd),
                    row_group.padding = gt::px(pd),
                    summary_row.padding  = gt::px(pd),
                    grand_summary_row.padding = gt::px(pd),
                    source_notes.padding = gt::px(pd),
                    data_row.padding.horizontal = gt::px(horizontal_padding * 4 / 3),
                    heading.padding.horizontal = gt::px(horizontal_padding * 4 / 3),
                    column_labels.padding.horizontal = gt::px(horizontal_padding * 4 / 3),
                    footnotes.padding.horizontal = gt::px(horizontal_padding * 4 / 3),
                    row_group.padding.horizontal = gt::px(horizontal_padding * 4 / 3),
                    summary_row.padding.horizontal = gt::px(horizontal_padding * 4 / 3),
                    grand_summary_row.padding.horizontal = gt::px(horizontal_padding * 4 / 3),
                    source_notes.padding.horizontal = gt::px(horizontal_padding * 4 / 3),
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
                                line_spacing = 2,
                                horizontal_padding = 3,
                                table_align = "left",
                                layout = "autofit",
                                table_width = 0) {
  myborder <- list(color = border_color, width = border_width, style = "solid")
  pd <- (line_spacing - 1) * font_size * 2 / 3

  break_keys <- x$col_keys[grepl("^breakcolumn", x$col_keys)]
  break_labels <- rep(" ", length(break_keys))
  names(break_labels) <- break_keys
  if (flextable::nrow_part(x, "header") == 1) {
    x <- x |>
      flextable::separate_header()
  }

  x <- x |>
    flextable::font(part = "all", fontname = family) |>
    flextable::color(color = text_color, part = "all") |>
    flextable::border(border.top = myborder,
                      border.bottom = myborder,
                      part = "header") |>
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
    )
  if (length(break_labels) > 0) {
    x <- flextable::void(x, j = break_keys, part = "body") |>
      flextable::labelizor(
        part = "header",
        labels = break_labels)

  }
  flextable::set_table_properties(x, layout = layout, align = table_align, width = table_width, opts_word = list(keep_with_next = TRUE))


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
                    p_value = .05,
                    digits = 2,
                    bold_significant = TRUE,
                    star_significant = FALSE,
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
  if (star_significant) {
    bold_significant <- FALSE
  }


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


  star_names <- paste0(v_names, "starcolumn")
  remove_star_columns <- star_names
  if (star_significant) {
    my_p <- c(ct$p)
    p <- character(length = length(my_p))

    p[my_p <= .05] <- "\\*"
    p[my_p <= .01] <- "\\*\\*"
    p[my_p <= .001] <- "\\*\\*\\*"
    Rstar <- matrix(p, nrow = length(v_names))

    colnames(Rstar) <- star_names
    rownames(Rstar) <- star_names
    Rstar[upper.tri(Rstar, diag = TRUE)] <- ""
    Rstar <- tibble::as_tibble(Rstar)
    remove_star_columns <- Rstar %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), \(x) max(nchar(x)))) %>%
      tidyr::pivot_longer(dplyr::everything()) %>%
      dplyr::filter(value == 0) %>%
      dplyr::pull(name)

  }

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
      dplyr::select(Variable, dplyr::everything()) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.double), \(x) signs::signs(x, accuracy = .1 ^ digits))) |>
      dplyr::mutate(Variable = paste0(dplyr::row_number(), ". ", .data$Variable))
  } else {
    d_msd <- tibble::tibble(Variable = v_names) |>
      dplyr::mutate(Variable = paste0(dplyr::row_number(), ". ", .data$Variable))
  }


  colnames(R) <- v_seq
  if (star_significant) {
    v_names_star <- rbind(v_seq, paste0(v_names, "starcolumn")) %>% c()

    R <- cbind(R, Rstar) |>
      dplyr::select(dplyr::all_of(v_names_star)) |>
      dplyr::select(-dplyr::all_of(remove_star_columns))



  }

  star_names <- star_names[!(star_names %in% remove_star_columns)]

  max_char_summary_data <- apply(d_msd, 2, \(x) max(nchar(x)))
  max_char_summary_columns <- nchar(colnames(d_msd))
  max_char_summary <- rbind(max_char_summary_data,
                            max_char_summary_columns) |>
    apply(2,max)

  max_char <- c(max_char_summary, rep(max_char_R, ncol(R)))
  max_char[max_char < 3] <- 3
  column_percent <- 100 * max_char / sum(max_char)

  k_functions <- ncol(d_msd)



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
      flextable::align(j = "Variable",
                       align = "left",
                       part = "all") |>
      flextable::align(j = -1,
                       align = "right",
                       part = "all") |>
      flextable::align(j = star_names,
                       align = "left",
                       part = "all") |>
      flextable::italic(j = colnames(d_msd)[-1],
                        part = "header",
                        italic = TRUE) |>
      ftExtra::colformat_md(j = -"Variable")

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
          any(ct$p <= p_value)) {

        if (bold_significant) {
          my_paragraph <- flextable::as_paragraph(
            flextable::as_i("Note. "),
            "Correlations significant at ",
            flextable::as_i("p"),
            " < ",
            formatC(p_value, digits = 2, format = "fg") |> gsub(pattern = "^0", replacement = ""),
            " are ",
            flextable::as_b("bolded"),
            "."
          )

        } else {
          my_paragraph <- flextable::as_paragraph(
            "*",
            flextable::as_i("p"),
            " < .05, **",
            flextable::as_i("p"),
            " < .01, ***",
            flextable::as_i("p"),
            " < .001"
          )
        }



        d <- d |>
          flextable::add_footer_lines(values = my_paragraph) |>
          flextable::font(part = "footer", fontname = family) |>
          flextable::fontsize(part = "footer", size = font_size) |>
          flextable::line_spacing(part = "footer", space = line_spacing) |>
          flextable::color(part = "footer", color = text_color)

      }

    } else {
      d <- flextable::add_footer_lines(
        x = d,
        values = ftExtra::as_paragraph_md(
          paste0("*Note*. ", note))) |>
        flextable::font(part = "footer", fontname = family) |>
        flextable::fontsize(part = "footer", size = font_size) |>
        flextable::line_spacing(part = "footer", space = line_spacing) |>
        flextable::color(part = "footer", color = text_color)

    }

    d_R <- flextable::autofit(d, add_w = 0, add_h = 0)

    }



  if (output == "gt") {
    lstarname <- rep("", length(star_names)) |> `names<-`(star_names)|> as.list()

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
          style = gt::cell_text(font = family, size = gt::px(font_size * 4 / 3), color = text_color),
          locations = gt::cells_footnotes()
        ) |>
        gt::tab_style(
          style = gt::cell_text(indent = gt::px(5), color = text_color),
          locations = list(
            gt::cells_column_labels(columns = "Variable"),
            gt::cells_body(columns = "Variable"),
            gt::cells_footnotes()
          )
        ) |>
        gt::tab_style(style = gt::cell_text(font = family, size = gt::px(font_size * 4 / 3), color = text_color), locations = list(
          gt::cells_column_labels(),
          gt::cells_body(),
          gt::cells_footnotes()
        ))

      if (star_significant) {
        d <- d |>
          gt::tab_style(style = "padding-left:0px", locations = gt::cells_body(star_names)) %>%
          gt::tab_style(style = "padding-right:0px", locations = gt::cells_body(as.character(v_seq[-length(v_seq)])))
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
apa_chisq <- function(x, note = NULL, family = "Times New Roman", font_size = 12, text_color = "black", line_spacing = 2, border_color = "black", border_width = .5, suppress_warnings = TRUE) {
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
      flextable::font(part = "all",fontname = family) |>
      flextable::color(color = text_color, part = "all") |>
      flextable::fontsize(part = "all", size = font_size) |>
      flextable::line_spacing(space = line_spacing, part = "all") |>
      flextable::padding(padding.top = 0, padding.bottom = 0, part = "all") |>
      flextable::align(j = 1, align = "left", part = "all") |>
      flextable::autofit(add_w = 0, add_h = 0, part = "footer")
}


#' Use flextable::dim_pretty to fit column widths
#'
#' @param x flextable
#' @param min_width minimum width of columns
#' @param unit Can be `in`, `cm`, or `mm`
#' @returns flextable
#' @export
pretty_widths <- function(x, min_width = .1, unit = c("in", "cm", "mm")) {
  unit <- match.arg(unit, choices = c("in", "cm", "mm"))
  pwidth <- flextable::dim_pretty(x, unit = unit)$widths
  pwidth[pwidth < min_width] <- min_width
  flextable::width(x, width = flextable::dim_pretty(x)$widths, unit = unit)
}


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



  blank_names <- paste0("breakcolumn", seq_along(where_names))

  where_numbers <- seq(1, ncol(d))[colnames(d) %in% where_names]
  shifter <- ifelse(.before, 0, 1)
  keys <- R.utils::insert(colnames(d),
                          ats = where_numbers + shifter,
                          values = blank_names)
  d[,blank_names] <- NA
  d[,keys]

}

#' Make flextable with merged row titles according to selected column
#'
#' @param data data.frame or tibble
#' @param column quoted column name to group rows
#' @param prefix to be added to each title
#' @param sep separator for prefix
#' @param ... arguments passed to flextable::flextable
#'
#' @returns flextable
#' @export
#'
#' @examples
#' library(dplyr)
#' mtcars %>%
#'   select(vs, am, gear, carb) %>%
#'   pivot_longer(-vs,  names_to = "Variable") %>%
#'   dplyr::summarise(Mean = round(mean(value), 2),
#'                    SD = round(sd(value), 2),
#'                    .by = c(Variable,vs)) %>%
#'   mutate(vs = factor(vs, levels = 0:1, labels = c("Automatic", "Manual"))) %>%
#'   flextable_row_title("vs")
#'
apa_flextable <- function(data,
                          row_title_column = NULL,
                          row_title_prefix = "",
                          row_title_sep = " ",
                          row_title_border = list(
                            color = "gray20",
                            style = "solid",
                            width = 1
                          ),
                          col_keys = names(data),
                          cwidth = .75,
                          cheight = .25,
                          ...) {
  if (row_title_prefix == "")  {
    row_title_sep <- ""
  }

  data <- dplyr::mutate(
    data,
    dplyr::across(dplyr::where(\(x) rlang::is_integerish(x) & !is.factor(x)), as.integer))

  is_docx <- knitr::pandoc_to("docx")

  if (is_docx) {
    d <- data
  } else {
    d <- dplyr::select(data, -dplyr::starts_with("breakcolumn"))
  }

  if (is.null(row_title_column)) {
    ft <- flextable::flextable(d,
                               col_keys = col_keys[!(col_keys %in% row_title_column)],
                               cwidth = cwidth,
                               cheight = cheight,
                               ...)
  } else {
    d <- flextable::as_grouped_data(d, groups = row_title_column)
    d$row_title <- d[, row_title_column]
    d <- tidyr::fill(d, !!row_title_column)
    ft <- flextable::flextable(d,
                               col_keys = col_keys[!(col_keys %in% row_title_column)],
                               cwidth = cwidth,
                               cheight = cheight,
                               ...) |>
      flextable::mk_par(
        i = ~ !is.na(row_title),
        value = as_paragraph(row_title_prefix, row_title_sep , row_title)
      ) %>%
      flextable::merge_h(i = ~ !is.na(row_title)) %>%
      flextable::align(i = ~ !is.na(row_title),
                       align = "center",
                       part = "body") %>%
      flextable::surround(
        i = ~ !is.na(row_title),
        border.top = row_title_border
      )


  }

  ft <- flextable::separate_header(ft)
  if (!is_docx) {
    ft <- flextable::empty_blanks(ft)
  }

  ft
}
