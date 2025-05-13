
#' Style object with APA style
#'
#' @param x object
#' @param family font family
#' @param font_size font size
#' @param border_color border color
#' @param border_width border width in pixels
#' @param line_spacing spacing between lines
#' @name style_apa
#' @returns object
#' @export
#'
#' @examples
#' d <- data.frame(x = 1:3, y = 4:6)
#' flextable::flextable(d) |>
#'   style_apa()
#' gt::gt(d) |>
#'   style_apa()
style_apa <- function(x,
                      family = "Times New Roman",
                      font_size = 12,
                      border_color = "black",
                      border_width = 0.5,
                      line_spacing = 2) {
  UseMethod("style_apa")
}

#' @export
#' @rdname style_apa
style_apa.gt_tbl <- function(x ,
                             family = "Times New Roman",
                             font_size = 12,
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

      table.border.bottom.style = "solid",
      table.border.bottom.color = border_color,
      table.border.bottom.width = border_width,

      heading.border.bottom.style = "solid",
      heading.border.bottom.color = border_color,
      heading.border.bottom.width = border_width,

      table_body.border.top.style = "solid",
      table_body.border.top.color = "black",
      table_body.border.top.width = border_width,

    ) |>
    gt::opt_table_font(font = family, color = "black", size = gt::px(font_size * 16 / 12)) |>
    gt::tab_options(data_row.padding = gt::px(pd),
                    heading.padding = gt::px(pd),
                    column_labels.padding = gt::px(pd)) |>
    gt::sub_missing(missing_text = "")
}

#' @export
#' @rdname style_apa
style_apa.flextable <- function(x ,
                                family = "Times New Roman",
                                font_size = 12,
                                border_color = "black",
                                border_width = 0.5,
                                line_spacing = 2) {
  myborder <- list(color = border_color, width = border_width, style = "solid")
  x |>
    flextable::font(part = "all",fontname = family) |>
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
#' @param bold_significant bold significant correlations
#' @param output output type. Can be "flextable" "gt" or "tibble"
#' @inheritParams style_apa
#' @param ... <[`data-masking`][rlang::args_data_masking]> parameters passed to psych::corTest
#'
#' @returns flextable, gt, or tibble
#' @export
#'
#' @examples
#' cor_apa(mtcars[, c("mpg", "am", "gear", "carb")], output = "flextable")
#' cor_apa(mtcars[, c("mpg", "am", "gear", "carb")], output = "gt")
#' cor_apa(mtcars[, c("mpg", "am", "gear", "carb")], output = "tibble")
cor_apa <- function(data,
                    p_value = .05,
                    bold_significant = TRUE,
                    output = c("flextable", "gt", "tibble"),
                    family = "Times New Roman",
                    font_size = 12,
                    border_color = "black",
                    border_width = 0.5,
                    line_spacing = 2,
                    ...

) {
  output <- match.arg(output)
  v_names <- colnames(data)
  v_seq <- seq(ncol(data))


  ct <- psych::corTest(data, ...)

  R <- ct$r |>
    apply(2, signs::signs, accuracy = .01, trim_leading_zeros = TRUE)



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

  d_msd <- tibble::tibble(Variable = paste0(v_seq, ". ", v_names),
                  M = colMeans(data, na.rm = TRUE) |>
                    signs::signs(accuracy = .01),

                  SD = apply(data, 2, stats::sd, na.rm = TRUE) |>
                    signs::signs(accuracy = .01))


  colnames(R) <- v_seq

  d_R <- dplyr::bind_cols(d_msd, R)

  attr(d_R, "cortest") <- ct

  if (output == "flextable") {
    flextable::set_flextable_defaults(font.family = family)
    d <-
      flextable::flextable(d_R) |>
      style_apa(family = family,
                font_size = font_size,
                border_color = border_color,
                border_width = border_width,
                line_spacing = line_spacing) |>
      flextable::align(j = "Variable", align = "left", part = "all") |>
      flextable::align(j = -1, align = "right", part = "all") |>
      flextable::italic(j = c("M", "SD"), part = "header", italic = TRUE) |>
      ftExtra::colformat_md(j = -"Variable") |>
      flextable::autofit(add_w = 0, add_h = 0)

    d_R <- d
  }

  if (output == "gt") {
    d <-  gt::gt(d_R) |>
      style_apa(family = family,
                font_size = font_size,
                border_color = border_color,
                border_width = border_width,
                line_spacing = line_spacing) |>
      gt::cols_align(align = "right", columns = -"Variable") |>
      gt::tab_style(locations = gt::cells_column_labels(c("M", "SD")), style = gt::cell_text(style = "italic"))  |>
      gt::fmt_markdown(columns = -1)

    k <- nrow(d$`_boxhead`)

    d$`_boxhead`[seq(1, k), "column_width"] <- rep(gt::pct(round(100 / (k - 3), 1)),k)
    d_R <- d

  }


  d_R
}


