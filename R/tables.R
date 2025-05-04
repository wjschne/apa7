
#' Style object with APA style
#'
#' @param x object
#' @param family font family
#' @param border_color border color
#' @param border_width border width in pixels
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
style_apa <- function(x, family = c("Times", "Times New Roman"),
                         border_color = "black",
                         border_width = 0.5) {
  UseMethod("style_apa")
}

#' @export
#' @rdname style_apa
style_apa.gt_tbl <- function(x ,
                             family = c("Times", "Times New Roman"),
                             border_color = "black",
                             border_width = 0.5) {
  x |>
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
    gt::opt_table_font(font = family, color = "black")
}

#' @export
#' @rdname style_apa
style_apa.flextable <- function(x ,
                                family = c("Times", "Times New Roman"),
                                border_color = "black",
                                border_width = 0.5) {
  myborder <- flextable::fp_border_default(color = border_color, width = border_width, style = "solid")
  x |>
    flextable::border(border.top = myborder,
                      border.bottom = myborder,
                      part = "header") |>
    flextable::hline_bottom(part = "all", border = myborder)

}


