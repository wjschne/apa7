#' Tables ready for `apa_flextable`
#'
#' The tibbles returned by [apa_parameters()], [apa_performance()], and
#' [apa_performance_comparison()] have class `apa7_tbl`. They are ordinary
#' tibbles for post-processing with dplyr, but in knitr and Quarto documents
#' they are rendered as an [apa_flextable()] automatically. In the console they
#' print as tibbles.
#'
#' The `apa7.print` option controls this behavior:
#'
#' * `"auto"` (default): flextable in knitr/Quarto, tibble in the console.
#' * `"flextable"`: flextable everywhere.
#' * `"tibble"`: tibble everywhere.
#'
#' To render with non-default settings, pass the table to [apa_flextable()]
#' explicitly, which always returns a flextable.
#'
#' @param x an `apa7_tbl`
#' @param ... arguments passed to the next method
#' @name apa7_tbl
#' @return `x`, invisibly, for `print`. A knit_asis or flextable output for `knit_print`.
#' @examples
#' fit <- lm(mpg ~ cyl + wt, data = mtcars)
#' # Prints as a tibble in the console
#' apa_parameters(fit)
#'
#' # It is still a tibble, so it can be post-processed
#' apa_parameters(fit) |>
#'   dplyr::filter(Variable != "Constant") |>
#'   apa_flextable()
NULL

#' @keywords internal
#' @noRd
as_apa7_tbl <- function(x) {
  class(x) <- unique(c("apa7_tbl", class(x)))
  x
}

#' @keywords internal
#' @noRd
apa7_print_mode <- function() {
  mode <- getOption("apa7.print", "auto")
  if (!(is.character(mode) && length(mode) == 1 && mode %in% c("auto", "flextable", "tibble"))) {
    mode <- "auto" # nocov
  }
  mode
}

#' @rdname apa7_tbl
#' @export
print.apa7_tbl <- function(x, ...) {
  if (apa7_print_mode() == "flextable") {
    print(apa_flextable(x), ...) # nocov
    return(invisible(x)) # nocov
  }
  NextMethod()
}

#' @rdname apa7_tbl
#' @exportS3Method knitr::knit_print
knit_print.apa7_tbl <- function(x, ...) {
  if (apa7_print_mode() == "tibble") {
    class(x) <- setdiff(class(x), "apa7_tbl")
    return(knitr::knit_print(x, ...))
  }
  knitr::knit_print(apa_flextable(x), ...)
}
