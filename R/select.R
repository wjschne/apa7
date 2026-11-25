#' Names of the columns selected by a tidyselect argument
#'
#' Accepts unquoted names (`c(mpg, hp)`), quoted names (`c("mpg", "hp")`),
#' tidyselect helpers (`starts_with("x")`), and a character vector stored in a
#' variable (`vars`), which is used as if wrapped in [tidyselect::all_of()].
#' Names of a selection (e.g., the codes of a lookup vector such as `c(A1 =
#' "Am indifferent to the feelings of others")`) are ignored: they would
#' rename the columns in tidyselect, but only the columns themselves are
#' wanted here.
#'
#' @param quo quosure of the argument (from [rlang::enquo()])
#' @param data data frame
#' @param call environment of the calling function, for error messages
#' @returns character vector of column names (`NULL` if the argument is `NULL`)
#' @keywords internal
#' @noRd
select_names <- function(quo, data, call = rlang::caller_env()) {
  if (rlang::quo_is_null(quo)) {
    return(NULL)
  }
  # A character vector stored in a variable that is not a column name: use it
  # as all_of() would, without tidyselect's deprecation message
  if (rlang::quo_is_symbol(quo) && !(rlang::as_name(quo) %in% colnames(data))) {
    value <- tryCatch(rlang::eval_tidy(quo), error = function(e) NULL)
    if (is.character(value)) {
      quo <- rlang::quo(tidyselect::all_of(!!value))
    }
  }
  # Renaming is allowed only so that named selections do not fail; the
  # columns' own names are returned
  positions <- tidyselect::eval_select(
    quo,
    data,
    allow_rename = TRUE,
    error_call = call
  )
  colnames(data)[positions]
}
