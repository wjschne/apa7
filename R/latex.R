#' @exportS3Method knitr::knit_print
knit_print.latex <- function(x, ...) {
  res = paste0('$$\n', x, '\n$$')
  knitr::asis_output(res)
}
#' Display latex matrices
#'
#' @param x a matrix
#' @param bracket type of bracket: bmatrix, Bmatrix, pmatrix, vmatrix, Vmatrix, , matrix
#'
#' @returns character
#' @export
#'
#' @examples
#' matrix_display(diag(2))
matrix_display <- function(
  x,
  bracket = c("[", "(", "|", ""),
  include_names = TRUE
) {
  bracket <- match.arg(bracket)
  if (bracket == "") {
    rbracket <- ""
  } else {
    rbracket <- switch(bracket, `[` = "]", `(` = ")", `|` = "|")
  }

  if (include_names) {
    M <- paste0(
      "\\begin{array}{c c}\n",
      paste0(
        "& \\begin{array} {@{} ",
        paste0(rep("c", ncol(x)), collapse = " "),
        " @{}}\n    "
      ),
      paste0(colnames(x), collapse = " & "),
      "\n  \\end{array}\\\\",
      "\n\\begin{array}{c}\n  ",
      paste0(rownames(x), collapse = " \\\\ "),
      "\n\\end{array}\\hspace{-1em} &\n",
      "\\left",
      bracket,
      "\n",
      paste0(
        " \\begin{array} {@{} ",
        paste0(rep("c", ncol(x)), collapse = " "),
        " @{}}\n    "
      ),
      paste(
        apply(x, MARGIN = 1, FUN = paste0, collapse = " & "),
        collapse = "\\\\\n    "
      ),
      "\n  \\end{array}",
      "\\right",
      rbracket,
      "\n\\end{array}"
    )
  } else {
    M <- paste(
      apply(M, MARGIN = 1, FUN = paste0, collapse = " & "),
      collapse = "\\\\\n"
    )
    M <- paste0("\\left", bracket, " \n", M, "\n\\right", rbracket)
  }
  # M <- paste0("\\begin{", bracket, "}\n", M, "\n\\end{", bracket, "}")
  class(M) <- c("latex", class(M))
  M
}
