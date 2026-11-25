#' Metadata of the Quarto or R Markdown document being rendered
#'
#' Quarto writes the resolved options of the format being rendered (the front
#' matter merged with `_quarto.yml`, the extension's defaults, and the
#' options of the format) to the JSON file named by the `QUARTO_EXECUTE_INFO`
#' environment variable. Without it (R Markdown, or Quarto without jsonlite),
#' the front matter is read from `rmarkdown::metadata`, with the options of
#' the format being rendered (e.g., `format: apaquarto-typst:`) merged in.
#' @returns a list, or `NULL` when no document is being rendered
#' @keywords internal
#' @noRd
document_metadata <- function() {
  if (!isTRUE(getOption("knitr.in.progress"))) {
    return(NULL)
  }
  info <- Sys.getenv("QUARTO_EXECUTE_INFO")
  if (nzchar(info) && file.exists(info) && requireNamespace("jsonlite", quietly = TRUE)) {
    key <- paste(info, file.mtime(info))
    if (!identical(the$document_metadata$key, key)) {
      meta <- tryCatch(jsonlite::read_json(info)$format$metadata, error = function(e) NULL)
      the$document_metadata <- list(key = key, meta = meta)
    }
    return(the$document_metadata$meta)
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    return(NULL)
  }
  meta <- tryCatch(rmarkdown::metadata, error = function(e) NULL)
  if (!is.list(meta) || length(meta) == 0) {
    return(NULL)
  }
  to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  formats <- meta$format
  if (is.list(formats) && length(to) == 1 && !is.null(names(formats))) {
    # e.g., typst or apaquarto-typst; pdf or apaquarto-pdf when rendered to
    # latex
    to_names <- c(to, if (to == "latex") "pdf")
    matches <- names(formats) %in% to_names |
      Reduce(`|`, lapply(to_names, \(t) endsWith(names(formats), paste0("-", t))))
    for (name in names(formats)[matches]) {
      if (is.list(formats[[name]])) {
        meta <- utils::modifyList(meta, formats[[name]])
      }
    }
  }
  meta
}

#' Typst's name for a paper size written in a document's `papersize`
#'
#' Pandoc and LaTeX write paper sizes several ways (e.g., `letter`,
#' `letterpaper`, `us-letter`, and `USLetter`), so the name is reduced to its
#' stem, as apaquarto does, and matched to Typst's names.
#' @returns a name in `typst_paper_widths`, or `NULL`
#' @keywords internal
#' @noRd
typst_paper_name <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  x <- tolower(trimws(as.character(unlist(x))[1]))
  if (is.na(x)) {
    return(NULL)
  }
  if (x %in% names(typst_paper_widths)) {
    return(x)
  }
  stem <- gsub("[^a-z0-9]", "", x)
  stem <- sub("paper$", "", sub("^us", "", stem))
  latex <- c(letter = "us-letter", legal = "us-legal", executive = "us-executive", b5 = "iso-b5")
  if (stem %in% names(latex)) {
    return(latex[[stem]])
  }
  if (stem %in% names(typst_paper_widths)) {
    return(stem)
  }
  NULL
}

#' Page layout of the document being rendered
#'
#' The paper size (`papersize`) and apaquarto's `documentmode` of the
#' document being rendered. The mode sets the side margins: 0.75 inches each
#' in journal mode (`jou`), 1.5 inches on the left and 1 inch on the right in
#' thesis mode, and 1 inch each otherwise. A document without a `papersize`
#' is taken to be on letter-size paper, as apaquarto's are.
#' @returns `NULL` when no document is being rendered or it sets neither
#'   `papersize` nor `documentmode`; otherwise a list with `paper` (a Typst
#'   paper name), `mode` (`"man"`, `"jou"`, `"stu"`, `"doc"`, or `"thesis"`),
#'   and `margins` (the two side margins together, in inches)
#' @keywords internal
#' @noRd
document_layout <- function() {
  meta <- document_metadata()
  if (is.null(meta)) {
    return(NULL)
  }
  paper <- typst_paper_name(meta$papersize)
  mode <- meta$documentmode
  if (is.null(paper) && is.null(mode)) {
    return(NULL)
  }
  mode <- tolower(trimws(as.character(unlist(mode %||% "man"))[1]))
  aliases <- c(journal = "jou", manuscript = "man", document = "doc", student = "stu", dissertation = "thesis")
  if (mode %in% names(aliases)) {
    mode <- aliases[[mode]]
  }
  margins <- switch(mode, jou = 1.5, thesis = 2.5, 2)
  list(paper = paper %||% "us-letter", mode = mode, margins = margins)
}
