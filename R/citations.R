#' Citations in table notes
#'
#' Some table notes cite the articles that describe a method (e.g., the
#' trim-and-fill method in [apa_publication_bias()]). The references are in a
#' BibTeX file that comes with apa7, and `apa7_write_bib()` copies it to a
#' document's folder so that the citations in notes can be resolved and added
#' to the reference list.
#'
#' Citations in notes take one of two forms:
#'
#' * text: "(Duval & Tweedie, 2000)", which the author must add to the
#'   reference list by hand.
#' * keys: Pandoc citation keys, "\[@duvalTrimFillSimple2000\]", which
#'   Pandoc's citeproc (and [apaquarto](https://wjschne.github.io/apaquarto/))
#'   turns into formatted citations and reference-list entries, if the
#'   document's bibliography has the entries.
#'
#' The form is set by the `citations` argument of [apa_note()], or for all
#' notes by `apa7_defaults(citations = )` (or `options(apa7.citations = )`):
#' `"auto"` (default), `"keys"`, or
#' `"text"`. With `"auto"`, a citation is written as a key when a Quarto or
#' R Markdown document is being rendered and its `bibliography` (in the YAML
#' header, or for a whole Quarto project or folder in `_quarto.yml` or
#' `_metadata.yml`) has an entry with that key, and as text otherwise (e.g., in the
#' console, or in a table saved with [flextable::save_as_docx()]). So listing
#' the file written by `apa7_write_bib()` in the YAML header is enough:
#'
#' ```
#' ---
#' bibliography: [references.bib, apa7.bib]
#' ---
#' ```
#'
#' with `apa7::apa7_write_bib()` in the setup chunk. The citation keys are
#' those that the Better BibTeX extension for Zotero makes by default (e.g.,
#' `eggerBiasMetaanalysisDetected1997`), so entries exported from Zotero with
#' the same keys work, too.
#'
#' Citations in notes from `apa_flextable(note = TRUE)` and the
#' `apa-note` chunk option of apaquarto are resolved when the document is
#' rendered. Citeproc does not see text that skips Pandoc (e.g., a flextable
#' saved directly to a Word file), so `"auto"` writes text there.
#'
#' @param file path of the BibTeX file to write
#' @param overwrite if `TRUE` (default), an existing file is replaced
#' @returns `apa7_write_bib()` returns `file`, invisibly. `apa7_bib()` returns
#'   the path of the BibTeX file that comes with apa7.
#' @export
#' @examples
#' # In a document's setup chunk: apa7_write_bib("apa7.bib")
#' path <- apa7_write_bib(tempfile(fileext = ".bib"))
#' readLines(path, n = 8)
apa7_write_bib <- function(file = "apa7.bib", overwrite = TRUE) {
  if (!is.character(file) || length(file) != 1 || is.na(file) || !nzchar(file)) {
    cli::cli_abort("{.arg file} must be a file path.")
  }
  if (file.exists(file) && !isTRUE(overwrite)) {
    cli::cli_abort("{.file {file}} already exists. Use {.code overwrite = TRUE} to replace it.")
  }
  ok <- file.copy(apa7_bib(), file, overwrite = TRUE)
  if (!isTRUE(ok)) {
    cli::cli_abort("Could not write {.file {file}}.") # nocov
  }
  invisible(file)
}

#' @rdname apa7_write_bib
#' @export
apa7_bib <- function() {
  system.file("apa7.bib", package = "apa7", mustWork = TRUE)
}

#' Entries of a BibTeX file
#'
#' A small parser for the fields that citations need: the key, the authors'
#' last names, and the year. Braces are removed from names.
#' @param path path of a BibTeX file
#' @returns named list (by key) of lists with `authors` and `year`
#' @keywords internal
#' @noRd
bib_entries <- function(path) {
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  starts <- gregexpr("@[A-Za-z]+\\s*\\{\\s*[^,\\s]+\\s*,", text, perl = TRUE)[[1]]
  if (starts[1] == -1) {
    return(list())
  }
  ends <- c(starts[-1] - 1, nchar(text))
  entries <- list()
  for (i in seq_along(starts)) {
    entry <- substr(text, starts[i], ends[i])
    key <- sub("(?s)^@[A-Za-z]+\\s*\\{\\s*([^,\\s]+)\\s*,.*$", "\\1", entry, perl = TRUE)
    field <- function(name) {
      m <- regmatches(
        entry,
        regexpr(paste0("(?is)\\b", name, "\\s*=\\s*(\\{(?:[^{}]|\\{(?:[^{}]|\\{[^{}]*\\})*\\})*\\}|\"[^\"]*\"|[0-9]+)"), entry, perl = TRUE)
      )
      if (length(m) == 0) {
        return(NA_character_)
      }
      value <- sub(paste0("(?is)^", name, "\\s*=\\s*"), "", m, perl = TRUE)
      value <- gsub("^[{\"]|[}\"]$", "", value)
      trimws(gsub("[{}]", "", value))
    }
    authors <- field("author")
    last_names <- if (is.na(authors)) character(0) else {
      people <- trimws(strsplit(authors, "\\s+and\\s+")[[1]])
      vapply(people, \(p) {
        if (grepl(",", p, fixed = TRUE)) trimws(sub(",.*$", "", p)) else utils::tail(strsplit(p, "\\s+")[[1]], 1)
      }, character(1), USE.NAMES = FALSE)
    }
    year <- field("year")
    if (is.na(year)) {
      date <- field("date")
      year <- if (is.na(date)) "n.d." else substr(date, 1, 4)
    }
    # A republished work is cited with both years, as in "Tukey, 1953/1994"
    original <- field("origdate")
    if (is.na(original)) original <- field("origyear")
    if (!is.na(original)) year <- paste0(substr(original, 1, 4), "/", year)
    entries[[key]] <- list(authors = last_names, year = year)
  }
  entries
}

#' Entries of apa7's BibTeX file (read once per session)
#' @keywords internal
#' @noRd
apa7_bib_entries <- function() {
  if (is.null(the$bib_entries)) {
    the$bib_entries <- bib_entries(apa7_bib())
  }
  the$bib_entries
}

#' Author-date text of a citation
#' @param key citation key in apa7's BibTeX file
#' @param narrative if `TRUE`, "Egger et al. (1997)"; otherwise
#'   "Egger et al., 1997" (for use inside parentheses)
#' @keywords internal
#' @noRd
citation_text <- function(key, narrative = FALSE) {
  entry <- apa7_bib_entries()[[key]]
  if (is.null(entry)) {
    cli::cli_abort("Unknown citation key {.val {key}}.", .internal = TRUE) # nocov
  }
  a <- entry$authors
  names <- if (length(a) == 1) {
    a
  } else if (length(a) == 2) {
    if (narrative) paste(a[1], "and", a[2]) else paste(a[1], "&", a[2])
  } else {
    paste(a[1], "et al.")
  }
  if (narrative) paste0(names, " (", entry$year, ")") else paste0(names, ", ", entry$year)
}

#' Mark a citation in note text
#'
#' Note functions write citations as markers, which [apa_note()] turns into
#' text or citation keys.
#' @param key citation key in apa7's BibTeX file, or several keys to cite
#'   together ("(Kramer, 1956; Tukey, 1953/1994)")
#' @param narrative if `TRUE`, a narrative citation ("Egger et al. (1997)");
#'   otherwise a parenthetical one ("(Egger et al., 1997)")
#' @keywords internal
#' @noRd
cite <- function(key, narrative = FALSE) {
  paste0("<<", if (narrative) "citet" else "cite", ":", paste(key, collapse = ";"), ">>")
}

#' Keys in the bibliography of the document being rendered
#' @returns character vector of keys, or NULL when no document is being
#'   rendered (or it has no bibliography)
#' @keywords internal
#' @noRd
document_bib_keys <- function() {
  if (!isTRUE(getOption("knitr.in.progress"))) {
    return(NULL)
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    return(NULL) # nocov
  }
  files <- tryCatch(unlist(rmarkdown::metadata$bibliography), error = function(e) NULL)
  input_dir <- tryCatch(dirname(knitr::current_input(dir = TRUE)), error = function(e) ".")
  if (length(files) == 0) {
    # Quarto gives knitr only the document's own metadata, so a bibliography
    # set for a whole folder or project is read from those files
    files <- quarto_bib_files(input_dir)
  }
  if (length(files) == 0) {
    return(NULL)
  }
  keys <- character(0)
  for (f in files) {
    path <- if (file.exists(f)) f else file.path(input_dir, f)
    if (file.exists(path) && grepl("\\.bib$", path, ignore.case = TRUE)) {
      keys <- c(keys, names(tryCatch(bib_entries(path), error = function(e) list())))
    }
  }
  keys
}

#' Bibliography files set in a Quarto project's metadata files
#'
#' Looks for `bibliography` in `_metadata.yml` (directory metadata) and
#' `_quarto.yml` (project metadata), from the document's folder up to the
#' project's root folder. The nearest file that sets it wins, as in Quarto.
#' Outside a Quarto project, directory metadata does not apply.
#' @param input_dir folder of the document being rendered
#' @returns paths of the bibliography files (relative paths are relative to
#'   the metadata file that names them), or `character(0)`
#' @keywords internal
#' @noRd
quarto_bib_files <- function(input_dir) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    return(character(0)) # nocov
  }
  dir <- normalizePath(input_dir, winslash = "/", mustWork = FALSE)
  found <- character(0)
  repeat {
    for (meta in c("_metadata.yml", "_quarto.yml")) {
      path <- file.path(dir, meta)
      if (length(found) == 0 && file.exists(path)) {
        bib <- tryCatch(
          unlist(yaml::read_yaml(path)$bibliography),
          error = function(e) NULL
        )
        if (length(bib) > 0) {
          absolute <- grepl("^([A-Za-z]:)?[/\\\\]", bib)
          found <- ifelse(absolute, bib, file.path(dir, bib))
        }
      }
    }
    if (file.exists(file.path(dir, "_quarto.yml"))) {
      return(found)
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) {
      # Not in a Quarto project
      return(character(0))
    }
    dir <- parent
  }
}

#' Turn citation markers into text or keys
#' @param sentences character vector of note sentences
#' @param citations `"auto"`, `"keys"`, or `"text"`
#' @keywords internal
#' @noRd
resolve_citations <- function(sentences, citations = "auto") {
  pattern <- "<<(cite|citet):([^>]+)>>"
  if (!any(grepl(pattern, sentences))) {
    return(sentences)
  }
  available <- if (citations == "auto") document_bib_keys() else NULL
  vapply(sentences, \(s) {
    repeat {
      m <- regmatches(s, regexec(pattern, s))[[1]]
      if (length(m) == 0) {
        break
      }
      narrative <- m[2] == "citet"
      keys <- strsplit(m[3], ";", fixed = TRUE)[[1]]
      # All keys or none, so one parenthesis never mixes keys and text
      as_key <- citations == "keys" || (citations == "auto" && all(keys %in% available))
      replacement <- if (as_key) {
        if (narrative) {
          paste0("@", keys, collapse = ", ")
        } else {
          paste0("[", paste0("@", keys, collapse = "; "), "]")
        }
      } else {
        if (narrative) {
          paste(vapply(keys, citation_text, "", narrative = TRUE), collapse = ", ")
        } else {
          paste0("(", paste(vapply(keys, citation_text, ""), collapse = "; "), ")")
        }
      }
      s <- sub(m[1], replacement, s, fixed = TRUE)
    }
    s
  }, character(1), USE.NAMES = FALSE)
}

#' The citations setting
#' @keywords internal
#' @noRd
citations_mode <- function(citations = NULL, call = rlang::caller_env()) {
  mode <- citations %||% getOption("apa7.citations", "auto")
  if (!(is.character(mode) && length(mode) == 1 && mode %in% c("auto", "keys", "text"))) {
    cli::cli_abort("{.arg citations} (or the {.code apa7.citations} option) must be {.val auto}, {.val keys}, or {.val text}.", call = call)
  }
  mode
}

#' Citation of a degrees-of-freedom method
#' @param method `"kenward"` or `"satterthwaite"`
#' @keywords internal
#' @noRd
df_method_citation <- function(method) {
  cite(switch(
    method,
    kenward = "kenwardSmallSampleInference1997",
    satterthwaite = "satterthwaiteApproximateDistributionEstimates1946"
  ))
}
