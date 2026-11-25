#' The paper size named by the page_width setting
#' @returns the paper's name, or `NULL` if `page_width` is a number of inches
#' @keywords internal
#' @noRd
page_paper <- function() {
  stored <- getOption("apa7.page_width")
  if (is.character(stored) && length(stored) == 1) tolower(trimws(stored)) else NULL
}

#' Width of the text area of a landscape page, in inches
#'
#' Set with `apa7_defaults(landscape_width = )`. Otherwise, the height of the
#' paper named by `page_width` less 1-inch margins on each side, or, when
#' `page_width` is a number, the text width of a landscape page shaped like
#' letter-size paper with the same margins (9 inches for 6.5).
#' @keywords internal
#' @noRd
landscape_width_in <- function() {
  set <- getOption("apa7.landscape_width")
  if (length(set) == 1 && !is.na(set)) {
    return(set)
  }
  paper <- page_paper()
  if (!is.null(paper)) {
    return(typst_paper_heights[[paper]] / 25.4 - 2)
  }
  (page_width_in() + 2) * 279.4 / 215.9 - 2
}

#' Width of one column of a two-column page, in inches
#'
#' Set with `apa7_defaults(column_width = )`. Otherwise, computed the way
#' apaquarto's two-column journal mode (`documentmode: jou`) lays out the
#' page: side margins of 0.75 inches and a gutter of 4% of the text width
#' between the two columns, so that each column is 48% of the text width. On
#' letter-size paper, that is (8.5 - 1.5) * .48 = 3.36 inches. When
#' `page_width` is a number, the paper is taken to be that number plus 1-inch
#' margins.
#' @keywords internal
#' @noRd
column_width_in <- function() {
  set <- getOption("apa7.column_width")
  if (length(set) == 1 && !is.na(set)) {
    return(set)
  }
  paper <- page_paper()
  paper_width <- if (!is.null(paper)) typst_paper_widths[[paper]] / 25.4 else page_width_in() + 2
  (paper_width - 2 * 0.75) * 0.48
}

#' Names of the widths that `table_width` can name
#' @keywords internal
#' @noRd
table_width_containers <- c(page = "page", portrait = "page", landscape = "landscape", column = "column")

#' Units in which `table_width` can be given, per inch
#' @keywords internal
#' @noRd
table_width_units <- c(inches = 1, `in` = 1, cm = 2.54, mm = 25.4)

#' Width of a named container, in inches
#' @keywords internal
#' @noRd
container_width_in <- function(container) {
  switch(
    container,
    page = page_width_in(),
    landscape = landscape_width_in(),
    column = column_width_in()
  )
}

#' Resolve a table width
#'
#' Every table function that takes `table_width` resolves it here, so that it
#' means the same thing everywhere:
#'
#' * `NULL`: the width asked for by the code chunk (its `apa7-width` or
#'   `apa7.width` option, or `"page"` with apaquarto's `apa-twocolumn:
#'   true`; see `chunk_table_width()`), or else the `table_width` setting of
#'   [apa7_defaults()] (1 unless changed).
#' * A number from 0 to 1: that proportion of the default width (the width
#'   named by the `table_width` setting: the portrait page unless changed).
#'   0 keeps the column widths that fit the contents.
#' * `"page"` (or `"portrait"`), `"landscape"`, or `"column"`: the full width
#'   of the text area of a portrait page, of a landscape page, or of one
#'   column of a two-column page.
#' * A named number: a proportion of a named width (e.g.,
#'   `c(landscape = .8)`) or a width in `inches`, `cm`, or `mm` (e.g.,
#'   `c(inches = 5)`).
#' * A number greater than 1: a width in `unit` (inches unless given), as in
#'   apa7 0.2.0, with a warning to name the unit.
#'
#' @param table_width the value given to `table_width`
#' @param unit unit of an unnamed number greater than 1 (`"in"`, `"cm"`, or
#'   `"mm"`)
#' @param arg,call argument name and calling environment for messages
#' @param chunk if `TRUE`, a `NULL` `table_width` is first looked for in the
#'   options of the code chunk
#' @returns list of class `apa7_table_width` with `inches` (the width in
#'   inches, or 0 to fit the contents), `proportion` (the proportion of its
#'   container that the table fills, from 0 to 1), and `container` (`"page"`,
#'   `"landscape"`, or `"column"`)
#' @keywords internal
#' @noRd
resolve_table_width <- function(table_width = NULL, unit = "in", arg = "table_width", chunk = TRUE, call = rlang::caller_env()) {
  if (inherits(table_width, "apa7_table_width")) {
    return(table_width)
  }
  from_setting <- FALSE
  if (is.null(table_width) && chunk) {
    # The width asked for by the code chunk that makes the table
    from_chunk <- chunk_table_width()
    if (!is.null(from_chunk)) {
      table_width <- from_chunk$value
      arg <- from_chunk$option
    }
  }
  if (is.null(table_width)) {
    from_setting <- TRUE
    table_width <- getOption("apa7.table_width", 1)
    arg <- "table_width"
  }
  # The default container: the one that the table_width setting names
  default_container <- function() {
    if (from_setting) {
      return("page")
    }
    resolve_table_width(NULL, chunk = FALSE, call = call)$container
  }
  make <- function(inches, proportion, container) {
    structure(
      list(inches = inches, proportion = proportion, container = container),
      class = "apa7_table_width"
    )
  }
  bad <- function(what) {
    cli::cli_abort(
      c(
        paste0("{.arg {arg}} ", what),
        i = "Use a proportion of the page width from 0 to 1 (e.g., {.code .5}); {.val page} (or {.val portrait}), {.val landscape}, or {.val column}; or a named number such as {.code c(landscape = .8)} or {.code c(inches = 5)}."
      ),
      call = call,
      .envir = parent.frame()
    )
  }

  if (is.character(table_width)) {
    if (length(table_width) != 1 || is.na(table_width)) {
      bad("must be a single value.")
    }
    name <- tolower(trimws(table_width))
    if (!(name %in% names(table_width_containers))) {
      bad("must name a width, not {.val {table_width}}.")
    }
    container <- table_width_containers[[name]]
    return(make(container_width_in(container), 1, container))
  }
  if (!is.numeric(table_width) || length(table_width) != 1 || is.na(table_width) || table_width < 0) {
    bad("must be a single number of at least 0 or the name of a width.")
  }
  name <- names(table_width)
  value <- unname(table_width)
  if (is.null(name) || identical(name, "")) {
    if (value <= 1) {
      container <- default_container()
      return(make(value * container_width_in(container), value, container))
    }
    # A width in inches (or unit), as in apa7 0.2.0 and earlier
    name <- if (unit %in% c("cm", "mm")) unit else "inches"
    legacy <- TRUE
  } else {
    legacy <- FALSE
  }
  name <- tolower(name)
  if (name %in% names(table_width_containers)) {
    if (value > 1) {
      bad("must be a proportion from 0 to 1 when it names a width (e.g., {.code c({name} = .8)}).")
    }
    container <- table_width_containers[[name]]
    return(make(value * container_width_in(container), value, container))
  }
  if (name %in% names(table_width_units)) {
    inches <- value / table_width_units[[name]]
    container <- default_container()
    available <- container_width_in(container)
    wider <- inches > available + 1e-8
    fits_landscape <- wider && container != "landscape" && inches <= landscape_width_in() + 1e-8
    too_wide <- c(
      "!" = if (wider) "A table {signif(inches, 3)} inches wide is wider than the {container} ({signif(available, 3)} inches), so it fills the {container}.",
      i = if (fits_landscape) "For a table on a landscape page, use {.code {arg} = \"landscape\"}."
    )
    if (legacy) {
      # One warning for both problems
      cli::cli_warn(
        c(
          "A {.arg {arg}} greater than 1 is read as a width in {name}.",
          i = "To avoid this warning, name the unit (e.g., {.code {arg} = c({name} = {value})}) or give a proportion of the page width from 0 to 1.",
          too_wide
        ),
        call = call
      )
    } else if (wider) {
      cli::cli_warn(too_wide, call = call)
    }
    return(make(min(inches, available), min(inches / available, 1), container))
  }
  bad("names an unknown width or unit: {.val {name}}.")
}

#' Table width asked for by the current code chunk
#'
#' While knitr runs a code chunk, its `apa7-width` option (`apa7.width` in R
#' Markdown) sets the width of the tables the chunk makes, in any form that
#' `table_width` takes. In YAML, a named number is written as a map (e.g.,
#' `#| apa7-width: {landscape: .8}`). apaquarto's `apa-twocolumn: true`,
#' which spans a table across both columns of a two-column page, makes the
#' table as wide as the page.
#' @returns `NULL`, or a list with the `value` and the name of the `option`
#'   that gave it
#' @keywords internal
#' @noRd
chunk_table_width <- function() {
  if (!isNamespaceLoaded("knitr") || !isTRUE(getOption("knitr.in.progress"))) {
    return(NULL)
  }
  for (option in c("apa7-width", "apa7.width")) {
    value <- knitr::opts_current$get(option)
    if (!is.null(value)) {
      # A YAML map such as {landscape: .8} arrives as a list
      if (is.list(value)) {
        value <- unlist(value)
      }
      return(list(value = value, option = option))
    }
  }
  if (isTRUE(knitr::opts_current$get("apa-twocolumn"))) {
    return(list(value = "page", option = "apa-twocolumn"))
  }
  NULL
}
