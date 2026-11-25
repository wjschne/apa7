#' Widths of Typst's paper sizes, in millimeters
#'
#' The paper sizes accepted by the `paper` parameter of Typst's `page`
#' function (https://typst.app/docs/reference/layout/page/#parameters-paper),
#' from the `papers!` macro in Typst's source
#' (crates/typst-library/src/layout/page.rs).
#' @keywords internal
#' @noRd
typst_paper_widths <- c(
  "a0" = 841.0,
  "a1" = 594.0,
  "a2" = 420.0,
  "a3" = 297.0,
  "a4" = 210.0,
  "a5" = 148.0,
  "a6" = 105.0,
  "a7" = 74.0,
  "a8" = 52.0,
  "a9" = 37.0,
  "a10" = 26.0,
  "a11" = 18.0,
  "iso-b1" = 707.0,
  "iso-b2" = 500.0,
  "iso-b3" = 353.0,
  "iso-b4" = 250.0,
  "iso-b5" = 176.0,
  "iso-b6" = 125.0,
  "iso-b7" = 88.0,
  "iso-b8" = 62.0,
  "iso-c3" = 324.0,
  "iso-c4" = 229.0,
  "iso-c5" = 162.0,
  "iso-c6" = 114.0,
  "iso-c7" = 81.0,
  "iso-c8" = 57.0,
  "din-d3" = 272.0,
  "din-d4" = 192.0,
  "din-d5" = 136.0,
  "din-d6" = 96.0,
  "din-d7" = 68.0,
  "din-d8" = 48.0,
  "sis-g5" = 169.0,
  "sis-e5" = 115.0,
  "ansi-a" = 216.0,
  "ansi-b" = 279.0,
  "ansi-c" = 432.0,
  "ansi-d" = 559.0,
  "ansi-e" = 864.0,
  "arch-a" = 229.0,
  "arch-b" = 305.0,
  "arch-c" = 457.0,
  "arch-d" = 610.0,
  "arch-e1" = 762.0,
  "arch-e" = 914.0,
  "jis-b0" = 1030.0,
  "jis-b1" = 728.0,
  "jis-b2" = 515.0,
  "jis-b3" = 364.0,
  "jis-b4" = 257.0,
  "jis-b5" = 182.0,
  "jis-b6" = 128.0,
  "jis-b7" = 91.0,
  "jis-b8" = 64.0,
  "jis-b9" = 45.0,
  "jis-b10" = 32.0,
  "jis-b11" = 22.0,
  "sac-d0" = 764.0,
  "sac-d1" = 532.0,
  "sac-d2" = 380.0,
  "sac-d3" = 264.0,
  "sac-d4" = 188.0,
  "sac-d5" = 130.0,
  "sac-d6" = 92.0,
  "iso-id-1" = 85.6,
  "iso-id-2" = 74.0,
  "iso-id-3" = 88.0,
  "asia-f4" = 210.0,
  "jp-shiroku-ban-4" = 264.0,
  "jp-shiroku-ban-5" = 189.0,
  "jp-shiroku-ban-6" = 127.0,
  "jp-kiku-4" = 227.0,
  "jp-kiku-5" = 151.0,
  "jp-business-card" = 91.0,
  "cn-business-card" = 90.0,
  "eu-business-card" = 85.0,
  "fr-telli\u00e8re" = 340.0,
  "fr-couronne-\u00e9criture" = 360.0,
  "fr-couronne-\u00e9dition" = 370.0,
  "fr-raisin" = 500.0,
  "fr-carr\u00e9" = 450.0,
  "fr-j\u00e9sus" = 560.0,
  "uk-brief" = 406.4,
  "uk-draft" = 254.0,
  "uk-foolscap" = 203.2,
  "uk-quarto" = 203.2,
  "uk-crown" = 508.0,
  "uk-book-a" = 111.0,
  "uk-book-b" = 129.0,
  "us-letter" = 215.9,
  "us-legal" = 215.9,
  "us-tabloid" = 279.4,
  "us-executive" = 184.15,
  "us-foolscap-folio" = 215.9,
  "us-statement" = 139.7,
  "us-ledger" = 431.8,
  "us-oficio" = 215.9,
  "us-gov-letter" = 203.2,
  "us-gov-legal" = 215.9,
  "us-business-card" = 88.9,
  "us-digest" = 139.7,
  "us-trade" = 152.4,
  "newspaper-compact" = 280.0,
  "newspaper-berliner" = 315.0,
  "newspaper-broadsheet" = 381.0,
  "presentation-16-9" = 297.0,
  "presentation-4-3" = 280.0
)


#' Heights of Typst's paper sizes, in millimeters
#'
#' In the same order as `typst_paper_widths`, from the same `papers!` macro.
#' @keywords internal
#' @noRd
typst_paper_heights <- c(
  "a0" = 1189.0,
  "a1" = 841.0,
  "a2" = 594.0,
  "a3" = 420.0,
  "a4" = 297.0,
  "a5" = 210.0,
  "a6" = 148.0,
  "a7" = 105.0,
  "a8" = 74.0,
  "a9" = 52.0,
  "a10" = 37.0,
  "a11" = 26.0,
  "iso-b1" = 1000.0,
  "iso-b2" = 707.0,
  "iso-b3" = 500.0,
  "iso-b4" = 353.0,
  "iso-b5" = 250.0,
  "iso-b6" = 176.0,
  "iso-b7" = 125.0,
  "iso-b8" = 88.0,
  "iso-c3" = 458.0,
  "iso-c4" = 324.0,
  "iso-c5" = 229.0,
  "iso-c6" = 162.0,
  "iso-c7" = 114.0,
  "iso-c8" = 81.0,
  "din-d3" = 385.0,
  "din-d4" = 272.0,
  "din-d5" = 192.0,
  "din-d6" = 136.0,
  "din-d7" = 96.0,
  "din-d8" = 68.0,
  "sis-g5" = 239.0,
  "sis-e5" = 220.0,
  "ansi-a" = 279.0,
  "ansi-b" = 432.0,
  "ansi-c" = 559.0,
  "ansi-d" = 864.0,
  "ansi-e" = 1118.0,
  "arch-a" = 305.0,
  "arch-b" = 457.0,
  "arch-c" = 610.0,
  "arch-d" = 914.0,
  "arch-e1" = 1067.0,
  "arch-e" = 1219.0,
  "jis-b0" = 1456.0,
  "jis-b1" = 1030.0,
  "jis-b2" = 728.0,
  "jis-b3" = 515.0,
  "jis-b4" = 364.0,
  "jis-b5" = 257.0,
  "jis-b6" = 182.0,
  "jis-b7" = 128.0,
  "jis-b8" = 91.0,
  "jis-b9" = 64.0,
  "jis-b10" = 45.0,
  "jis-b11" = 32.0,
  "sac-d0" = 1064.0,
  "sac-d1" = 760.0,
  "sac-d2" = 528.0,
  "sac-d3" = 376.0,
  "sac-d4" = 260.0,
  "sac-d5" = 184.0,
  "sac-d6" = 126.0,
  "iso-id-1" = 53.98,
  "iso-id-2" = 105.0,
  "iso-id-3" = 125.0,
  "asia-f4" = 330.0,
  "jp-shiroku-ban-4" = 379.0,
  "jp-shiroku-ban-5" = 262.0,
  "jp-shiroku-ban-6" = 188.0,
  "jp-kiku-4" = 306.0,
  "jp-kiku-5" = 227.0,
  "jp-business-card" = 55.0,
  "cn-business-card" = 54.0,
  "eu-business-card" = 55.0,
  "fr-telli\u00e8re" = 440.0,
  "fr-couronne-\u00e9criture" = 460.0,
  "fr-couronne-\u00e9dition" = 470.0,
  "fr-raisin" = 650.0,
  "fr-carr\u00e9" = 560.0,
  "fr-j\u00e9sus" = 760.0,
  "uk-brief" = 342.9,
  "uk-draft" = 406.4,
  "uk-foolscap" = 330.2,
  "uk-quarto" = 254.0,
  "uk-crown" = 381.0,
  "uk-book-a" = 178.0,
  "uk-book-b" = 198.0,
  "us-letter" = 279.4,
  "us-legal" = 355.6,
  "us-tabloid" = 431.8,
  "us-executive" = 266.7,
  "us-foolscap-folio" = 342.9,
  "us-statement" = 215.9,
  "us-ledger" = 279.4,
  "us-oficio" = 340.36,
  "us-gov-letter" = 266.7,
  "us-gov-legal" = 330.2,
  "us-business-card" = 50.8,
  "us-digest" = 215.9,
  "us-trade" = 228.6,
  "newspaper-compact" = 430.0,
  "newspaper-berliner" = 470.0,
  "newspaper-broadsheet" = 578.0,
  "presentation-16-9" = 167.0625,
  "presentation-4-3" = 210.0
)

#' Width of the text area of a page, in inches
#'
#' @param page_width a number (the width of the text area, in inches) or the
#'   name of a Typst paper size (e.g., `"a4"` or `"us-letter"`), whose width
#'   less APA's 1-inch margins on each side is the width of the text area
#' @param margin width of each side margin, in inches
#' @returns width in inches
#' @keywords internal
#' @noRd
page_text_width <- function(page_width, margin = 1, call = rlang::caller_env()) {
  message <- "{.arg page_width} must be a single positive number (inches) or the name of a paper size."
  if (is.numeric(page_width)) {
    if (length(page_width) != 1 || is.na(page_width) || page_width <= 0) {
      cli::cli_abort(message, call = call)
    }
    return(page_width)
  }
  if (!is.character(page_width) || length(page_width) != 1 || is.na(page_width)) {
    cli::cli_abort(message, call = call)
  }
  name <- tolower(trimws(page_width))
  if (!(name %in% names(typst_paper_widths))) {
    # The closest names (at most three)
    distance <- as.vector(utils::adist(name, names(typst_paper_widths)))
    near <- names(typst_paper_widths)[distance <= 2 & distance == min(distance)]
    if (length(near) == 0) {
      # Names that contain it (e.g., "letter" in "us-letter")
      near <- names(typst_paper_widths)[grepl(name, names(typst_paper_widths), fixed = TRUE)]
    }
    near <- utils::head(near, 3)
    cli::cli_abort(
      c(
        "{.val {page_width}} is not a paper size.",
        i = if (length(near) > 0) "Did you mean {.or {.val {near}}}?",
        i = "Use a Typst paper name (e.g., {.val a4} or {.val us-letter}) or a width in inches."
      ),
      call = call
    )
  }
  text_width <- typst_paper_widths[[name]] / 25.4 - 2 * margin
  if (text_width <= 0) {
    cli::cli_abort("Paper size {.val {page_width}} is too narrow for 1-inch margins.", call = call)
  }
  text_width
}

#' Width of the text area set with apa7_defaults(page_width = )
#' @keywords internal
#' @noRd
page_width_in <- function() {
  the$page_width %||% 6.5
}
