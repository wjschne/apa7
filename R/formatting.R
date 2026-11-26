#' Align text on center text (default is decimal)
#'
#' `align_chr()` formats numbers and pads them on both sides of the decimal
#' point (or another `center` character) so that every value has the same
#' width. When the column is centered, the decimal points line up.
#'
#' @details
#' With a few minor details omitted, `align_chr()` does roughly this:
#'
#' 1. If `format_numeric_character = TRUE`, converts text made only of digits,
#'    minus signs, and periods to numbers.
#' 2. Formats numbers with [signs::signs()]: rounds them to `accuracy`, replaces
#'    hyphens with true minus signs, and adds commas to large numbers. Plus
#'    signs are added if `add_plusses = TRUE`, leading zeros are removed if
#'    `trim_leading_zeros = TRUE`, and trailing zeros are dropped if
#'    `drop0trailing = TRUE`. Whole numbers are not given decimals unless
#'    `format_integers = TRUE`.
#' 3. Splits each value at the first `center` character, then pads the part
#'    before it on the left and the part after it on the right (`side` limits
#'    the padding to one side), so that every value has the same width on each
#'    side of the center.
#'
#' By default, values are padded with figure spaces, which are as wide as a
#' digit. With this default, if `width_aware_padding = TRUE` in
#' [apa7_defaults()] (the default) and the default font is installed, values
#' are instead padded to the same *rendered* width in that font (see
#' [num_pad()]), so that values with minus signs, commas, or markdown still
#' line up. Other `padding_character` values are counted in characters.
#'
#' The padded values line up only if every cell in the column is aligned the
#' same way. [apa_flextable()] centers the columns of numbers and lines up
#' padded values (see its Details).
#'
#' @param x vector (numeric or character)
#' @param accuracy number to round to. If NULL, the current default accuracy set with [apa7_defaults()] will be used.
#' @param trim_leading_zeros if `TRUE`, leading zeros are removed (e.g., .45 instead of 0.45), as APA style requires for values that cannot exceed 1 (correlations, proportions, *p*-values). If `FALSE` (default), they are kept.
#' @param drop0trailing Drop trailing zeros
#' @param add_plusses if `TRUE`, positive numbers get a plus sign. If `FALSE` (default), they do not.
#' @param padding_character character to use for padding, default is `&numsp;` (figure space). With the default, text is padded to equal rendered width in the default font family (see [num_pad()] and the `width_aware_padding` argument of [apa7_defaults()]).
#' @param center text on which to align text. Center on decimal by default, but can be any text.
#' @param format_integers If TRUE, integers will be formatted with digits
#' @param side side on which to make text of equal width
#' @param NA_value value to replace NA
#' @param format_numeric_character format character variables with numeric content
#' @param ... additional arguments passed to `signs::signs()`
#'
#' @return character vector
#' @export
#'
#' @examples
#' align_chr(c(1, 10, 100))
align_chr <- function(
  x,
  accuracy = NULL,
  trim_leading_zeros = FALSE,
  drop0trailing = FALSE,
  add_plusses = FALSE,
  padding_character = NULL, # figure space
  center = ".",
  format_integers = FALSE,
  side = c("both", "left", "right"),
  NA_value = "",
  format_numeric_character = FALSE,
  ...
) {
  if (is.null(padding_character)) {
    # Default padding character is a figure space
    padding_character <- "\u2007"
  }

  # With figure-space padding, pad to equal rendered width in the default font
  pad_font <- NA
  if (
    isTRUE(the$width_aware_padding) &&
      padding_character %in% c("\u2007", "&numsp;")
  ) {
    pad_font <- the$font_family
  }
  center1 <- center
  if (center1 == ".") {
    center1 <- "\\."
  }

  if (is.null(accuracy)) {
    accuracy <- the$accuracy
  }

  # Use xx to make changes, keep x pristine
  xx <- x

  # Trim spaces
  if (is.character(x)) {
    xx <- stringr::str_trim(xx)
  }

  # On which side to align
  side <- match.arg(side, c("both", "left", "right"))

  # If character text is all numbers, make numeric
  if (is.character(xx) && format_numeric_character) {
    if (all(grepl("^[-0-9.]+$", xx))) {
      xx <- as.numeric(xx)
    }
  }

  # If x is all integers, then do not add decimals
  if (
    (rlang::is_integerish(x) || rlang::is_integerish(xx)) && !format_integers
  ) {
    xx <- num_pad(
      signs::signs(
        xx,
        add_plusses = add_plusses,
        big.mark = ","
      ),
      padding_character = padding_character,
      font_family = pad_font
    )
  } else {
    if (is.numeric(xx)) {
      xx <- signs::signs(
        xx,
        accuracy = accuracy,
        add_plusses = add_plusses,
        big.mark = ",",
        trim_leading_zeros = trim_leading_zeros,
        drop0trailing = drop0trailing,
        ...
      )
    }

    # Make split on only on the first instance of center
    splitter <- "apa7replacecharacter"
    xx <- stringr::str_replace(xx, center1, splitter)

    left_x <- stringr::str_split_i(
      xx,
      pattern = splitter,
      i = 1
    )

    right_x <- stringr::str_split_i(
      xx,
      pattern = splitter,
      i = 2
    )

    if (side != "right") {
      left_x <- num_pad(
        left_x,
        padding_character = padding_character,
        NA_value = "",
        font_family = pad_font
      )
    }

    if (side != "left") {
      right_x <- num_pad(
        right_x,
        pad_left = FALSE,
        padding_character = padding_character,
        NA_value = "",
        font_family = pad_font
      )
    }

    # Make middle text equal to center
    middle <- paste0(center, rep("", length(xx)))

    # Remove center text if splitter is not detected
    middle[!stringr::str_detect(xx, splitter)] <- ""

    # Remove center text if xx is NA
    middle[is.na(xx)] <- ""

    # combine left, middle, and right text
    xx <- paste0(left_x, middle, right_x)
  }

  # Make NA if original x is NA, zero-length, or empty
  xx[is.na(x) | nchar(x) == 0 | x == ""] <- NA_value

  xx
}

#' p-value in APA format
#'
#' @param p probability
#' @param inline if `TRUE`, the symbol and relation are included (e.g., *p* = .04 or *p* < .001), for use in running text (one *p*-value at a time). If `FALSE` (default), only the number is returned (e.g., .04 or <.001), for use in tables.
#' @param markdown if `TRUE` (default), the output is markdown (e.g., `*p*` in italics). If `FALSE`, it is plain text, which can be used in LaTeX.
#' @param min_digits minimum number of digits to round to. Default is 2.
#' @param max_digits maximum number of digits to round to. Default is 3.
#' @param align decimal alignment if `TRUE`
#'
#' @return character vector
#' @export
#'
#' @examples
#' # Values less than .001 are <.001
#' apa_p(.0002)
#' # Values between .001 and .01 are rounded to 3 digits
#' apa_p(.002)
#' # Values between .01 and .995 are rounded to 2 digits
#' apa_p(.02)
#' apa_p(.22)
#' apa_p(.994)
#' # Values above .995 are >.99
#' apa_p(.999)
#' # Rounding to 3 digits
#' apa_p(.2341, min_digits = 3)
#' apa_p(.0123, min_digits = 3)
#' apa_p(.00123, min_digits = 3)
#' apa_p(.000123, min_digits = 3)
#' apa_p(.991, min_digits = 3)
#' apa_p(.9991, min_digits = 3)
#' apa_p(.9995, min_digits = 3)
apa_p <- function(
  p,
  inline = FALSE,
  markdown = TRUE,
  min_digits = 2,
  max_digits = 3,
  align = FALSE
) {
  min_p <- 10^(-max_digits)
  min_p_chr <- signs::signs(
    min_p,
    trim_leading_zeros = TRUE,
    accuracy = min_p
  )

  if (is.character(p)) {
    p <- stringr::str_trim(p)
    less_than <- stringr::str_detect(
      string = p,
      pattern = stringr::fixed("<",
                               ignore_case = TRUE))
    p <- stringr::str_remove(p, "^<") |> as.numeric()
    p[less_than] <- 10^(-max_digits - 1)
  }

  if (inline) {
    if (length(p) > 1) {
      stop("Only one p can be processed for inline mode.")
    }

    if (p < min_p) {
      if (markdown) {
        return(paste0("*p*\u00A0<\u00A0", min_p_chr))
      } else {
        return(paste0("p < ", min_p_chr))
      }
    }
  }

  max_p <- 10^(-min_digits)
  max_p_chr <- signs::signs(
    1 - max_p,
    trim_leading_zeros = TRUE,
    accuracy = max_p
  )
  p_threshold <- max_p - max_p * 0.05
  max_p_threshold <- 1 - max_p * 0.5

  digit <- ifelse(p < p_threshold, max_digits, min_digits)
  p_formatted <- purrr::map2_chr(p, digit, function(pp, dd) {
    if (is.na(pp)) {
      return(NA)
    }
    formatC(round(pp, dd + 1), digits = dd, format = "f")
  })

  p_formatted <- sub(x = p_formatted, pattern = "^0\\.", replacement = ".")

  gt_symbol <- ifelse(markdown, "\\>", ">")
  p_symbol <- ifelse(markdown, "*p*", "p")

  operator <- ifelse(
    p < min_p,
    "<",
    ifelse(p >= max_p_threshold, gt_symbol, "=")
  )
  sep <- ifelse(markdown, "\u00A0", " ")

  prefix <- paste0(p_symbol, sep, operator, sep)
  if (inline) {
    p_formatted[p < min_p] <- min_p_chr
    p_formatted[p >= max_p_threshold] <- max_p_chr
  } else {
    p_formatted[p < min_p] <- paste0("<", min_p_chr)
    p_formatted[p >= max_p_threshold] <- paste0(gt_symbol, max_p_chr)
  }

  pv <- paste0(ifelse(inline, prefix, ""), p_formatted)
  pv[is.na(p)] <- NA
  if (align) {
    pv <- align_chr(pv, format_numeric_character = FALSE)
  }
  pv
}

#' Warn about a renamed argument
#'
#' @param old,new old and new argument names
#' @param fn function name
#' @noRd
warn_renamed_arg <- function(old, new, fn) {
  rlang::warn(
    paste0("The `", old, "` argument of `", fn, "()` is deprecated. Use `", new, "` instead."),
    .frequency = "regularly",
    .frequency_id = paste0("apa7_", fn, "_", old)
  )
}

#' Make star notes for p-values
#'
#' @param alpha vector of alpha values (*p*-value thresholds), from largest to
#'   smallest. Each gets one more star.
#' @param x deprecated; use `alpha`
#' @inheritParams p2stars
#'
#' @return character vector
#' @export
#'
#' @examples
#' apa_p_star_note()
#' apa_p_star_note(alpha = c(0.10, 0.05, 0.01, 0.001),
#'                 first_alpha_marginal = TRUE)
apa_p_star_note <- function(
  alpha = c(0.05, 0.01, 0.001),
  first_alpha_marginal = FALSE,
  x
) {
  if (!missing(x)) {
    warn_renamed_arg("x", "alpha", "apa_p_star_note")
    alpha <- x
  }
  x <- alpha
  paste0(
    p2stars(
      x,
      alpha = x,
      superscript = TRUE,
      first_alpha_marginal = first_alpha_marginal,
      add_trailing_space = TRUE
    ),
    "*p*\u00A0\u2264\u00A0",
    signs::signs(
      x,
      trim_leading_zeros = TRUE,
      drop0trailing = TRUE,
      accuracy = 0.001
    ),
    collapse = ". "
  )
}

#' Return markdown text with hanging indent
#'
#' @param x text
#' @param indent number of `space` characters that indent the second and later lines
#' @param width maximum number of characters per line
#' @param space indenting character. If `NULL` (default), a non-breaking space.
#' @param newline text inserted at each line break (default: a markdown hard line break, a backslash followed by a newline)
#' @param whitespace_only if `TRUE`, lines break only at spaces. If `FALSE` (default), they can also break after hyphens and slashes (see [stringr::str_wrap()]).
#' @param wrap_equal_width if `TRUE`, lines are made roughly equal in width (see [str_wrap_equal()]). If `FALSE` (default), each line is filled up to `width`.
#'
#' @return character vector
#' @export
#'
#' @examples
#' hanging_indent(
#'   "Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Erlbaum."
#' )
hanging_indent <- function(
  x,
  indent = 4,
  width = 30,
  space = NULL,
  newline = "\\\\\n",
  whitespace_only = FALSE,
  wrap_equal_width = FALSE
) {
  if (is.null(space)) {
    space <- "\u00A0" # non-breaking space
  }
  indent_chr <- paste0(rep(space, indent), collapse = "")

  x <- stringr::str_replace_all(x, "\u2007", "\u2057")

  if (wrap_equal_width) {
    x <- str_wrap_equal(x, max_width = width)
  } else {
    x <- stringr::str_wrap(x, width = width, whitespace_only = whitespace_only)
  }
  stringr::str_replace_all(x, "\n", paste0(newline, indent_chr)) |>
    stringr::str_replace_all("\u2057", "\u2007")
}

#' Like `stringr::str_wrap`, but attempts to create lines of equal width
#'
#' @param x character
#' @param max_width maximum number of characters per line
#' @param sep text inserted at each line break
#'
#' @return character
#' @export
#'
#' @examples
#' str_wrap_equal("This function attempts to split the string into lines with roughly equal width.")
str_wrap_equal <- function(x, max_width = 30L, sep = "\n") {
  purrr::map_chr(x, \(xi) {
    # Find overall text length
    xlen <- stringr::str_length(xi)
    if (is.na(xlen)) {
      NA_character_
    } else {
      # Remove any line breaks and allow lines to break at forward slashes
      xi <- stringr::str_replace(xi, "\n", " ") |>
        stringr::str_replace("/", "/ ")
      # Number of lines likely needed
      k <- ceiling(xlen / max_width)
      # Optimal line length
      preferred_width <- xlen / k
      # Split text into words
      words <- stringr::str_split(xi, pattern = " ", simplify = FALSE)[[1]]
      # Number of words in text
      k_words <- length(words)
      # Length of each word in text
      word_len <- stringr::str_length(words)
      # Create empty text lines with a few extra, if needed
      textlines <- rep("", k + 10)
      # Current text line
      i <- 1
      # Decide whether to add a word to the current line or to start a new line
      for (w in seq(k_words)) {
        # Width of current line before adding a new word
        current_width <- stringr::str_length(textlines[i])
        # Width of current line if a new word is added
        proposed_width <- current_width + word_len[w] + 1
        # Difference between current width and preferred width
        current_difference <- abs(current_width - preferred_width)
        # Difference between proposed width and preferred width
        proposed_difference <- abs(proposed_width - preferred_width)
        # Should we start a new line?
        if (
          current_difference < proposed_difference | proposed_width > max_width
        ) {
          i <- i + 1
        }
        # Add word to current line, remove spaces, and rejoin words divided by forward slashes
        textlines[i] <- stringr::str_trim(paste(textlines[i], words[w])) |>
          stringr::str_replace("/ ", "/")
      }
      # Collapse non-empty lines by separation character
      paste0(textlines[stringr::str_length(textlines) > 0], collapse = sep)
    }
  })
}

#' Tests if a character vector contains numeric-like values
#'
#' @param x character vector
#' @param elementwise if `TRUE`, returns a logical vector for each element, otherwise returns a single logical value indicating if all elements are numeric-like (default: `FALSE`)
#' @return logical vector
#' @export
#'
#' @examples
#' is_numeric_like(c("-9", " 2.0", "-1.0 "))
#' is_numeric_like(c("9-", -1, "10"))
#' is_numeric_like(c("9", -1.2, "10"))
is_numeric_like <- function(x, elementwise = FALSE) {
  x <- as.character(x) |>
    stringr::str_trim()

  test_x <- grepl("^-?[0-9.]+$", x) | is.na(x) | !nzchar(x)
  if (elementwise) {
    test_x
  } else {
    all(test_x)
  }
}

#' Pads text on the left or right so that the width is the same for each element of the vector
#'
#' `num_pad()` adds spaces to the left or right of each element of a text
#' vector so that all elements have the same width, counted in characters or
#' measured in a font. [align_chr()] uses it to line up numbers.
#'
#' @details
#' By default, text is padded with one `padding_character` per missing visible character. Markdown markup (e.g., `**`, `*`, `^`, `~`, and backslash escapes) is not counted. If `font_family` names an installed font, text is instead padded to equal *rendered* width in that font, using a combination of figure, punctuation, thin, and hair spaces (`padding_character` is then ignored). Superscripts and subscripts are measured at 2/3 of the font size.
#'
#' @param x vector of text
#' @param pad_left if TRUE (default), pads on the left, otherwise pads on the right
#' @param padding_character character to use for padding, default is `&numsp;` (figure space)
#' @param NA_value value to replace NA
#' @param font_family font in which to measure text widths. If `NULL` (default) or `NA`, or if the font is not installed, widths are counted in characters.
#'
#' @return character vector
#' @export
#'
#' @examples
#' num_pad(c("a", "bb"))
#' # A minus sign (U+2212) and a figure-space-padded 1 have the same width
#' num_pad(c("\u22121", "1"), font_family = "Times New Roman")
num_pad <- function(
  x,
  pad_left = TRUE,
  padding_character = "&numsp;",
  NA_value = "",
  font_family = NULL
) {
  width_aware <- !is.null(font_family) &&
    !is.na(font_family) &&
    font_available(font_family)

  if (width_aware) {
    w <- md_text_width(x, font_family)
    pad_chr <- space_fill(max(w) - w, font_family)
  } else {
    nch <- nchar(md_visible_text(x))
    nch[is.na(x)] <- 0
    pad_chr <- strrep(padding_character, max(nch) - nch)
  }

  x[is.na(x)] <- NA_value

  if (pad_left) {
    paste0(pad_chr, x)
  } else {
    paste0(x, pad_chr)
  }
}

# Width-aware padding ----

# Spaces used to fill padding, widest first
padding_spaces <- c(
  figure = "\u2007",
  punctuation = "\u2008",
  thin = "\u2009",
  hair = "\u200a"
)

md_entities <- c(
  "&numsp;" = "\u2007",
  "&puncsp;" = "\u2008",
  "&thinsp;" = "\u2009",
  "&hairsp;" = "\u200a",
  "&nbsp;" = "\u00a0",
  "&minus;" = "\u2212",
  "&times;" = "\u00d7",
  "&lt;" = "<",
  "&gt;" = ">",
  "&amp;" = "&"
)

#' Visible text of markdown, with scripts marked
#'
#' Removes emphasis markers and backslash escapes and converts common HTML
#' entities. Superscripts (`^x^`) and subscripts (`~x~`) are kept with their
#' delimiters so callers can measure them separately.
#' @keywords internal
#' @noRd
md_strip <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringr::str_replace_all(x, stringr::fixed(md_entities))
  # Other entities (e.g., &sigma;) are one glyph; approximate with "x"
  x <- stringr::str_replace_all(x, "&[A-Za-z0-9#]+;", "x")
  # Protect escaped characters, then drop unescaped emphasis markers
  x <- stringr::str_replace_all(x, "\\\\\\*", "\u0001")
  x <- stringr::str_remove_all(x, "\\*")
  x <- stringr::str_replace_all(x, "\\\\(.)", "\\1")
  stringr::str_replace_all(x, "\u0001", "*")
}

#' Visible text of markdown (scripts flattened)
#' @keywords internal
#' @noRd
md_visible_text <- function(x) {
  out <- stringr::str_remove_all(md_strip(x), "[\\^~]")
  out[is.na(x)] <- NA
  out
}

#' Width of text in em
#'
#' Sums the advance widths of individual characters, ignoring kerning. Word
#' does not kern body text by default, and Times New Roman kerns some digit
#' pairs (e.g., "11"), so kerned widths would misalign Word tables.
#' @keywords internal
#' @noRd
string_width_em <- function(x, font_family) {
  out <- numeric(length(x))
  has_text <- !is.na(x) & nzchar(x)
  if (any(has_text)) {
    size <- 1000
    chars <- strsplit(x[has_text], "")
    glyphs <- unique(unlist(chars))
    glyph_width <- systemfonts::string_width(
      glyphs,
      family = font_family,
      size = size,
      res = 72
    ) / size
    names(glyph_width) <- glyphs
    out[has_text] <- vapply(chars, \(g) sum(glyph_width[g]), numeric(1))
  }
  out
}

#' Rendered width of markdown text in em
#'
#' Superscripts and subscripts are measured at `script_scale` times the font size.
#' @keywords internal
#' @noRd
md_text_width <- function(
  x,
  font_family = the$font_family,
  script_scale = the$script_scale
) {
  script_re <- "\\^[^\\^]*\\^|~[^~]*~"
  s <- md_strip(x)
  scripts <- stringr::str_extract_all(s, script_re) |>
    purrr::map_chr(\(p) stringr::str_remove_all(paste0(p, collapse = ""), "[\\^~]"))
  base <- stringr::str_remove_all(s, script_re)
  string_width_em(base, font_family) +
    script_scale * string_width_em(scripts, font_family)
}

#' Is a font installed?
#' @keywords internal
#' @noRd
font_available <- function(font_family) {
  if (is.null(the$font_available)) {
    the$font_available <- list()
  }
  if (is.null(the$font_available[[font_family]])) {
    families <- c(
      systemfonts::system_fonts()$family,
      systemfonts::registry_fonts()$family
    )
    the$font_available[[font_family]] <- tolower(font_family) %in%
      tolower(families)
  }
  the$font_available[[font_family]]
}

#' Spaces that fill a width
#'
#' Uses as many figure spaces as fit, then the combination of punctuation,
#' thin, and hair spaces that best fills the remainder.
#' @param deficit width to fill, in em
#' @keywords internal
#' @noRd
space_fill <- function(deficit, font_family) {
  w <- string_width_em(padding_spaces, font_family)
  names(w) <- names(padding_spaces)
  combos <- expand.grid(punctuation = 0:1, thin = 0:2, hair = 0:5)
  combos$width <- as.matrix(combos) %*% w[names(combos)]
  combos$n <- rowSums(combos[, c("punctuation", "thin", "hair")])
  # Fewest characters among the most accurate combinations
  combos <- combos[order(combos$n), ]

  purrr::map_chr(deficit, \(d) {
    if (is.na(d) || d <= 0) {
      return("")
    }
    n_figure <- floor(d / w[["figure"]] + 1e-6)
    remainder <- d - n_figure * w[["figure"]]
    best <- combos[which.min(round(abs(remainder - combos$width), 4)), ]
    paste0(
      strrep(padding_spaces[["figure"]], n_figure),
      strrep(padding_spaces[["punctuation"]], best$punctuation),
      strrep(padding_spaces[["thin"]], best$thin),
      strrep(padding_spaces[["hair"]], best$hair)
    )
  })
}

#' Scientific notation for markdown
#'
#' @param x numeric vector
#' @param digits number of decimal places (of the mantissa, for numbers in scientific notation)
#' @param lb,ub numbers at or below `lb` or at or above `ub` are written in scientific notation (e.g., 2.46 × 10^7^); numbers between them are written as usual
#' @param align if `TRUE`, the numbers are aligned on the decimal point with [align_chr()]
#'
#' @returns A character vector
#' @export
#'
#' @examples
#' sci_md(c(.0045,1.44, 24567857.45))
sci_md <- function(x, digits = 2, lb = 0.0095, ub = 99.5, align = FALSE) {
  x_sci <- formatC(x, digits = digits, format = "e")
  x_sci_left <- stringr::str_split_i(x_sci, pattern = "e", i = 1)
  x_sci_right <- stringr::str_split_i(x_sci, pattern = "e", i = 2) |>
    as.numeric() |>
    signs::signs()

  x_md <- formatC(x, digits = digits, format = "f")

  out_range <- which(x >= ub | x <= lb)
  x_sci_right[out_range] <- paste0("\u00D7", "10^", x_sci_right[out_range], "^")

  # Values within the bounds use fixed notation, not the mantissa alone
  in_range <- !is.na(x) & x > lb & x < ub
  x_sci_md <- paste0(x_sci_left, x_sci_right)
  x_sci_md[in_range] <- x_md[in_range]
  x_sci_md[is.na(x)] <- NA

  if (align) {
    x_sci_md <- align_chr(x_sci_md)
  }

  x_sci_md
}

#' Combine formatted interval bounds into "\[LL, UL\]"
#'
#' `l` and `u` are already formatted and aligned. Rows flagged in `missing`
#' are blank.
#' @keywords internal
#' @noRd
ci_md <- function(l, u, missing = FALSE) {
  middle <- rep(", ", length(l))
  middle[missing] <- ""
  l[missing] <- ""
  u[missing] <- ""
  align_chr(paste0(l, middle, u), center = ", ") |>
    tagger("[", "]")
}

#' Re-align formatted "\[LL, UL\]" intervals
#'
#' Used when intervals formatted separately (e.g., one table per model) are
#' stacked into one column.
#' @keywords internal
#' @noRd
realign_ci <- function(x) {
  missing <- is.na(x) | !nzchar(stringr::str_trim(x))
  inner <- stringr::str_remove_all(x, "^\\[|\\]$")
  ci_md(
    align_chr(stringr::str_split_i(inner, ", ", 1)),
    align_chr(stringr::str_split_i(inner, ", ", 2)),
    missing = missing
  )
}

#' Is a character vector made of formatted "\[LL, UL\]" intervals?
#' @keywords internal
#' @noRd
is_ci_md <- function(x) {
  x <- x[!is.na(x) & nzchar(stringr::str_trim(x))]
  is.character(x) && length(x) > 0 && all(grepl("^\\[.+, .+\\]$", x))
}

#' Convert p-values to stars
#'
#' @param p vector of numbers
#' @param alpha vector of thresholds
#' @param first_alpha_marginal if TRUE, the first alpha value is treated as marginal and gets a dagger instead of a star
#' @param prefix usually backslashes to prevent markdown from interpreting asterisks as bullets or italics
#' @param superscript make as superscript
#' @param add_trailing_space if TRUE, adds a trailing space after the stars (default: FALSE)
#'
#' @return character vector
#' @export
#'
#' @examples
#' p2stars(c(.32, .02, .005),
#'         alpha = c(.05, .01))
p2stars <- function(
  p,
  alpha = c(0.05, .01, .001),
  first_alpha_marginal = FALSE,
  superscript = FALSE,
  add_trailing_space = FALSE,
  prefix = "\\"
) {
  pstars <- purrr::map_chr(p, \(pp) {
    if (is.na(pp)) {
      return("")
    }
    paste0(rep(paste0(prefix, "*"), sum((pp <= alpha) * 1L)), collapse = "")
  })
  if (first_alpha_marginal) {
    # if the first value is marginal, we don't want to add a star
    pstars[pstars == paste0(prefix, "*")] <- "\u2020"
    # Remove initial star
    if (prefix == "\\") {
      pstars <- stringr::str_remove(pstars, "\\\\\\*")
    } else {
      pstars <- stringr::str_remove(pstars, paste0(prefix, "\\*"))
    }
  }
  trailing_space <- ifelse(add_trailing_space, "\u2009", "")
  if (superscript) {
    pstars[nchar(pstars) > 0] <- paste0(
      "^",
      pstars[nchar(pstars) > 0],
      trailing_space,
      "^"
    )
  } else {
    pstars[nchar(pstars) > 0] <- paste0(
      pstars[nchar(pstars) > 0],
      trailing_space
    )
  }
  pstars
}
# p2stars(.10, alpha = c(.10, .05, .01, .001), first_alpha_marginal = FALSE)

#' Surrounds text with tags unless empty
#'
#' @param x character vector
#' @param tag opening tag, e.g., `<span>` or `**`
#' @param right_tag closing tag. By default, the opening tag with `<` replaced by `</` (e.g., `<span>` becomes `</span>`), so markdown tags such as `**` close with themselves.
#'
#' @return character vector
#' @export
#'
#' @examples
#' x <- c("hello", "", NA)
#' tagger(x, "<span>")
#' bold_md(x)
#' italic_md(x)
#' superscript_md(x)
#' subscript_md(x)
#' header_md("Level 1")
#' header_md("Level 2", 2)
tagger <- function(x, tag = "<span>", right_tag = gsub("^<", "</", tag)) {
  y <- paste0(tag, x, right_tag)
  y[is.na(x)] <- NA
  y[!nzchar(x)] <- ""
  y
}

#' @rdname tagger
#' @export
bold_md <- function(x) {
  tagger(x, "**")
}

#' @rdname tagger
#' @export
italic_md <- function(x) {
  tagger(x, "*")
}

#' @rdname tagger
#' @export
superscript_md <- function(x) {
  tagger(x, "^")
}

#' @rdname tagger
#' @export
subscript_md <- function(x) {
  tagger(x, "~")
}

#' @rdname tagger
#' @param level heading level
#' @export
header_md <- function(x, level = 1) {
  tagger(x, paste0(paste0(rep("#", level), collapse = ""), " "), right_tag = "")
}

# Column format ----
#' Column format class
#'
#' This class is used to define the format of columns in tables, including the name, header, latex representation, a formatter function, and what the header abbreviates.
#'
#' @details
#' A `column_format` is an S7 object. Its properties are read and changed with
#' `@` (e.g., `R2@header`, or `R2@formatter <- my_function`). Formats are
#' collected in a [column_formats()] object, in which each format is read,
#' added, or replaced with `$` (e.g., `column_formats()$p`). The table
#' functions use a format for every column with the same name as the format.
#'
#' @param name name of column
#' @param header markdown representation of header name
#' @param latex latex representation of header name
#' @param formatter function that formats the column values. It should take a vector of values and return a character vector of formatted values.
#' @param abbreviation what the header stands for, used by [apa_note()] to define the abbreviations in a table's note (e.g., `"standardized coefficient"` defines the `&beta;` header as `"&beta; = standardized coefficient"`). The term defined is the header, unless the string is named (e.g., `c(CI = "confidence interval")` defines "CI" for a header such as "95% CI"). Leave empty (the default) for symbols that need no definition, such as *M*, *SD*, *t*, and *p*.
#' @export
#' @return column_format object
#' @examples
#' R2 <- column_format(
#'          "R2",
#'          header = "*R*^2^",
#'          latex = "$R^2$",
#'          formatter = \(x, accuracy = .01, ...) {
#'                        align_chr(x,
#'                                  accuracy = accuracy,
#'                                  trim_leading_zeros = TRUE,
#'                                  ...)
#'                        })
#' R2
#' R2@header
#' R2@formatter(c(.123, .45))
#'
#' # Define an abbreviation in table notes
#' SS <- column_formats()$Sum_Squares
#' SS@abbreviation <- "sum of squares"
column_format <- new_class(
  "column_format",
  properties = list(
    name = class_character,
    header = class_character,
    latex = class_character,
    formatter = class_function,
    abbreviation = class_character
  ),
  validator = function(self) {
    if (length(self@abbreviation) > 1) {
      "@abbreviation must be a single string (or empty)"
    }
  }
)

as_tibble <- S7::new_external_generic(
  package = "tibble",
  name = "as_tibble",
  dispatch_args = "x"
)

method(as_tibble, column_format) <- function(x, ...) {
  tibble::tibble(
    name = x@name,
    header = x@header,
    latex = ifelse(is.null(x@latex), NA, x@latex),
    formatter = list(x@formatter),
    abbreviation = abbreviation_text(x)
  )
}

#' The abbreviation of a column format as "term = definition" (or NA)
#' @keywords internal
#' @noRd
abbreviation_text <- function(x) {
  a <- column_abbreviations(x@name, stats::setNames(list(x), x@name))
  if (length(a) == 0) NA_character_ else paste0(names(a), " = ", a)
}

method(str, column_format) <- function(object, ...) {
  cli::cli_h2(S7_class(object)@name)
  print(tibble::as_tibble(object))
  invisible(object)
}

#' @keywords internal
#' @noRd
variable_name_formatter <- function(
  x,
  pattern = NULL,
  replacement = NULL,
  formatter = NULL
) {
  # Rules are applied in order: specific exponent rules must come before the
  # generic caret rule, which would otherwise consume their carets
  pre_p2r <- c(
    `\\:` = " : ",
    `\\(Intercept\\)` = the$intercept_text,
    ` \\[linear\\]$` = "",
    ` \\[quadratic\\]$` = " Apasevencaret 2 Apasevencaret",
    ` \\[cubic\\]$` = " Apasevencaret 3 Apasevencaret",
    # poly() terms (e.g., "x [1st degree]", "x [2nd degree]")
    ` \\[1st degree\\]$` = "",
    ` \\[(\\d+)(?:st|nd|rd|th) degree\\]$` = " Apasevencaret \\1 Apasevencaret",
    # Exponents, with (x^2^) or without (x^2, from I(x^2)) a closing caret
    `\\^(\\d+)\\^` = " Apasevencaret \\1 Apasevencaret",
    `\\^(\\d+)` = " Apasevencaret \\1 Apasevencaret",
    `\\^` = "Apasevencaret",
    `\\u2013` = "Apasevenendash ",
    # Ordinal thresholds (e.g., "Low|High")
    ` ?\\| ?` = " Apasevenpipe ",
    ` \\[([^]]+)\\]` = "Apasevenendash \\1",
    `\\&numsp;` = "Apasevennumsp",
    `\\&nbsp;` = "Apasevennbsp",
    # "vs." in paired-test labels (e.g., "Pre vs. Post")
    ` vs\\. ` = " Apasevenvs "
  )

  post_p2r <- c(
    ` ?Apasevenendash ?` = "\u2013",
    ` ?Apasevenpipe ?` = " | ",
    ` ?Apasevenvs ?` = " vs. ",
    ` Apasevendotspace` = ".\u2007",
    `Apasevendotspace` = ".\u2007",
    `Apasevennumsp` = "\u2007",
    `Apaseventhinsp` = "\u2009",
    `Apasevennbsp` = "\u00A0",
    `\\(` = " (",
    `\\)` = ") ",
    `^ \\(` = "(",
    `\\) $` = ")",
    ` Apasevencaret (\\d+) Apasevencaret` = "^\\1^"
  )

  # Protect all-caps words (e.g., ACT, SAT-V) from title casing
  caps_regex <- "(?<![A-Za-z0-9])[A-Z]{2,}[A-Z0-9]*(?:[-_][A-Z0-9]+)*(?![A-Za-z0-9])"
  x <- stringr::str_replace_all(x, pre_p2r)
  caps_words <- unique(unlist(stringr::str_extract_all(x, caps_regex)))
  caps_words <- caps_words[order(-nchar(caps_words))]
  caps_keys <- paste0(
    "Apasevencap",
    chartr("0123456789", "abcdefghij", seq_along(caps_words)),
    "z"
  )
  for (i in seq_along(caps_words)) {
    x <- stringr::str_replace_all(
      x,
      paste0("(?<![A-Za-z0-9])", caps_words[i], "(?![A-Za-z0-9])"),
      caps_keys[i]
    )
  }

  x <- x |>
    stringr::str_replace_all("\\.\u2007", "Apasevendotspace") |>
    stringr::str_replace_all("\u2007", "Apasevennumsp") |>
    stringr::str_replace_all("\u00A0", "Apasevennbsp") |>
    stringr::str_replace_all("\u2009", "Apaseventhinsp") |>
    snakecase::to_title_case(
      abbreviations = c(
        ":",
        "\u00D7",
        "\\^",
        "\\(",
        "\\)",
        "\\$",
        "\\]",
        "\\[",
        "s\\)",
        "\\.\u2007",
        "\u2007",
        "\\\\\\\n",
        "\u00A0",
        "\u2007",
        "&thinsp;",
        "&numsp;",
        "&nbsp;",
        "Apasevendotspace",
        "Apasevennumsp",
        "Apasevennbsp",
        "Apaseventhinsp",
        "Apasevencaret",
        "Apasevenendash",
        "Apasevenpipe",
        "Apasevenvs",
        "\u2013",
        "\t",
        "\n"
      )
    ) |>
    stringr::str_replace_all(post_p2r)

  for (i in seq_along(caps_words)) {
    x <- stringr::str_replace_all(
      x,
      stringr::fixed(caps_keys[i]),
      gsub("_", " ", caps_words[i], fixed = TRUE)
    )
  }

  x <- x |>
    stringr::str_replace_all("\u2013(\\p{Ll})", \(m) {
      paste0("\u2013", toupper(substring(m, 2)))
    }) |>
    stringr::str_replace_all(" \u00D7 ", "\u00D7") |>
    stringr::str_replace_all("\u00D7", " \u00D7 ")

  if (!is.null(pattern) && !is.null(replacement)) {
    x <- stringr::str_replace_all(x, pattern, replacement)
  }
  x
}

# internal states ----
the <- new.env(parent = emptyenv())
the$accuracy <- .01
the$intercept_text <- "Constant"
the$font_family <- "Times New Roman"
the$font_size <- 12
the$line_spacing <- 2
the$page_width <- 6.5
# Pad to equal rendered width in the$font_family (see num_pad)
the$width_aware_padding <- TRUE
# accuracy, intercept_text, font_family, font_size, line_spacing, page_width,
# and width_aware_padding are replaced when the package is loaded by active bindings that read the
# apa7.* options (see bind_option_settings())
# Superscripts and subscripts are drawn smaller than the base text. Word draws
# them at about 2/3 of the font size; browsers at about 0.83.
the$script_scale <- 2 / 3
default_number_formatter <- function(x, accuracy = the$accuracy, ...) {
  align_chr(x, accuracy = accuracy, ...)
}
the$number_formatter <- default_number_formatter

the$sci_number_formatter <- \(x, digits = 2, ...) {
  sci_md(x, digits = digits, align = TRUE)
}
default_pvalue_formatter <- function(x, accuracy = the$accuracy, ...) {
  max_digits <- 3
  min_digits <- round(-log10(accuracy))
  if (max_digits < min_digits) {
    max_digits <- min_digits
  }
  align_chr(
    apa_p(
      x,
      markdown = TRUE,
      min_digits = min_digits,
      max_digits = max_digits,
      ...
    ),
    format_numeric_character = FALSE
  )
}
the$pvalue_formatter <- default_pvalue_formatter

default_trim_leading_zero <- function(x, accuracy = the$accuracy, ...) {
  align_chr(x, accuracy = accuracy, trim_leading_zeros = TRUE, ...)
}
the$trim_leading_zero <- default_trim_leading_zero
the$variable_name_formatter <- variable_name_formatter

# Columns ----
the$columns <- list(
  alpha = column_format(
    name = "alpha",
    header = "&alpha;",
    latex = "$\\alpha$",
    formatter = the$trim_leading_zero
  ),
  AIC = column_format(
    name = "AIC",
    header = "*AIC*",
    latex = "$AIC$",
    formatter = the$number_formatter,
    abbreviation = "Akaike information criterion"
  ),
  AICc = column_format(
    name = "AICc",
    header = "*AICc*",
    latex = "$AICc$",
    formatter = the$number_formatter,
    abbreviation = "corrected Akaike information criterion"
  ),
  AIC_wt = column_format(
    name = "AIC_wt",
    header = "*AIC* weight",
    latex = "$AIC$ weight",
    formatter = the$number_formatter
  ),
  AICc_wt = column_format(
    name = "AICc_wt",
    header = "*AICc* weight",
    latex = "$AICc$ weight",
    formatter = the$number_formatter
  ),
  B = column_format(
    name = "B",
    header = "*B*",
    latex = "$B$",
    formatter = the$number_formatter
  ),
  b = column_format(
    name = "b",
    header = "*b*",
    latex = "$b$",
    formatter = the$number_formatter
  ),
  beta = column_format(
    name = "beta",
    header = "&beta;",
    latex = "$\\beta$",
    formatter = the$trim_leading_zero
  ),
  BIC = column_format(
    name = "BIC",
    header = "*BIC*",
    latex = "$BIC$",
    formatter = the$number_formatter,
    abbreviation = "Bayesian information criterion"
  ),
  BICc = column_format(
    name = "BICc",
    header = "*BICc*",
    latex = "$BICc$",
    formatter = the$number_formatter,
    abbreviation = "corrected Bayesian information criterion"
  ),
  BIC_wt = column_format(
    name = "BIC_wt",
    header = "*BIC* weight",
    latex = "$BIC$ weight",
    formatter = the$number_formatter
  ),
  Chi2 = column_format(
    name = "Chi2",
    header = "*&chi;*^2^",
    latex = "$\\chi^2$",
    formatter = the$number_formatter
  ),
  `Chi2 value` = column_format(
    name = "Chi2 value",
    header = "*&chi;*^2^",
    latex = "$\\chi^2$",
    formatter = the$number_formatter
  ),
  Chi2_df = column_format(
    name = "Chi2_df",
    header = "*df*",
    latex = "$df$",
    formatter = the$number_formatter
  ),
  chisq = column_format(
    name = "chisq",
    header = "*&chi;*^2^",
    latex = "$\\chi^2$",
    formatter = the$number_formatter
  ),
  Chisq = column_format(
    name = "Chisq",
    header = "*&chi;*^2^",
    latex = "$\\chi^2$",
    formatter = the$number_formatter
  ),
  p_Chi2 = column_format(
    name = "p_Chi2",
    header = "*p*",
    latex = "$p$",
    formatter = the$pvalue_formatter
  ),
  CFI = column_format(
    name = "CFI",
    header = "CFI",
    latex = "CFI",
    formatter = the$trim_leading_zero,
    abbreviation = "comparative fit index"
  ),
  CI = column_format(
    name = "CI",
    header = "{round(ci * 100)}% CI",
    latex = "{round(ci * 100)}\\% CI",
    formatter = \(l, u, accuracy = the$accuracy, ...) {
      ci_md(
        align_chr(l, accuracy = accuracy, ...),
        align_chr(u, accuracy = accuracy, ...),
        missing = is.na(l) | is.na(u)
      )
    },
    abbreviation = c(CI = "confidence interval")
  ),
  CI_high = column_format(
    name = "CI_high",
    header = "UL",
    latex = "UL",
    formatter = the$number_formatter,
    abbreviation = "upper limit"
  ),
  CI_low = column_format(
    name = "CI_low",
    header = "LL",
    latex = "LL",
    formatter = the$number_formatter,
    abbreviation = "lower limit"
  ),
  CI_percent = column_format(
    name = "CI_percent",
    header = "{p}% CI",
    latex = "{p}\\% CI",
    formatter = function(x, accuracy = the$accuracy) {
      x_split <- stringr::str_replace_all(
        x,
        c(`\\[` = "", `\\]` = "")
      ) |>
        stringr::str_split_fixed(",", n = 2)
      lb <- x_split[, 1] |> align_chr(accuracy = accuracy)
      ub <- x_split[, 2] |> align_chr(accuracy = accuracy)
      x_new <- paste0("[", lb, ", ", ub, "]")
      x_new[nchar(x) == 0 | is.na(x) | x == ""] <- NA
      x_new
    },
    abbreviation = c(CI = "confidence interval")
  ),
  cohens_d = column_format(
    name = "cohens_d",
    header = "Cohen's *d*",
    latex = "Cohen's $d$",
    formatter = the$number_formatter
  ),
  Cohens_d = column_format(
    name = "Cohens_d",
    header = "Cohen's *d*",
    latex = "Cohen's $d$",
    formatter = the$number_formatter
  ),
  Coefficient = column_format(
    name = "Coefficient",
    header = "*B*",
    latex = "$B$",
    formatter = the$number_formatter
  ),
  Cohens_w = column_format(
    name = "Cohens_w",
    header = "Cohen's *w*",
    latex = "Cohen's $w$",
    formatter = the$number_formatter
  ),
  Cramers_v = column_format(
    name = "Cramers_v",
    header = "Cram\u00E9r's *V*",
    latex = "Cram\u00E9r's $V$",
    formatter = the$trim_leading_zero
  ),
  cronbach = column_format(
    name = "cronbach",
    header = "Cronbach's &alpha;",
    latex = "Cronbach's $\\alpha$",
    formatter = the$trim_leading_zero
  ),
  d_av = column_format(
    name = "d_av",
    header = "*d*~av~",
    latex = "$d_{\\text{av}}$",
    formatter = the$number_formatter
  ),
  d_z = column_format(
    name = "d_z",
    header = "*d*~z~",
    latex = "$d_{\\text{z}}$",
    formatter = the$number_formatter
  ),
  d_rm = column_format(
    name = "d_rm",
    header = "*d*~rm~",
    latex = "$d_{\\text{rm}}$",
    formatter = the$number_formatter
  ),
  g_av = column_format(
    name = "g_av",
    header = "*g*~av~",
    latex = "$g_{\\text{av}}$",
    formatter = the$number_formatter
  ),
  g_z = column_format(
    name = "g_z",
    header = "*g*~z~",
    latex = "$g_{\\text{z}}$",
    formatter = the$number_formatter
  ),
  g_rm = column_format(
    name = "g_rm",
    header = "*g*~rm~",
    latex = "$g_{\\text{rm}}$",
    formatter = the$number_formatter
  ),
  deltaR2 = column_format(
    name = "deltaR2",
    header = "&Delta;*R*^2^",
    latex = "$\\Delta R^2$",
    formatter = the$trim_leading_zero,
    abbreviation = "change in *R*^2^"
  ),
  deltaR2_conditional = column_format(
    name = "deltaR2_conditional",
    header = "&Delta;*R*^2^~conditional~",
    latex = "$\\Delta R^2_{\\text{conditional}}$",
    formatter = the$trim_leading_zero,
    abbreviation = "change in *R*^2^~conditional~"
  ),
  deltaAIC = column_format(
    name = "deltaAIC",
    header = "&Delta;*AIC*",
    latex = "$\\Delta AIC$",
    formatter = the$number_formatter
  ),
  deltaBIC = column_format(
    name = "deltaBIC",
    header = "&Delta;*BIC*",
    latex = "$\\Delta BIC$",
    formatter = the$number_formatter
  ),
  deltachi2 = column_format(
    name = "deltachi2",
    header = "&Delta;&chi;^2^",
    latex = "$\\Delta \\chi^2$",
    formatter = the$number_formatter
  ),
  df = column_format(
    name = "df",
    header = "*df*",
    latex = "$df$",
    formatter = the$number_formatter
  ),
  edf = column_format(
    name = "edf",
    header = "*edf*",
    latex = "$edf$",
    formatter = the$number_formatter,
    abbreviation = "effective degrees of freedom"
  ),
  df_diff = column_format(
    name = "df_diff",
    header = "&Delta;*df*",
    latex = "$\\Delta df$",
    formatter = the$number_formatter
  ),
  df_error = column_format(
    name = "df",
    header = "*df*",
    latex = "$df$",
    formatter = the$number_formatter
  ),
  eta2 = column_format(
    name = "eta2",
    header = "*&eta;*^2^",
    latex = "$\\eta^2$",
    formatter = the$trim_leading_zero,
    abbreviation = "eta squared"
  ),
  Eta2 = column_format(
    name = "eta2",
    header = "*&eta;*^2^",
    latex = "$\\eta^2$",
    formatter = the$trim_leading_zero,
    abbreviation = "eta squared"
  ),
  Eta2_partial = column_format(
    name = "Eta2_partial",
    header = "*&eta;*^2^~p~",
    latex = "$\\eta^2_p$",
    formatter = the$trim_leading_zero,
    abbreviation = "partial eta squared"
  ),
  Eta2_generalized = column_format(
    name = "Eta2_generalized",
    header = "*&eta;*^2^~G~",
    latex = "$\\eta^2_G$",
    formatter = the$trim_leading_zero,
    abbreviation = paste0("generalized eta squared ", cite("olejnikGeneralizedEtaOmega2003"))
  ),
  Epsilon2 = column_format(
    name = "Epsilon2",
    header = "*&epsilon;*^2^",
    latex = "$\\epsilon^2$",
    formatter = the$trim_leading_zero,
    abbreviation = "epsilon squared"
  ),
  Epsilon2_partial = column_format(
    name = "Epsilon2_partial",
    header = "*&epsilon;*^2^~p~",
    latex = "$\\epsilon^2_p$",
    formatter = the$trim_leading_zero,
    abbreviation = "partial epsilon squared"
  ),
  Exp_Coefficient = column_format(
    name = "Exp_Coefficient",
    header = "Exp(*B*)",
    latex = "Exp($B$)",
    formatter = the$sci_number_formatter,
    abbreviation = "exponentiated coefficient"
  ),
  Exp_CI = column_format(
    name = "Exp_CI",
    header = "{round(ci * 100)}% CI for Exp(*B*)",
    latex = "{round(ci * 100)}\\% CI for Exp($B$)",
    formatter = \(l, u, digits = 2, ...) {
      ci_md(
        sci_md(l, digits = digits, align = TRUE),
        sci_md(u, digits = digits, align = TRUE),
        missing = is.na(l) | is.na(u)
      )
    },
    abbreviation = c(CI = "confidence interval")
  ),
  `F` = column_format(
    name = "F",
    header = "*F*",
    latex = "$F$",
    formatter = the$number_formatter
  ),
  m = column_format(
    name = "m",
    header = "*m*",
    latex = "$m$",
    formatter = the$number_formatter
  ),
  M = column_format(
    name = "M",
    header = "*M*",
    latex = "$M$",
    formatter = the$number_formatter
  ),
  Mean = column_format(
    name = "Mean",
    header = "*Mean*",
    latex = "$Mean$",
    formatter = the$number_formatter
  ),
  Mdn = column_format(
    name = "Mdn",
    header = "*Mdn*",
    latex = "$Mdn$",
    formatter = the$number_formatter
  ),
  n = column_format(
    name = "n",
    header = "*n*",
    latex = "$n$",
    formatter = the$number_formatter
  ),
  N = column_format(
    name = "N",
    header = "*N*",
    latex = "$N$",
    formatter = the$number_formatter
  ),
  NFI = column_format(
    name = "NFI",
    header = "NFI",
    latex = "NFI",
    formatter = the$trim_leading_zero,
    abbreviation = "normed fit index"
  ),
  omega = column_format(
    name = "omega",
    header = "&omega;",
    latex = "$\\omega$",
    formatter = the$trim_leading_zero
  ),
  Omega2 = column_format(
    name = "Omega2",
    header = "*&omega;*^2^",
    latex = "$\\omega^2$",
    formatter = the$trim_leading_zero,
    abbreviation = "omega squared"
  ),
  Omega2_partial = column_format(
    name = "Omega2_partial",
    header = "*&omega;*^2^~p~",
    latex = "$\\omega^2_p$",
    formatter = the$trim_leading_zero,
    abbreviation = "partial omega squared"
  ),
  Mean_Square = column_format(
    name = "Mean_Square",
    header = "*MS*",
    latex = "$MS$",
    formatter = the$number_formatter
  ),
  p = column_format(
    name = "p",
    header = "*p*",
    latex = "$p$",
    formatter = the$pvalue_formatter
  ),
  Parameter = column_format(
    name = "Parameter",
    header = "Variable",
    latex = "Variable",
    formatter = the$variable_name_formatter
  ),
  phi = column_format(
    name = "phi",
    header = "&phi;",
    latex = "$\\phi$",
    formatter = the$trim_leading_zero
  ),
  Phi = column_format(
    name = "Phi",
    header = "&Phi;",
    latex = "$\\Phi$",
    formatter = the$number_formatter
  ),
  r = column_format(
    name = "r",
    header = "*r*",
    latex = "$r$",
    formatter = the$trim_leading_zero
  ),
  r_rank_biserial = column_format(
    name = "r_rank_biserial",
    header = "*r*~rb~",
    latex = "$r_{\\text{rb}}$",
    formatter = the$trim_leading_zero,
    abbreviation = "rank-biserial correlation"
  ),
  rho = column_format(
    name = "rho",
    header = "&rho;",
    latex = "$\\rho$",
    formatter = the$trim_leading_zero,
    abbreviation = "Spearman's rank correlation"
  ),
  R2 = column_format(
    name = "R2",
    header = "*R*^2^",
    latex = "$R^2$",
    formatter = the$trim_leading_zero
  ),
  R2_adjusted = column_format(
    name = "R2_adjusted",
    header = "adj*R*^2^",
    latex = "$\\text{adj}R^2$",
    formatter = the$trim_leading_zero
  ),
  R2_conditional = column_format(
    name = "R2_conditional",
    header = "*R*^2^~conditional~",
    latex = "$R^2_{\\text{conditional}}$",
    formatter = the$trim_leading_zero,
    abbreviation = "variance explained by fixed and random effects"
  ),
  R2_marginal = column_format(
    name = "R2_marginal",
    header = "*R*^2^~marginal~",
    latex = "$R^2_{\\text{marginal}}$",
    formatter = the$trim_leading_zero,
    abbreviation = "variance explained by fixed effects"
  ),
  Hedges_g = column_format(
    name = "Hedges_g",
    header = "Hedges' *g*",
    latex = "Hedges' $g$",
    formatter = the$number_formatter
  ),
  h2 = column_format(
    name = "h2",
    header = "*h*^2^",
    latex = "$h^2$",
    formatter = the$trim_leading_zero,
    abbreviation = "communality"
  ),
  u2 = column_format(
    name = "u2",
    header = "*u*^2^",
    latex = "$u^2$",
    formatter = the$trim_leading_zero,
    abbreviation = "uniqueness"
  ),
  Complexity = column_format(
    name = "Complexity",
    header = "Complexity",
    latex = "Complexity",
    formatter = the$number_formatter,
    abbreviation = "Hofmann's complexity index"
  ),
  ICC = column_format(
    name = "ICC",
    header = "ICC",
    latex = "ICC",
    formatter = the$trim_leading_zero,
    abbreviation = "intraclass correlation coefficient"
  ),
  RMSE = column_format(
    name = "RMSE",
    header = "*RMSE*",
    latex = "$RMSE$",
    formatter = the$number_formatter,
    abbreviation = "root mean square error"
  ),
  RMSEA = column_format(
    name = "RMSEA",
    header = "RMSEA",
    latex = "RMSEA",
    formatter = the$trim_leading_zero,
    abbreviation = "root mean square error of approximation"
  ),
  SRMR = column_format(
    name = "SRMR",
    header = "SRMR",
    latex = "SRMR",
    formatter = the$trim_leading_zero,
    abbreviation = "standardized root mean square residual"
  ),
  TLI = column_format(
    name = "TLI",
    header = "TLI",
    latex = "TLI",
    formatter = the$trim_leading_zero,
    abbreviation = "Tucker\u2013Lewis index"
  ),
  s = column_format(
    name = "s",
    header = "*s*",
    latex = "$s$",
    formatter = the$number_formatter
  ),
  SD = column_format(
    name = "SD",
    header = "*SD*",
    latex = "$SD$",
    formatter = the$number_formatter
  ),
  SE = column_format(
    name = "SE",
    header = "*SE*",
    latex = "$SE$",
    formatter = the$number_formatter
  ),
  Sum_Squares = column_format(
    name = "Sum_Squares",
    header = "*SS*",
    latex = "$SS$",
    formatter = the$number_formatter
  ),
  SE_B = column_format(
    name = "SE_B",
    header = "*SE_B*",
    latex = "$SE~B$",
    formatter = the$number_formatter
  ),
  Sigma = column_format(
    name = "Sigma",
    header = "&sigma;~*e*~",
    latex = "$\\sigma_{e}$",
    formatter = the$number_formatter,
    abbreviation = "residual standard deviation"
  ),
  Std_Coefficient = column_format(
    name = "Std_Coefficient",
    header = "&beta;",
    latex = "$\\beta$",
    formatter = the$trim_leading_zero,
    abbreviation = "standardized coefficient"
  ),
  t = column_format(
    name = "t",
    header = "*t*",
    latex = "$t$",
    formatter = the$number_formatter
  ),
  t_df = column_format(
    name = "t_df",
    header = "*t*({df})",
    latex = "$t$({df})",
    formatter = the$number_formatter
  ),
  tau = column_format(
    name = "tau",
    header = "&tau;",
    latex = "$\\tau$",
    formatter = the$number_formatter
  ),
  V = column_format(
    name = "V",
    header = "*V*",
    latex = "$V$",
    formatter = the$number_formatter
  ),
  Variable = column_format(
    name = "Variable",
    header = "Variable",
    latex = "Variable",
    formatter = the$variable_name_formatter
  ),
  W = column_format(
    name = "W",
    header = "*W*",
    latex = "$W$",
    formatter = the$number_formatter
  ),
  z = column_format(
    name = "z",
    header = "*z*",
    latex = "$z$",
    formatter = the$number_formatter
  )
)

# column_formats ----
#' Create a set of column formats
#'
#' Returns an S7 object that contains a list of `column_format` objects that can be used to format parameters in APA style.
#'
#' @details
#' Each [column_format()] in the list is named by the column it formats.
#' Formats are read, added, replaced, and removed with `$` (e.g.,
#' `my_formats$beta`, `my_formats$Predictor <- column_format("Predictor")`, or
#' `my_formats$beta <- NULL`), and the properties of a single format are read
#' and changed with `@` (e.g., `my_formats$p@header`). The `get_` properties
#' listed under Slots return the names, headers, formatters, and so on of all
#' the formats at once (e.g., `my_formats@get_tibble`).
#'
#' Use the object with the `column_formats` argument of [apa_flextable()],
#' [apa_format_columns()], and the table functions, or set it for every table
#' with `apa7_defaults(column_formats = my_formats)`.
#'
#' @param .data list of `column_format` objects
#' @param accuracy rounding accuracy of the formatters (e.g., `.01` for two decimals). If `NULL` (default), the `accuracy` set with [apa7_defaults()].
#' @param intercept_text label of the intercept row (e.g., `"Constant"` or `"Intercept"`)
#' @param starred_columns names of columns to which significance stars are added
#' @param variable_labels named character vector that relabels variables: the names are the labels and the values are the variable names (e.g., `c("Parental Income" = "parental_income", "Number of Siblings" = "n_siblings")`)
#' @param custom_columns named list of [column_format()] objects that are added, or that replace the formats of the same name
#' @slot get_column_names getter for column names
#' @slot get_headers getter for column headers
#' @slot get_latex getter for column latex headers
#' @slot get_formatters getter for column formatters
#' @slot get_header_rename getter for column names with headers as names
#' @slot get_header_rename_latex getter for column names with latex headers as names
#' @slot get_tibble getter for tibble with column names, headers, latex headers, and formatters
#' @slot get_abbreviations getter for the abbreviations defined by the column formats, as a character vector of definitions named by the term defined
#'
#' @return column_formats
#' @export
#'
#' @examples
#' my_formats <- column_formats()
#' my_formats$Coefficient@formatter <- \(x) round(x, 2)
#' my_formats$Coefficient@formatter(2.214)
column_formats <- new_class(
  name = "column_formats",
  parent = class_list,
  properties = list(
    accuracy = new_property(
      name = "accuracy",
      class = class_double,
      setter = function(self, value) {
        for (cf in S7_data(self)) {
          if (!is.null(formals(cf@formatter)["accuracy"])) {
            cff <- formals(cf@formatter)
            cff$accuracy <- value
            formals(cf@formatter) <- cff
            self[[cf@name]] <- cf
          }
        }

        self@accuracy <- value
        self
      }
    ),
    intercept_text = class_character,
    starred_columns = class_character,
    variable_labels = class_character,
    get_column_names = new_property(getter = \(self) {
      names(S7_data(self))
    }),
    get_headers = new_property(getter = \(self) {
      purrr::map_chr(S7_data(self), "header")
    }),
    get_latex = new_property(getter = \(self) {
      purrr::map_chr(
        S7_data(self),
        \(ap) ifelse(is.null(ap@latex), ap@name, ap@latex)
      )
    }),
    get_formatters = new_property(getter = \(self) {
      purrr::map(S7_data(self), "formatter")
    }),
    get_header_rename = new_property(getter = \(self) {
      v <- self@get_column_names
      names(v) <- self@get_headers
      v
    }),
    get_header_rename_latex = new_property(getter = \(self) {
      v <- self@get_column_names
      names(v) <- self@get_latex
      v
    }),
    get_tibble = new_property(getter = \(self) {
      tibble::tibble(
        name = self@get_column_names,
        header = self@get_headers,
        latex = self@get_latex,
        formatter = self@get_formatters
      )
    }),
    get_abbreviations = new_property(getter = \(self) {
      column_abbreviations(names(S7_data(self)), self)
    })
  ),
  constructor = function(
    .data = NULL,
    accuracy = NULL,
    intercept_text = NULL,
    starred_columns = character(0),
    variable_labels = character(0),
    custom_columns = NULL
  ) {
    if (is.null(.data)) {
      .data <- the$columns
    }
    if (is.null(accuracy)) {
      accuracy <- the$accuracy
    }
    if (is.null(intercept_text)) {
      intercept_text <- the$intercept_text
    }

    for (cf in .data) {
      if (!is.null(formals(cf@formatter)["accuracy"])) {
        cff <- formals(cf@formatter)
        cff$accuracy <- accuracy
        formals(cf@formatter) <- cff
        .data[[cf@name]] <- cf
      }
    }

    if (!is.null(custom_columns)) {
      if (is.null(names(custom_columns))) {
        stop("custom_columns must be a named list")
      }
      .data[names(custom_columns)] <- custom_columns
    }

    S7::new_object(
      .data,
      accuracy = accuracy,
      intercept_text = intercept_text,
      starred_columns = starred_columns,
      variable_labels = variable_labels
    )
  }
)

method(str, column_formats) <- function(object, ...) {
  cat("<", S7_class(object)@name, ">\n", sep = "")
  cat("\tColumns\n")
  print(object@get_tibble)
  cat("\tProperties\n")
  pr <- prop_names(object)
  pr <- pr[stringr::str_starts(pr, "get_", TRUE)]
  cat(str(object = props(object, pr)))
  invisible(object)
}

method(print, column_formats) <- function(x, ...) {
  cli::cli_h2(S7_class(x)@name)
  print(x@get_tibble)
  invisible(x)
}

method(as_tibble, column_formats) <- function(x, ...) {
  x@get_tibble
}

the$column_formats <- column_formats()

#' Set defaults for apa7 package
#'
#' Sets options that apply to all apa7 tables. Settings that are not given
#' are left as they are.
#'
#' Each setting except `column_formats`, `number_formatter`, and
#' `trim_leading_zero` is stored as an R option (e.g.,
#' `apa7_defaults(page_width = "a4")` sets `options(apa7.page_width = "a4")`),
#' so it can also be set with [options()], including in an `.Rprofile` file
#' that runs at the start of every R session, before apa7 is loaded:
#'
#' | Argument | Option | Default |
#' |:---|:---|:---|
#' | `accuracy` | `apa7.accuracy` | `.01` |
#' | `font_family` | `apa7.font_family` | `"Times New Roman"` |
#' | `font_size` | `apa7.font_size` | `12` |
#' | `line_spacing` | `apa7.line_spacing` | `2` |
#' | `intercept_text` | `apa7.intercept_text` | `"Constant"` |
#' | `width_aware_padding` | `apa7.width_aware_padding` | `TRUE` |
#' | `page_width` | `apa7.page_width` | `6.5` |
#' | `landscape_width` | `apa7.landscape_width` | `NA` (from `page_width`) |
#' | `column_width` | `apa7.column_width` | `NA` (from `page_width`) |
#' | `table_width` | `apa7.table_width` | `1` |
#' | `print` | `apa7.print` | `"auto"` |
#' | `citations` | `apa7.citations` | `"auto"` |
#'
#' `column_formats`, `number_formatter`, and `trim_leading_zero` are objects
#' that need apa7 to be loaded, so they are set only with `apa7_defaults()`
#' and last for the R session.
#'
#' When `page_width` and `table_width` are not set, a Quarto or R Markdown
#' document being rendered supplies them: its `papersize` sets the paper, and
#' apaquarto's `documentmode` sets the margins (0.75 inches on each side in
#' journal mode, `jou`, and 1.5 inches on the left in thesis mode). In
#' journal mode, which sets the text in two columns, tables are as wide as a
#' column by default. A setting made with `apa7_defaults()` or [options()]
#' overrides the document.
#'
#' The default column formats follow `accuracy`: changing it (with
#' `apa7_defaults()` or the `apa7.accuracy` option) changes the number of
#' decimals in tables. Setting `column_formats` without `accuracy` makes the
#' formats' own accuracy the `accuracy` setting.
#'
#' @param accuracy rounding accuracy of numbers in tables (default: .01, for
#'   two decimals). Model tables such as [apa_parameters()] and [apa_anova()]
#'   use it. Descriptive tables such as [apa_descriptives()], [apa_cor()], and
#'   [apa_loadings()] have their own `digits` argument instead (`digits = 2`
#'   is the same as `accuracy = .01`).
#' @param font_family font family
#' @param font_size font size of tables and their notes, in points (default: 12)
#' @param line_spacing spacing between the lines of tables' headers and
#'   bodies: 2 (default) for double spacing, 1.5, or 1 for single spacing.
#'   APA style allows any of these in tables. Table notes are always
#'   single-spaced.
#' @param intercept_text label of the intercept row in regression tables (default: `"Constant"`)
#' @param column_formats a [column_formats()] object: the headers, formatters, and note definitions of table columns
#' @param number_formatter function that formats numbers that can exceed 1 in absolute value (e.g., coefficients, standard errors, and test statistics). It takes a numeric vector and `accuracy` and returns a character vector. By default, [align_chr()] with the `accuracy` setting.
#' @param trim_leading_zero function that formats numbers that cannot exceed 1 in absolute value (e.g., correlations, standardized coefficients, and *R*^2^), which APA style writes without a leading zero. By default, [align_chr()] with `trim_leading_zeros = TRUE`. Note that this is a function, unlike the `TRUE`/`FALSE` `trim_leading_zeros` argument of [align_chr()].
#' @param width_aware_padding if `TRUE` (default), [align_chr()] pads text to equal rendered width in `font_family` (when the font is installed). If `FALSE`, text is padded with one figure space per missing character.
#' @param page_width width of the text area of a portrait page, which tables
#'   fill with `table_width = "page"` (or `1`, by default): a number (in
#'   inches) or the name of a paper size, whose width less APA's 1-inch
#'   margins on each side is used (e.g., `"us-letter"` gives 6.5 inches, the
#'   default, and `"a4"` gives 6.27 inches). If not set, the width comes
#'   from the paper and margins of the document being rendered, if any (see
#'   Details). Paper sizes are those of Typst's
#'   `page` function (e.g., `"a4"`, `"a5"`, `"us-letter"`, `"us-legal"`,
#'   `"jis-b5"`); see <https://typst.app/docs/reference/layout/page/#parameters-paper>.
#' @param landscape_width width of the text area of a landscape page, in
#'   inches, which tables fill with `table_width = "landscape"`. If `NA`
#'   (default), the height of the paper named by `page_width` less 1-inch
#'   margins (9 inches for `"us-letter"` and 9.69 inches for `"a4"`). When
#'   `page_width` is a number, the paper is taken to have the proportions of
#'   letter-size paper.
#' @param column_width width of one column of a two-column page, in inches,
#'   which tables fill with `table_width = "column"`. If `NA` (default),
#'   the width of a column in apaquarto's two-column journal mode
#'   (`documentmode: jou`): the paper width less 0.75-inch margins, less a
#'   gutter of 4% between the columns, divided by 2 (3.36 inches for
#'   `"us-letter"` and 3.25 inches for `"a4"`). When `page_width` is a
#'   number, the paper is taken to be that number plus 1-inch margins.
#' @param table_width default width of tables, in any of the forms that the
#'   `table_width` argument of [apa_flextable()] takes: `1` (default) for the
#'   full width of a portrait page, a proportion of it (e.g., `.8`),
#'   `"page"`, `"landscape"`, `"column"`, or a named number (e.g.,
#'   `c(column = 1)`). In a two-column document, `table_width = "column"`
#'   makes tables as wide as a column, and a proportion given to a table
#'   function is then a proportion of the column. If not set, tables in a
#'   document in apaquarto's journal mode (`documentmode: jou`) are as wide as
#'   a column.
#' @param print how tables made by apa7 functions (see [apa7_tbl]) are
#'   displayed: `"auto"` (default) as tables in Quarto and R Markdown
#'   documents and as tibbles in the console, `"flextable"` as tables
#'   everywhere, or `"tibble"` as tibbles everywhere
#' @param citations how articles cited in table notes are written: `"auto"`
#'   (default), `"keys"`, or `"text"`. See [apa7_write_bib()].
#' @param reset if `TRUE`, reset all defaults (except as specified)
#'
#' @return the previous values of the settings that were changed (all of
#'   them with `reset = TRUE`), invisibly, as a list that restores them when
#'   passed back to `apa7_defaults()` (e.g., with [do.call()])
#' @export
#'
#' @examples
#' old <- apa7_defaults(accuracy = .001)
#'
#' # Tables as wide as the text on A4 paper with 1-inch margins
#' apa7_defaults(page_width = "a4")
#'
#' # A two-column document: tables are as wide as a column unless a table
#' # asks for another width (e.g., table_width = "page" in apa_flextable())
#' apa7_defaults(table_width = "column")
#'
#' # Restore the earlier accuracy
#' do.call(apa7_defaults, old)
#'
#' # Reset to package defaults
#' apa7_defaults(reset = TRUE)
apa7_defaults <- function(
  accuracy = NULL,
  font_family = NULL,
  font_size = NULL,
  line_spacing = NULL,
  intercept_text = NULL,
  column_formats = NULL,
  number_formatter = NULL,
  trim_leading_zero = NULL,
  width_aware_padding = NULL,
  page_width = NULL,
  landscape_width = NULL,
  column_width = NULL,
  table_width = NULL,
  print = NULL,
  citations = NULL,
  reset = FALSE
) {
  previous <- apa7_current_defaults()

  # Check values before changing anything
  if (!is.null(accuracy) && !(is.numeric(accuracy) && length(accuracy) == 1 && !is.na(accuracy) && accuracy > 0)) {
    cli::cli_abort("{.arg accuracy} must be a single positive number (e.g., {.code .01}).")
  }
  if (!is.null(font_size) && !(is.numeric(font_size) && length(font_size) == 1 && !is.na(font_size) && font_size > 0)) {
    cli::cli_abort("{.arg font_size} must be a single positive number (points).")
  }
  if (!is.null(line_spacing) && !(is.numeric(line_spacing) && length(line_spacing) == 1 && !is.na(line_spacing) && line_spacing >= 1)) {
    cli::cli_abort("{.arg line_spacing} must be a single number of at least 1 (e.g., 1 for single spacing or 2 for double spacing).")
  }
  for (arg in c("font_family", "intercept_text")) {
    value <- get(arg)
    if (!is.null(value) && !(is.character(value) && length(value) == 1 && !is.na(value))) {
      cli::cli_abort("{.arg {arg}} must be a single character string.")
    }
  }
  if (!is.null(width_aware_padding) && !(isTRUE(width_aware_padding) || isFALSE(width_aware_padding))) {
    cli::cli_abort("{.arg width_aware_padding} must be {.code TRUE} or {.code FALSE}.")
  }
  if (!is.null(page_width)) {
    page_text_width(page_width)
  }
  for (arg in c("landscape_width", "column_width")) {
    value <- get(arg)
    if (!is.null(value) && !(length(value) == 1 && (is.na(value) || (is.numeric(value) && value > 0)))) {
      cli::cli_abort("{.arg {arg}} must be a single positive number (inches), or {.code NA} to compute it from {.arg page_width}.")
    }
  }
  if (!is.null(table_width)) {
    resolve_table_width(table_width)
  }
  if (!is.null(print) && !(is.character(print) && length(print) == 1 && print %in% c("auto", "flextable", "tibble"))) {
    cli::cli_abort("{.arg print} must be {.val auto}, {.val flextable}, or {.val tibble}.")
  }
  if (!is.null(citations)) {
    citations_mode(citations)
  }

  if (reset) {
    options(stats::setNames(
      rep(list(NULL), length(apa7_option_settings)),
      purrr::map_chr(apa7_option_settings, "option")
    ))
    the$column_formats <- column_formats()
    the$number_formatter <- default_number_formatter
    the$trim_leading_zero <- default_trim_leading_zero
    the$pvalue_formatter <- default_pvalue_formatter
  }

  settings <- list(
    accuracy = accuracy,
    font_family = font_family,
    font_size = font_size,
    line_spacing = line_spacing,
    intercept_text = intercept_text,
    width_aware_padding = width_aware_padding,
    page_width = page_width,
    landscape_width = landscape_width,
    column_width = column_width,
    table_width = table_width,
    print = print,
    citations = citations
  )
  for (name in names(settings)) {
    if (!is.null(settings[[name]])) {
      options(stats::setNames(list(settings[[name]]), apa7_option_settings[[name]]$option))
    }
  }
  if (!is.null(column_formats)) {
    # The formats' accuracy becomes the accuracy setting, unless accuracy is
    # also given (then the formats follow it)
    if (is.null(accuracy) && length(column_formats@accuracy) == 1) {
      options(apa7.accuracy = column_formats@accuracy)
    }
    the$column_formats <- column_formats
  }
  if (!is.null(number_formatter)) {
    the$number_formatter <- number_formatter
  }
  if (!is.null(trim_leading_zero)) {
    the$trim_leading_zero <- trim_leading_zero
  }

  given <- names(previous)[!vapply(
    list(
      accuracy = accuracy, font_family = font_family, font_size = font_size,
      line_spacing = line_spacing, intercept_text = intercept_text,
      column_formats = column_formats, number_formatter = number_formatter,
      trim_leading_zero = trim_leading_zero, width_aware_padding = width_aware_padding,
      page_width = page_width, landscape_width = landscape_width,
      column_width = column_width, table_width = table_width,
      print = print, citations = citations
    )[names(previous)],
    is.null,
    logical(1)
  )]
  changed <- if (reset) names(previous) else given
  invisible(previous[changed])
}

#' Settings stored as R options
#'
#' Each has its option name, its default, and (for `page_width`) a function
#' that turns the stored value into what the package uses.
#' @keywords internal
#' @noRd
apa7_option_settings <- list(
  accuracy = list(option = "apa7.accuracy", default = .01),
  font_family = list(option = "apa7.font_family", default = "Times New Roman"),
  font_size = list(option = "apa7.font_size", default = 12),
  line_spacing = list(option = "apa7.line_spacing", default = 2),
  intercept_text = list(option = "apa7.intercept_text", default = "Constant"),
  width_aware_padding = list(option = "apa7.width_aware_padding", default = TRUE),
  page_width = list(option = "apa7.page_width", default = 6.5, read = function(x) page_text_width(x)),
  landscape_width = list(option = "apa7.landscape_width", default = NA),
  column_width = list(option = "apa7.column_width", default = NA),
  table_width = list(option = "apa7.table_width", default = 1),
  print = list(option = "apa7.print", default = "auto"),
  citations = list(option = "apa7.citations", default = "auto")
)

#' Current settings, as apa7_defaults() takes them
#' @keywords internal
#' @noRd
apa7_current_defaults <- function() {
  options_now <- lapply(apa7_option_settings, \(s) getOption(s$option, s$default))
  c(
    options_now[c("accuracy", "font_family", "font_size", "line_spacing", "intercept_text")],
    list(
      column_formats = the$column_formats,
      number_formatter = the$number_formatter,
      trim_leading_zero = the$trim_leading_zero
    ),
    options_now[c("width_aware_padding", "page_width", "landscape_width", "column_width", "table_width", "print", "citations")]
  )
}

#' Make the settings in `the` read and write their R options
#'
#' Called when the package is loaded, so that `the$accuracy` (and the other
#' settings stored as options) always gives the current option.
#' @keywords internal
#' @noRd
bind_option_settings <- function(env = the) {
  for (name in c("accuracy", "font_family", "font_size", "line_spacing", "intercept_text", "width_aware_padding", "page_width")) {
    setting <- apa7_option_settings[[name]]
    if (exists(name, envir = env, inherits = FALSE)) {
      rm(list = name, envir = env)
    }
    makeActiveBinding(
      name,
      local({
        setting <- setting
        function(value) {
          if (missing(value)) {
            stored <- getOption(setting$option)
            if (is.null(stored)) {
              return(setting$default)
            }
            if (is.null(setting$read)) stored else setting$read(stored)
          } else {
            options(stats::setNames(list(value), setting$option))
          }
        }
      }),
      env
    )
  }

  # The default column formats follow the accuracy setting: when it changes,
  # the formats' accuracy (which is written into each formatter) is updated
  if (exists("column_formats", envir = env, inherits = FALSE)) {
    stored <- get("column_formats", envir = env)
    rm(list = "column_formats", envir = env)
  } else {
    stored <- column_formats() # nocov
  }
  env$.column_formats <- stored
  makeActiveBinding(
    "column_formats",
    function(value) {
      if (missing(value)) {
        cf <- env$.column_formats
        accuracy <- env$accuracy
        if (!isTRUE(all.equal(cf@accuracy, accuracy))) {
          cf@accuracy <- accuracy
          env$.column_formats <- cf
        }
        cf
      } else {
        env$.column_formats <- value
      }
    },
    env
  )
  invisible(env)
}

#' Format data columns
#'
#' `apa_format_columns()` formats the columns of a data frame of results and
#' renames their headers in APA style, using a set of [column_formats()].
#' [apa_flextable()] calls it when `auto_format_columns = TRUE`.
#'
#' @details
#' Each column is matched by name to a [column_format()] in `column_formats`
#' (e.g., `p`, `SE`, `beta`, or `R2`). The values of a matched column are
#' formatted by the format's formatter, and its name is replaced by the
#' format's markdown header (e.g., `beta` becomes `*&beta;*`), or by its LaTeX
#' header if `latex_headers = TRUE`. Columns without a format, and columns
#' named in `no_format_columns`, are left as they are.
#'
#' If `format_separated_headers = TRUE` (default), a column name with a
#' separator (`sep`, an underscore by default) is matched by its last part, so
#' `Model 1_R2` is formatted like `R2` and becomes `Model 1_*R*^2^`. In
#' [apa_flextable()], the part before the separator becomes a column spanner.
#' Format names that contain an underscore themselves (e.g., `R2_adjusted`) are
#' matched whole.
#'
#' A few columns are matched by a pattern or combined:
#'
#' * `CI`, `CI_low`, and `CI_high` (e.g., from [parameters::parameters()])
#'   become a single column of intervals, with a header such as "95% CI" that
#'   gives the confidence level stored in `CI`.
#' * If there are `t` and `df_error` columns, the degrees of freedom are moved
#'   into the `t` header (e.g., *t*(28)), and `df_error` is dropped.
#' * A column named like `t(28)` is formatted with the `t_df` format (and
#'   gets the header *t*(28)), and one named like `90% CI` with the
#'   `CI_percent` format.
#'
#' To see the default formats, run `column_formats()@get_tibble`. The article
#' [Building Tables by Hand](https://wjschne.github.io/apa7/articles/summarize.html)
#' shows how to change them.
#'
#' @param data data set (data.frame or tibble)
#' @param column_formats `column_formats` object. If NULL, the current default formatter set with [apa7_defaults()] will be used.
#' @param no_format_columns Column name or tidyselect function. selected columns are not formatted
#' @param rename_headers if `TRUE`, rename headers with markdown or latex
#' @param latex_headers if `TRUE`, rename headers with latex instead of markdown
#' @param format_separated_headers if `TRUE`, format headers with separated names. For example, if the formatter formats column `R2` as `*R*^2^`, then `Model 1_R2` becomes `Model 1_*R*^2^`)
#' @param sep separator for separated headers (default is "_")
#' @param accuracy numeric (default: NULL, uses the current default accuracy set with [apa7_defaults()]). If not NULL, sets the accuracy for the formatter.
#'
#' @return tibble
#' @export
#'
#' @examples
#' lm(mpg ~ cyl + wt, data = mtcars) |>
#'   parameters::parameters() |>
#'   apa_format_columns() |>
#'   apa_flextable()
apa_format_columns <- function(
  data,
  column_formats = NULL,
  no_format_columns = NULL,
  rename_headers = TRUE,
  latex_headers = FALSE,
  format_separated_headers = TRUE,
  sep = "_",
  accuracy = NULL
) {
  CI_low <- CI_high <- df_error <- name <- header <- column <- group <- value <- pattern_1 <- pattern_2 <- replace_1 <- replace_2 <- pattern <- type <- should_replace <- id <- replacer <- formatter <- name1 <- latex1 <- latex <- NULL

  no_format_columns <- data |>
    dplyr::select(dplyr::all_of(no_format_columns)) |>
    colnames()

  if (is.null(column_formats)) {
    cf <- the$column_formats
  } else {
    cf <- column_formats
  }

  if (!is.null(accuracy)) {
    cf@accuracy <- accuracy
  }

  # Make sure data is a tibble
  data <- tibble::as_tibble(data)

  # Get names
  data_names <- colnames(data)
  format_names <- names(cf)

  if (any(stringr::str_detect(data_names, "t\\((\\d+)\\)$"))) {
    t_df <- data_names[stringr::str_detect(data_names, "t\\((\\d+)\\)$")]

    new_col <- column_format(
      t_df,
      header = stringr::str_replace(t_df, "t", "*t*"),
      latex = stringr::str_replace(t_df, "t", "$t$"),
      formatter = cf[["t_df"]]@formatter
    )

    cf[[t_df]] <- new_col
  }

  if (any(stringr::str_detect(data_names, "(\\d+)\\% CI$"))) {
    pci <- data_names[stringr::str_detect(data_names, "(\\d+)\\% CI$")]

    if (is.null(cf$CI_percent)) {
      cf$CI_percent <- the$column_formats$CI_percent
    }

    new_col <- column_format(
      pci,
      header = pci,
      latex = pci |> stringr::str_replace("\\%", "\\%"),
      formatter = cf$CI_percent@formatter
    )

    cf[[pci]] <- new_col
  }

  # Treat underscores in column_format header names differently
  temp_fix_names <- stringr::str_replace_all(
    string = format_names,
    pattern = "_",
    replacement = "apa7separator"
  ) |>
    `names<-`(format_names)

  d_formatter <- cf@get_tibble

  d_names <- tibble::tibble(column = data_names, name = data_names)

  if (format_separated_headers) {
    d_names <- d_names |>
      dplyr::mutate(
        name = stringr::str_replace_all(name, temp_fix_names) |>
          stringr::str_split_i(pattern = sep, i = -1) |>
          stringr::str_replace_all("apa7separator", "_")
      )
  }

  d_names <- dplyr::left_join(
    d_names,
    d_formatter,
    by = dplyr::join_by(name)
  ) |>
    dplyr::filter(!is.na(header))

  cls <- d_names |> dplyr::pull(column)

  if (length(no_format_columns) > 0) {
    cls <- cls[!(cls %in% no_format_columns)]
  }

  ci <- .95

  if (
    all(c("CI", "CI_low", "CI_high") %in% d_names$name) &&
      !is.null(cf$CI) &
      ("CI" %in% cls)
  ) {
    d_ci <- d_names |>
      dplyr::mutate(
        group = stringr::str_remove(column, paste0(sep, name, "$"))
      ) |>
      dplyr::filter(name == "CI") |>
      dplyr::mutate(
        CI_low = paste0(group, sep, "low"),
        CI_high = paste0(group, sep, "high")
      )

    for (i in seq_len(nrow(d_ci))) {
      ci_names <- d_ci[i, c("column", "CI_low", "CI_high")] |>
        tidyr::pivot_longer(dplyr::everything()) |>
        dplyr::pull(value)

      ci_name <- ci_names[1]

      if (all(ci_names %in% colnames(data))) {
        ci <- max(data[, ci_names[1]], na.rm = TRUE)

        d_names[d_names$column == d_ci[[i, "column"]], "header"] <- glue::glue(
          cf$CI@header
        ) |>
          as.character()

        data[, ci_names[1]] <- cf$CI@formatter(
          dplyr::pull(data, dplyr::any_of(ci_names[2])),
          dplyr::pull(data, dplyr::any_of(ci_names[3])),
          accuracy = cf@accuracy
        )
        data[, ci_names[2]] <- NULL
        data[, ci_names[3]] <- NULL
        cls <- cls[cls != ci_names[1]]
      }
    }
  }

  if (
    all(c("t", "df_error") %in% data_names) &&
      ("t" %in% cls)
  ) {
    df <- paste0(
      "(",
      align_chr(max(data$df_error, na.rm = TRUE), accuracy = the$accuracy),
      ")"
    )

    cf$t@header <- paste0(cf$t@header, df)

    if (stringr::str_detect(cf$t@latex, "\\$")) {
      cf$t@latex <- stringr::str_replace(cf$t@latex, "\\$$", paste0(df, "$"))
    } else {
      cf$t@latex <- paste0(cf$t@latex, df)
    }

    data <- dplyr::select(data, -df_error)

    cls <- cls[cls != "df_error"]
  }

  for (cl in cls) {
    f1 <- d_names |>
      dplyr::filter(column == cl) |>
      dplyr::pull(formatter)

    if (!is.null(f1)) {
      if (cl %in% colnames(data)) {
        data[[cl]] <- f1[[1]](data[[cl]])
      }
    }
  }

  if (rename_headers) {
    if (latex_headers) {
      rcls <- cf@get_latex
    } else {
      rcls <- cf@get_headers
    }

    if (nrow(d_names) > 0) {
      rcls <- d_names |>
        dplyr::mutate(
          name1 = stringr::str_remove_all(
            name,
            c(`\\(` = "\\\\(", `\\)` = "\\\\)")
          ),
          latex1 = stringr::str_replace_all(
            latex,
            pattern = "\\\\$",
            replacement = "\\\\$"
          )
        ) |>
        dplyr::mutate(
          pattern_1 = paste0("^", name1, "$"),
          replace_1 = dplyr::if_else(
            rep(latex_headers, dplyr::n()),
            latex1,
            header
          ),
          pattern_2 = paste0("_", name1, "$"),
          replace_2 = paste0(
            "_",
            dplyr::if_else(rep(latex_headers, dplyr::n()), latex1, header)
          )
        ) |>
        dplyr::select(-name1, -latex1) |>
        dplyr::select(column, pattern_1, pattern_2, replace_1, replace_2) |>
        tidyr::pivot_longer(-column) |>
        tidyr::separate(name, c("name", "type")) |>
        tidyr::pivot_wider() |>
        dplyr::mutate(
          should_replace = stringr::str_detect(
            column,
            pattern
          )
        ) |>
        dplyr::select(-type) |>
        dplyr::filter(should_replace) |>
        dplyr::mutate(id = dplyr::row_number(), .by = column) |>
        dplyr::filter(id == 1) |>
        dplyr::mutate(
          replacer = stringr::str_replace(
            column,
            pattern,
            replace
          )
        ) |>
        dplyr::select(replacer, column) |>
        tibble::deframe()

      data <- data |>
        dplyr::rename(dplyr::any_of(rcls))
    }
  }
  data
}

#' Prefix text with figure spaces to balance star text
#'
#' @param x character vector
#' @param star star text
#' @param superscript Place superscript text if `TRUE`
#'
#' @return character vector
#' @export
#'
#' @examples
#' # Starred values get one figure space per star on the left,
#' # so that the numbers line up when the column is centered
#' star_balance(c(".32", ".04\\^\\*\\^", ".005\\^\\*\\*\\^"))
star_balance <- function(x, star = "\\*", superscript = TRUE) {
  k <- stringr::str_count(x, star)
  ss <- strrep(ifelse(superscript, "^", ""), (k > 0) * 1)
  ss <- paste0(ss, strrep("&numsp;", k), ss, x)
  ss[is.na(x)] <- NA
  ss
}

the$summary_functions <- list(
  IQR = IQR,
  `Interquartile Range` = IQR,
  Kurtosis = psych::kurtosi,
  MAD = mad,
  `Median Absolute Deviation` = mad,
  M = mean,
  Mean = mean,
  Med = median,
  Median = median,
  n = function(x) sum(!is.na(x)),
  N = function(x) sum(!is.na(x)),
  Quantile = quantile,
  Range = range,
  SD = sd,
  Skewness = psych::skew,
  Var = var,
  Variance = var
)

#' Prepend column spanner labels to data column labels
#'
#' @param data data.frame or tibble
#' @param label character of column spanner
#' @param ... columns (i.e., one or more tidyselect functions and/or a vector of quoted or unquoted variable names)
#' @param relocate relocate columns with same spanner label to be adjacent
#'
#' @return data.frame or tibble
#' @export
#'
#' @examples
#' d <- data.frame(y = 1:3, x1 = 2:4, x2 = 3:5)
#'
#' # Unquoted variable names
#' column_spanner_label(d, "Label", c(x1, x2))
#' # Character values (quoted variable names)
#' column_spanner_label(d, "Label", c("x1", "x2"))
#' # Tidyselect function (e.g., starts_with, ends_with, contains)
#' column_spanner_label(d, "Label", dplyr::starts_with("x"))
#' # Tidyselect range
#' column_spanner_label(d, "Label", x1:x2)
#' # Selected variables are relocated after the first selected variable
#' column_spanner_label(d, "Label", c(x2, y))
column_spanner_label <- function(data, label, ..., relocate = TRUE) {
  cnames <- dplyr::select(data, ...) |> colnames()
  if (length(cnames) > 0) {
    if (relocate) {
      data <- dplyr::relocate(
        .data = data,
        dplyr::any_of(cnames),
        .after = cnames[1]
      )
    }
    dplyr::rename_with(
      .data = data,
      .fn = \(x) paste0(label, "_", x),
      .cols = dplyr::any_of(cnames)
    )
  } else {
    data
  }
}
