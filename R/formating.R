#' Align text on center text (default is decimal)
#'
#' @param x vector (numeric or character)
#' @param accuracy number to round to. If NULL, the current default accuracy set with [apa7_defaults()] will be used.
#' @param trim_leading_zeros if TRUE (default), trims leading zeros, otherwise keeps them
#' @param add_plusses if TRUE (default), adds a plus to positive numbers
#' @param padding_character character to use for padding, default is `&numsp;` (figure space)
#' @param center text on which to align text. Center on decimal by default, but can be any text.
#' @param format_integers If TRUE, integers will be formatted with digits
#' @param side side on which to make text of equal width
#' @param NA_value value to replace NA
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
    add_plusses = FALSE,
    padding_character = "\u2007", # figure space
    center = ".",
    format_integers = FALSE,
    side = c("both", "left", "right"),
    NA_value = "",
    ...) {


  center1 <- center
  if (center1 == ".") center1 <- "\\."


  if (is.null(accuracy)) accuracy <- the$accuracy
  xx <- x

  if (is.character(x)) {
    xx <- stringr::str_trim(xx)
  }

  side <- match.arg(side, c("both", "left", "right"))

  if (rlang::is_integerish(x) && !format_integers) {
    xx  <- num_pad(signs::signs(
      xx, add_plusses = add_plusses,
      big.mark = ","),
      padding_character = padding_character)

  } else {
    if (is.numeric(x)) {
      xx <- signs::signs(
        xx,
        accuracy = accuracy,
        add_plusses = add_plusses,
        big.mark = ",",
        trim_leading_zeros = trim_leading_zeros,
        ...)
    }

    # Make split on only on the first instance of center
    splitter <- "apa7replacecharacter"
    xx <- stringr::str_replace(xx, center1, splitter)

    left_x <- stringr::str_split_i(
      xx,
      pattern = splitter,
      i = 1)

    right_x <- stringr::str_split_i(
      xx,
      pattern = splitter,
      i = 2)

    if (side != "right") {
      left_x <- num_pad(left_x,
                        padding_character = padding_character,
                        NA_value = "")
    }

    if (side != "left") {
      right_x <- num_pad(right_x,
                         pad_left = FALSE,
                         padding_character = padding_character,
                         NA_value = "")
    }

    middle <- paste0(center,
                     rep("", length(xx)))

    middle[!stringr::str_detect(xx, splitter)] <- ""

    middle[is.na(xx)] <- ""

    xx <- paste0(left_x,
                 middle,
                 right_x)
  }
  xx[is.na(x) | x == length(x) | x == ""] <- NA_value


  xx
}

#' p-value in APA format
#'
#' @param p probability
#' @param inline If TRUE (default), returns statistic (e.g.,e p = .04), otherwise just the number (e.g., .04)
#' @param markdown By default, outputs text compatible with markdown if TRUE, otherwise prints plain text compatible with latex.
#' @param min_digits minimum number of digits to round to. Default is 2.
#' @param max_digits maximum number of digits to round to. Default is 3.
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
#' apa_p(.02)#'
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
apa_p <- function(p,
                  inline = FALSE,
                  markdown = TRUE,
                  min_digits = 2,
                  max_digits = 3) {
  min_p <- 10 ^ (-max_digits)
  min_p_chr <- signs::signs(
    min_p,
    trim_leading_zeros = TRUE,
    accuracy = min_p)

  if (is.character(p)) {
    p <- stringr::str_trim(p)
    less_than <- stringr::str_detect(p, "<")
    p <- stringr::str_remove(p, "^<") |> as.numeric()
    p[less_than] <- 10 ^ (-max_digits - 1)
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

  max_p <- 10 ^ (-min_digits)
  max_p_chr <- signs::signs(1 - max_p,
                            trim_leading_zeros = TRUE,
                            accuracy = max_p)
  p_threshold <- max_p - max_p * .05
  max_p_threshold <- 1 - max_p * .5

  digit <- ifelse(p < p_threshold, max_digits, min_digits)
  p_formatted <- purrr::map2_chr(p, digit, function(pp, dd) {
    if (is.na(pp)) return(NA)
    formatC(round(pp, dd + 1), digits = dd, format = "f")}
    )

  p_formatted <- sub(x = p_formatted ,
                     pattern = "^0\\.",
                     replacement = ".")

  gt_symbol <- ifelse(markdown, "\\>", ">")
  p_symbol <- ifelse(markdown, "*p*", "p")

  operator <- ifelse(
    p < min_p,
    "<",
    ifelse(p >= max_p_threshold,
           gt_symbol,
           "="))
  sep <- ifelse(markdown, "\u00A0", " ")



  prefix <- paste0(p_symbol, sep,operator, sep)
  if (inline) {
    p_formatted[p < min_p] <- min_p_chr
    p_formatted[p >= max_p_threshold] <- max_p_chr
  } else {
    p_formatted[p < min_p] <- paste0("<", min_p_chr)
    p_formatted[p >= max_p_threshold] <- paste0(gt_symbol, max_p_chr)
  }

  pv <- paste0(ifelse(inline, prefix, ""), p_formatted)
  pv[is.na(p)] <- NA
  pv
}


#' Make star notes for p-values
#'
#' @param x vector of alpha values (p-value thresholds)
#' @inheritParams p2stars
#'
#' @return character vector
#' @export
#'
#' @examples
#' apa_p_star_note()
#' apa_p_star_note(x = c(.10, .05, .01, .001), first_alpha_marginal = TRUE)
apa_p_star_note <- function(x = c(.05, .01, .001), first_alpha_marginal = FALSE) {
  paste0(
    p2stars(x, alpha = x, superscript = TRUE, first_alpha_marginal = first_alpha_marginal, add_trailing_space = TRUE),
    "*p*\u00A0<\u00A0",
    signs::signs(x,
                 trim_leading_zeros = TRUE,
                 drop0trailing = TRUE),
    collapse = ". "
  )
}

#' Return markdown text with hanging indent
#'
#' @param x text
#' @param indent number of spaces to indent
#' @param width number of characters to break lines
#' @param space indenting space character
#' @param newline text for creating new line
#' @param whitespace_only wrapping spaces only
#'
#' @return character vector
#' @export
#'
#' @examples
#' hanging_indent("Hello Darkness, my old friend. I've come to talk with you again.")
hanging_indent <- function(
    x,
    indent = 4,
    width = 30,
    space = "\u00A0",
    newline = "\\\\\n",
    whitespace_only = FALSE) {
  indent_chr <- paste0(rep(space, indent), collapse = "")

  stringr::str_replace_all(x, "\u2007", "\u2057") |>
    stringr::str_wrap(width = width,
                      whitespace_only = whitespace_only) |>
    stringr::str_replace_all("\n", paste0(newline, indent_chr)) |>
    stringr::str_replace_all("\u2057", "\u2007")
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
#' @param x vector of text
#' @param pad_left if TRUE (default), pads on the left, otherwise pads on the right
#' @param padding_character character to use for padding, default is `&numsp;` (figure space)
#' @param NA_value value to replace NA
#'
#' @return character vector
#' @export
#'
#' @examples
#' num_pad(c("a", "bb"))
num_pad <- function(x,
                    pad_left = TRUE,
                    padding_character = "&numsp;",
                    NA_value = "") {
  nch <- nchar(x)
  gtcount <- stringr::str_count(x, "\\>")
  gtcount[is.na(gtcount)] <- 0
  starcount <- stringr::str_count(x, "\\*")
  starcount[is.na(starcount)] <- 0
  nch <- nch - gtcount - starcount
  nch[is.na(nch)] <- 0

  x[is.na(x)] <- NA_value

  max_nch <- max(nch)
  pad_nch <- max_nch - nch
  pad_chr <- purrr::map_chr(pad_nch, \(t) {
    paste0(rep(padding_character, t), collapse = "")
  })
  if (pad_left) {
    paste0(pad_chr, x)
  } else {
    paste0(x, pad_chr)
  }
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
p2stars <- function(p,
                    alpha = c(0.05, .01, .001),
                    first_alpha_marginal = FALSE,
                    superscript = FALSE,
                    add_trailing_space = FALSE,
                    prefix = "\\") {
  pstars <- purrr::map_chr(p, \(pp) {
    if (is.na(pp)) return("")
    paste0(rep(paste0(prefix, "*"),
               sum((pp <= alpha) * 1L)),
           collapse = "")
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
      "^")
  } else {
    pstars[nchar(pstars) > 0] <- paste0(
      pstars[nchar(pstars) > 0],
      trailing_space)
  }
  pstars
}
p2stars(.10, alpha = c(.10, .05, .01, .001), first_alpha_marginal = F)


#' Surrounds text with tags unless empty
#'
#' @param x character vector
#' @param tag opening tag, e.g., `<span>`
#' @param right_tag closing tag, e.g., `</span>`. Defaults to the same value as the opening tag.
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
#' This class is used to define the format of columns in tables, including the name, header, latex representation, and a formatter function.
#' @param name name of column
#' @param header markdown representation of header name
#' @param latex latex representation of header name
#' @param formatter function that formats the column values. It should take a vector of values and return a character vector of formatted values.
#' @export
#' @return column_format object
#' @examples
#' R2 <- column_format(
#'          "R2",
#'          header = "*R*^2^",
#'          latex = "$R^2$",
#'          formatter = \(x, accuracy = the$accuracy, ...) {
#'                        align_chr(x,
#'                                  accuracy = accuracy,
#'                                  trim_leading_zeros = TRUE,
#'                                  ...)
#'                        })
#' R2
#' R2@header
#' R2@formatter
column_format <- new_class("column_format", properties = list(
  name = class_character,
  header = class_character,
  latex = class_character,
  formatter = class_function
))


as_tibble <- S7::new_external_generic(package = "tibble", name = "as_tibble", dispatch_args = "x")


method(as_tibble, column_format) <- function(x, ...) {
  tibble::tibble(
    name = x@name,
    header = x@header,
    latex = ifelse(is.null(x@latex), NA, x@latex),
    formatter = list(x@formatter))
}

method(str, column_format) <- function(object, ...) {
  cli::cli_h2(S7_class(object)@name)
  print(tibble::as_tibble(object))
  invisible(object)
}

#' @keywords internal
variable_name_formatter <- function(x,
                                    pattern = NULL,
                                    replacement = NULL,
                                    formatter = NULL) {
  x <- stringr::str_replace_all(x, "\\:", " : ") |>
    stringr::str_replace_all("\\^", "_^_") |>
    stringr::str_replace_all("(Intercept)", the$intercept_text) |>
    snakecase::to_title_case(abbreviations = c(":", "\u00D7", "\\^")) |>
    stringr::str_replace_all(" \u00D7 ", "\u00D7") |>
    stringr::str_replace_all("\u00D7", " \u00D7 ") |>
    stringr::str_replace_all("\\^(\\d)", "^\\1^")

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
the$number_formatter <- \(x, accuracy = the$accuracy, ...) {
  align_chr(x, accuracy = accuracy, ...)
}
the$pvalue_formatter <- \(x, accuracy = the$accuracy, ...) {
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
      max_digits = max_digits, ...))
}

the$trim_leading_zero <- \(x, accuracy = the$accuracy, ...) {
  align_chr(x,
            accuracy = accuracy,
            trim_leading_zeros = TRUE,
            ...)}

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
    formatter = the$number_formatter
  ),
  AICc = column_format(
    name = "AICc",
    header = "*AICc*",
    latex = "$AICc$",
    formatter = the$number_formatter
  ),
  B = column_format(
    name = "B",
    header = "*B*",
    latex = "$B$",
    formatter = the$number_formatter),
  b = column_format(
    name = "b",
    header = "*b*",
    latex = "$b$",
    formatter = the$number_formatter),
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
    formatter = the$number_formatter
  ),
  BICc = column_format(
    name = "BICc",
    header = "*BICc*",
    latex = "$BICc$",
    formatter = the$number_formatter
  ),
  Chi2 = column_format(
    name = "Chi2",
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
    formatter = the$trim_leading_zero
  ),
  CI = column_format(
    name = "CI",
    header = "{round(ci * 100)}% CI",
    latex = "{round(ci * 100)}\\%",
    formatter = \(l, u, accuracy = the$accuracy, ...) {
      middle <- rep(", ", length(l))
      middle[is.na(l) | is.na(u)] <- ""
      align_chr(
        paste0(align_chr(l, accuracy = accuracy, ...),
               middle,
               align_chr(u, accuracy = accuracy, ...)
               ), center = ", ") |>
        tagger("[", "]")
    }
  ),
  CI_low = column_format(
    name = "CI_low",
    header = "LL",
    latex = "LL",
    formatter = the$number_formatter
  ),
  CI_high = column_format(
    name = "CI_high",
    header = "UL",
    latex = "UL",
    formatter = the$number_formatter
  ),
  Coefficient = column_format(
    name = "Coefficient",
    header = "*B*",
    latex = "$B$",
    formatter = the$number_formatter),
  cronbach = column_format(
    name = "cronbach",
    header = "Cronbach's &alpha;",
    latex = "Cronbach's $\\alpha$",
    formatter = the$trim_leading_zero
  ),
  deltaR2 = column_format(
    name = "deltaR2",
    header = "&Delta;*R*^2^",
    latex = "$\\Delta R^2$",
    formatter = the$trim_leading_zero
  ),
  deltaAIC = column_format(
    name = "deltaAIC",
    header = "&Delta;*AIC*",
    latex = "$\\Delta AIC$",
    formatter = the$number_formatter
  ),
  deltaBIC = column_format(
    name = "deltaR2",
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
  df_diff = column_format(
    name = "df_diff",
    header = "&Delta;*df*",
    latex = "$\\Delta df$",
    formatter = the$number_formatter
  ),
  eta2 = column_format(
    name = "eta2",
    header = "*&eta;*^2^",
    latex = "$\\eta^2$",
    formatter = the$trim_leading_zero
  ),
    Eta2_partial = column_format(
    name = "Eta2_partial",
    header = "*&eta;*^2^",
    latex = "$\\eta^2$",
    formatter = the$trim_leading_zero
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
    formatter = the$trim_leading_zero
  ),
  omega = column_format(
    name = "omega",
    header = "&omega;",
    latex = "$\\omega$",
    formatter = the$trim_leading_zero
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
    formatter = \(x) stringr::str_replace_all(x, "\\:", " : ") |>
      stringr::str_replace_all("\\^", "_^_") |>
      stringr::str_replace_all("(Intercept)", the$intercept_text) |>
      snakecase::to_title_case(abbreviations = c(":", "\u00D7", "\\^")) |>
      stringr::str_replace_all(" \u00D7 ", "\u00D7") |>
      stringr::str_replace_all("\u00D7", " \u00D7 ") |>
      stringr::str_replace_all("\\^(\\d)", "^\\1^")
  ),
  r = column_format(
    name = "r",
    header = "*r*",
    latex = "$r$",
    formatter = the$trim_leading_zero
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
  RMSE = column_format(
    name = "RMSE",
    header = "*RMSE*",
    latex = "$RMSE$",
    formatter = the$number_formatter
  ),
  RMSEA = column_format(
    name = "RMSEA",
    header = "RMSEA",
    latex = "RMSEA",
    formatter = the$trim_leading_zero
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
    formatter = the$number_formatter
  ),
  Std_Coefficient = column_format(
    name = "Std_Coefficient",
    header = "&beta;",
    latex = "$\\beta$",
    formatter = the$trim_leading_zero),
  t = column_format(
    name = "t",
    header = "*t*",
    latex = "$t$",
    formatter = the$number_formatter
  )
)

# column_formats ----
#' Create a set of column formats
#'
#' Returns an S7 object that contains a list of `column_format` objects that can be used to format parameters in APA style.
#' @param .data list of `column_format` objects
#' @param accuracy numeric (passed to scales::number)
#' @param intercept_text describe intercept
#' @param starred_columns which columns get p-value stars
#' @param variable_labels named vector of variable names (with vector names as labels). For example, c(`Parental Income` = "parental_income", `Number of Siblings` = "n_siblings")
#' @param custom_columns named list of column_formats to add or replace existing columns
#' @slot get_column_names getter for column names
#' @slot get_headers getter for column headers
#' @slot get_latex getter for column latex headers
#' @slot get_formatters getter for column formatters
#' @slot get_header_rename getter for column names with headers as names
#' @slot get_header_rename_latex getter for column names with latex headers as names
#' @slot get_tibble getter for tibble with column names, headers, latex headers, and formatters
#'
#' @return column_formats
#' @export
#'
#' @examples
#' my_formatter <- column_formats()
#' my_formatter$Coefficient@formatter <- \(x) round(x, 2)
#' my_formatter$Coefficient@formatter(2.214)
column_formats <- new_class(
  name = "column_formats",
  parent = class_list,
  properties = list(
    accuracy = new_property(name = "accuracy",
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
    }),
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
      purrr::map_chr(S7_data(self), \(ap) ifelse(is.null(ap@latex), ap@name, ap@latex))
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
    if (is.null(.data)) .data <- the$columns
    if (is.null(accuracy)) accuracy <- the$accuracy
    if (is.null(intercept_text)) intercept_text <- the$intercept_text

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
#' @param accuracy numeric (default: .01)
#' @param font_family font family
#' @param intercept_text what to call the intercept
#' @param column_formats column formatting functions
#' @param number_formatter default function to format numbers
#' @param trim_leading_zero default function to trim leading zeros from numbers
#' @param reset if `TRUE`, reset all defaults (except as specified)
#'
#' @return previous defaults
#' @export
#'
#' @examples
#' apa7_defaults(accuracy = .001)
#' # Reset to package defaults
#' apa7_defaults(reset = TRUE)
apa7_defaults <- function(accuracy = NULL,
                          font_family = NULL,
                          intercept_text = NULL,
                          column_formats = NULL,
                          number_formatter = NULL,
                          trim_leading_zero = NULL,
                          reset = FALSE) {
  old <- the
  if (reset) {
    the$accuracy <- .01
    the$column_formats <- column_formats()
    the$intercept_text <- "Constant"
    the$font_family <- "Times New Roman"
    the$number_formatter <- \(x, accuracy = the$accuracy, ...) align_chr(
      x,
      accuracy = accuracy,
      ...
    )
    the$trim_leading_zero <- \(x, accuracy = the$accuracy, ...) align_chr(
      x,
      accuracy = accuracy,
      trim_leading_zeros = TRUE,
      ...
    )
    the$pvalue_formatter <- \(x, accuracy = the$accuracy, ...) {
      max_digits <- 3
      min_digits <- round(-log10(accuracy))
      if (max_digits < min_digits) {
        max_digits <- min_digits
      }
      align_chr(apa_p(x,
                      min_digits = min_digits,
                      max_digits = max_digits, ...))}
  }

  if (!is.null(accuracy)) the$accuracy <- accuracy
  if (!is.null(font_family)) the$font_family <- font_family
  if (!is.null(intercept_text)) the$intercept_text <- intercept_text
  if (!is.null(column_formats)) the$column_formats <- column_formats
  if (!is.null(number_formatter)) the$number_formatter <- number_formatter
  if (!is.null(trim_leading_zero)) the$trim_leading_zero <- trim_leading_zero

  invisible(old)
}

#' Format data columns
#'
#' @param data data set (data.frame or tibble)
#' @param column_formats `column_formats` object. If NULL, the current default formatter set with [apa7_defaults()] will be used.
#' @param columns (optional) vector of columns to format
#' @param rename_headers if `TRUE`, rename headers with markdown or latex
#' @param latex_headers if `TRUE`, rename headers with latex instead of markdown
#' @param format_separated_headers if `TRUE`, format headers with separated names. For example, if the formatter formats column `R2` as `*R*^2^`, then `Model 1_R2` becomes `Model 1_*R*^2^`)
#' @param sep separator for separated headers (default is "_")
#' @param accuracy numeric (default: NULL, uses the current default accuracy set with [apa7_defaults()]). If not NULL, sets the accuracy for the formatter.
#' @importFrom rlang :=
#'
#' @return tibble
#' @export
#'
#' @examples
#' lm(mpg ~ cyl + wt, data = mtcars) |>
#'   parameters::parameters() |>
#'   apa_format_columns() |>
#'   apa_flextable()
apa_format_columns <- function(data,
                               column_formats = NULL,
                               columns = NULL,
                               rename_headers = TRUE,
                               latex_headers = FALSE,
                               format_separated_headers = TRUE,
                               sep = "_",
                               accuracy = NULL) {
  CI_low <- CI_high <- df_error <- name <- header <- column <- group <- value <- pattern_1 <- pattern_2 <- replace_1 <- replace_2 <- pattern <- type <- should_replace <- id <- replacer <- formatter <- NULL

  if (is.null(column_formats)) {
    column_formats <- the$column_formats
  }

  if (!is.null(accuracy)) {
    column_formats@accuracy <- accuracy
  }

  data <- tibble::as_tibble(data)

  data_names <- colnames(data)
  format_names <- names(column_formats)

  temp_fix_names <- stringr::str_replace_all(
    string = format_names,
    pattern = "_",
    replacement = "apa7separator") |>
    `names<-`(format_names)

  d_formatter <- column_formats@get_tibble

  d_names <- tibble::tibble(column = data_names, name = data_names)

  if (format_separated_headers) {
    d_names <- d_names |>
      dplyr::mutate(
        name = stringr::str_replace_all(name, temp_fix_names) |>
          stringr::str_split_i(pattern = sep, i = -1) |>
          stringr::str_replace_all("apa7separator", "_")
      )
  }

  d_names <- dplyr::left_join(d_names,
                              d_formatter,
                              by = dplyr::join_by(name)) |>
    dplyr::filter(!is.na(header))

  cls <- d_names |> dplyr::pull(column)

  if (!is.null(columns)) {
    cls <- intersect(cls, columns)
  }

  ci <- .95

  if (all(c("CI", "CI_low", "CI_high") %in% d_names$name) &&
      !is.null(column_formats$CI) & ("CI" %in% cls)) {

    d_ci <- d_names |>
      dplyr::mutate(group = stringr::str_remove(column, paste0(sep, name, "$"))) |>
      dplyr::filter(name == "CI") |>
      dplyr::mutate(CI_low = paste0(group, sep, "low"),
             CI_high = paste0(group, sep, "high"))

    for (i in seq_len(nrow(d_ci))) {

      ci_names <- d_ci[i, c("column", "CI_low", "CI_high")] |>
        tidyr::pivot_longer(dplyr::everything()) |>
        dplyr::pull(value)

      ci_name <- ci_names[1]

      if (all(ci_names %in% colnames(data))) {
        ci <- max(data[, ci_names[1]], na.rm = TRUE)

        d_names[d_names$column == d_ci[[i, "column"]], "header"] <- glue::glue(column_formats$CI@header) |>
          as.character()

        data[, ci_names[1]] <- column_formats$CI@formatter(
          dplyr::pull(data, dplyr::any_of(ci_names[2])),
          dplyr::pull(data, dplyr::any_of(ci_names[3])),
          accuracy = column_formats@accuracy
        )
        data[, ci_names[2]] <- NULL
        data[, ci_names[3]] <- NULL
        cls <- cls[cls != ci_names[1]]
      }
    }
  }

  if (all(c("t", "df_error") %in% data_names)  &&
      ("t" %in% cls)) {
    df <- paste0(
      "(",
      align_chr(max(data$df_error, na.rm = TRUE),
                accuracy = the$accuracy),
      ")")

    column_formats$t@header <- paste0(column_formats$t@header, df)

    if (stringr::str_detect(column_formats$t@latex, "\\$")) {
      column_formats$t@latex <- stringr::str_replace(column_formats$t@latex, "\\$$", paste0(df, "$"))
    } else {
      column_formats$t@latex <- paste0(column_formats$t@latex, df)
    }

    data <- data |>
      dplyr::select(-df_error)

    cls <- cls[cls != "df_error"]
  }

  for (cl in cls) {
    f1 <- d_names |>
      dplyr::filter(column == cl) |>
      dplyr::pull(formatter)

    if (!is.null(f1)) {
      if (cl %in% colnames(data)) {
        data <- dplyr::mutate(
          data,
          {{ cl }} := f1[[1]]( data[, cl, drop = TRUE]))
      }
    }
  }

  if (rename_headers) {
    if (latex_headers) {
      rcls <- column_formats@get_latex
    } else {
      rcls <- column_formats@get_headers
    }

    if (nrow(d_names) > 0) {
      rcls <- d_names |>
        dplyr::mutate(pattern_1 = paste0("^", name, "$"),
                      replace_1 = header,
                      pattern_2 = paste0("_",name,"$"),
                      replace_2 = paste0("_", header)) |>
        dplyr::select(column,
                      pattern_1,
                      pattern_2,
                      replace_1,
                      replace_2) |>
        tidyr::pivot_longer(-column) |>
        tidyr::separate(name, c("name", "type")) |>
        tidyr::pivot_wider() |>
        dplyr::mutate(
          should_replace = stringr::str_detect(
            column,
            pattern)) |>
        dplyr::select(-type) |>
        dplyr::filter(should_replace) |>
        dplyr::mutate(id = dplyr::row_number(), .by = column) |>
        dplyr::filter(id == 1) |>
        dplyr::mutate(
          replacer = stringr::str_replace(
            column,
            pattern,
            replace)) |>
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
#'
#' star_balance(".05\\^\\*\\*\\^")
star_balance <- function(x, star = "\\*", superscript = TRUE) {
  k <- stringr::str_count(x, star)
  ss <- strrep(ifelse(superscript, "^", ""), (k > 0) * 1)
  ss <- paste0(ss, strrep("&numsp;", k), ss, x)
  ss[is.na(x)] <- NA
  ss
}




