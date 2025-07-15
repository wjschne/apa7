#' p-value in APA format
#'
#' @param p probability
#' @param inline If TRUE (default), returns statistic (e.g.,e p = .04), otherwise just the number (e.g., .04)
#' @param plain Outputs plain text compatible with latex if TRUE, otherwise defaults to  markdown
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
                  plain = FALSE,
                  min_digits = 2,
                  max_digits = 3) {
  min_p <- 10 ^ (-max_digits)
  min_p_chr <- signs::signs(min_p, trim_leading_zeros = TRUE, accuracy = min_p)

  if (inline) {
    if (length(p) > 1) stop("Only one p can be processed for inline mode.")
    if (p < min_p) {
      if (plain) {
        return(paste0("p < ", min_p_chr))
      } else {
        return(paste0("*p*\u00A0<\u00A0", min_p_chr))
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

  operator <- ifelse(p < min_p, "<", ifelse(p >= max_p_threshold, ">", "="))
  sep <- ifelse(plain, " ", "\u00A0")
  p_symbol <- ifelse(plain, "p", "*p*")

  prefix <- paste0(p_symbol, sep,operator, sep)
  if (inline) {
    p_formatted[p < min_p] <- min_p_chr
    p_formatted[p >= max_p_threshold] <- max_p_chr
  } else {
    p_formatted[p < min_p] <- paste0("<", min_p_chr)
    p_formatted[p >= max_p_threshold] <- paste0(">", max_p_chr)
  }

  pv <- paste0(ifelse(inline, prefix, ""), p_formatted)
  pv[is.na(p)] <- NA
  pv
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
#' @returns character vector
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
    str_replace_all("\n", paste0(newline, indent_chr)) |>
    str_replace_all("\u2057", "\u2007")


}




#' Pads text on the left or right so that the width is the same for each element of the vector
#'
#' @param x vector of text
#' @param pad_left if TRUE (default), pads on the left, otherwise pads on the right
#' @param padding_character character to use for padding, default is `&numsp;` (figure space)
#' @param NA_value value to replace NA
#'
#' @returns character vector
#' @export
#'
#' @examples
#' num_pad(c("a", "bb"))
num_pad <- function(x,
                    pad_left = TRUE,
                    padding_character = "&numsp;",
                    NA_value = "") {
  nch <- nchar(x)
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
#' @param ... additional arguments passed to `signs::signs()`
#'
#' @returns character vector
#' @export
#'
#' @examples
#' align_chr(c(1, 10, 100))
align_chr <- function(
    x,
    accuracy = NULL,
    trim_leading_zeros = FALSE,
    add_plusses = FALSE,
    padding_character = "\u2007",
    center = "\\.",
    format_integers = FALSE,
    side = c("both", "left", "right"),
    NA_value = "",
    ...) {

  if (is.null(accuracy)) accuracy <- the$accuracy
  xx <- x

  side <- match.arg(side, c("both", "left", "right"))

  if (rlang::is_integerish(x) && !format_integers) {
    xx  <- num_pad(signs::signs(
      xx, add_plusses = add_plusses),
      padding_character = padding_character)

  } else {
    if (is.numeric(x)) {
      xx <- signs::signs(
        xx,
        accuracy = accuracy,
        add_plusses = add_plusses,
        trim_leading_zeros = trim_leading_zeros,
        ...)
    }

    # Make split on only on the first instance of center
    splitter <- "apa7replacecharacter"
    xx <- stringr::str_replace(xx, center, splitter)

    left_x <- stringr::str_split_i(
      xx,
      pattern = splitter,
      i = 1)

    right_x <- stringr::str_split_i(
      xx,
      pattern = splitter,
      i = 2)

    if (side != "right") {
      left_x <- num_pad(left_x, padding_character = padding_character, NA_value = NA_value)
    }

    if (side != "left") {
      right_x <- num_pad(right_x,
                         pad_left = FALSE,
                         padding_character = padding_character, NA_value = NA_value)
    }

    middle <- paste0(stringr::str_remove_all(center, "\\\\"),
                     rep("", length(xx)))

    middle[is.na(xx)] <- ""


    xx <- paste0(left_x,
                middle,
                right_x)

  }
  xx[is.na(x) | x == length(x) | x == ""] <- NA_value


  xx
}

#' Convert p-values to stars
#'
#' @param p vector of numbers
#' @param alpha vector of thresholds
#' @param prefix usually backslashes to prevent markdown from interpreting asterisks as bullets or italics
#' @param superscript make as superscript
#'
#' @returns character vector
#' @export
#'
#' @examples
#' p2stars(c(.32, .02, .005),
#'         alpha = c(.05, .01))
p2stars <- function(p,
                    alpha = c(0.05, .01, .001),
                    superscript = FALSE,
                    prefix = "\\") {
  pstars <- purrr::map_chr(p, \(pp) {
    if (is.na(pp)) return("")
    paste0(rep(paste0(prefix, "*"),
               sum((pp <= alpha) * 1L)),
           collapse = "")
  })
  if (superscript) {
    pstars[nchar(pstars) > 0] <- paste0(
      "^",
      pstars[nchar(pstars) > 0],
      "^")
  }
  pstars

}




#' Surrounds text with tags unless empty
#'
#' @param x character vector
#' @param fence character vector
#'
#' @returns character vector
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
subcript_md <- function(x) {
  tagger(x, "~")
}

#' @rdname tagger
#' @param level heading level
#' @export
header_md <- function(x, level = 1) {
  tagger(x, paste0(paste0(rep("#", level), collapse = ""), " "), right_tag = "")
}


library(S7)

apa_column <- new_class("apa_column", properties = list(
  name = class_character,
  header = class_character,
  latex = class_character,
  formatter = class_function
))

method(str, apa_column) <- function(object, ...) {
  cli::cli_h2(S7_class(object)@name)
  print(tibble::tibble(name = object@name,
         header = object@header,
         formatter = list(object@formatter)))
  invisible(object)
  # str(object = props(object, c("name", "header")))
}

apa_column_list <- new_class("apa_column_list", parent = class_list)

# `@.apa7::apa_column_list` <- function(x, i) {
#   x[i]
# }

# internal states ----
the <- new.env(parent = emptyenv())
the$accuracy <- .01
the$intercept_text <- "Constant"
the$font_family <- "Times New Roman"
# Columns ----
the$columns <- list(
  AIC = apa_column(
    name = "AIC",
    header = "*AIC*",
    latex = "$AIC$",
    formatter = \(x) align_chr(
      x,
      accuracy = the$accuracy
    )
  ),
  AICc = apa_column(
    name = "AICc",
    header = "*AICc*",
    latex = "$AICc$",
    formatter = \(x) align_chr(
      x,
      accuracy = the$accuracy
    )
  ),
  BIC = apa_column(
    name = "BIC",
    header = "*BIC*",
    latex = "$BIC$",
    formatter = \(x) align_chr(
      x,
      accuracy = the$accuracy
    )
  ),
  BICc = apa_column(
    name = "BICc",
    header = "*BICc*",
    latex = "$BICc$",
    formatter = \(x) align_chr(
      x,
      accuracy = the$accuracy
    )
  ),
  Chi2 = apa_column(
    name = "Chi2",
    header = "*&chi*^2^",
    latex = "$\\chi^2$",
    formatter = \(x) align_chr(
      x,
      accuracy = the$accuracy
    )
  ),
  CI = apa_column(
    name = "CI",
    header = "{round(ci * 100)}% CI",
    latex = "{round(ci * 100)}\\%",
    formatter = \(l, u) {
      middle <- rep(", ", length(l))
      middle[is.na(l) | is.na(u)] <- ""
      align_chr(
        paste0(align_chr(l, accuracy = the$accuracy),
               middle,
               align_chr(u, accuracy = the$accuracy)
               ), center = ", ") |>
        tagger("[", "]")
    }
  ),
  Coefficient = apa_column(
    name = "Coefficient",
    header = "*B*",
    latex = "$B$",
    formatter = \(x, starred = FALSE) {
      align_chr(x, accuracy = the$accuracy)
    }
  ),
  deltaR2 = apa_column(
    name = "deltaR2",
    header = "&Delta;*R*^2^",
    latex = "$\\Delta R^2$",
    formatter = \(x) align_chr(
      x,
      accuracy = the$accuracy,
      trim_leading_zeros = TRUE
    )
  ),
  df = apa_column(
    name = "df",
    header = "*df*",
    latex = "$df$",
    formatter = \(x) align_chr(x)
  ),
  df_diff = apa_column(
    name = "df_diff",
    header = "&Delta;*df*",
    latex = "$\\Delta df$",
    formatter = \(x) align_chr(x)
  ),
  `F` = apa_column(
    name = "F",
    header = "*F*",
    latex = "$F$",
    formatter = \(x) align_chr(x, accuracy = the$accuracy)
  ),
  p = apa_column(
    name = "p",
    header = "*p*",
    latex = "$p$",
    formatter = \(x) align_chr(apa_p(x))
  ),
  Parameter = apa_column(
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
  R2 = apa_column(
    name = "R",
    header = "*R*^2^",
    latex = "$R^2$",
    formatter = \(x) align_chr(
      x,
      accuracy = the$accuracy,
      trim_leading_zeros = TRUE
    )
  ),
  R2_adjusted = apa_column(
    name = "R2_adjusted",
    header = "adj*R*^2^",
    latex = "$\\text{adj}R^2$",
    formatter = \(x) align_chr(
      x,
      accuracy = the$accuracy,
      trim_leading_zeros = TRUE
    )
  ),
  RMSE = apa_column(
    name = "RMSE",
    header = "*RMSE*",
    latex = "$RMSE$",
    formatter = \(x) align_chr(x, accuracy = the$accuracy)
  ),
  SE = apa_column(
    name = "SE",
    header = "*SE B*",
    latex = "$SE~B$",
    formatter = \(x) align_chr(x, accuracy = the$accuracy)
  ),
  Sigma = apa_column(
    name = "Sigma",
    header = "&sigma;~*e*~",
    latex = "$\\sigma_{e}$",
    formatter = \(x) align_chr(x, accuracy = the$accuracy)
  ),
  Std_Coefficient = apa_column(
    name = "Std_Coefficient",
    header = "&beta;",
    latex = "$\\beta$",
    formatter = \(x) align_chr(
      x,
      accuracy = the$accuracy,
      trim_leading_zeros = TRUE
    )
  ),
  t = apa_column(
    name = "t",
    header = "*t*",
    latex = "$t$",
    formatter = \(x) align_chr(x, accuracy = the$accuracy)
  )
)


# apa_parameter_formatter ----
#' Create a set of column formats
#'
#' @param accuracy numeric (passed to scales::number)
#' @param intercept_text describe intercept
#' @param starred_columns which columns get p-value stars
#' @param variable_labels named vector of variable names (with vector names as labels). For example, c(`Parental Income` = "parental_income", `Number of Siblings` = "n_siblings")
#'
#' @returns parameter_formatter
#' @export
#'
#' @examples
#' my_formatter <- parameter_formatter()
#' my_formatter@Coefficient@formatter <- \(x) round(x, 2)
#' my_formatter@Coefficient@formatter(2.214)
parameter_formatter <- new_class(
  name = "parameter_formatter",
  parent = class_list,
  properties = list(
    accuracy = class_double,
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
      purrr::map_chr(S7_data(self), "latex")
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
    variable_labels = character(0)
  ) {
    if (is.null(.data)) .data <- the$columns
    if (is.null(accuracy)) accuracy <- the$accuracy
    if (is.null(intercept_text)) intercept_text <- the$intercept_text

    S7::new_object(
      .data,
      accuracy = accuracy,
      intercept_text = intercept_text,
      starred_columns = starred_columns,
      variable_labels = variable_labels
    )
  }
)

method(str, parameter_formatter) <- function(object, ...) {
  # print(cli::cli_h2(S7_class(object)@name))
  cat("<", S7_class(object)@name, ">\n", sep = "")
  cat("\tColumns\n")
  print(object@get_tibble)
  cat("\tProperties\n")
  pr <- prop_names(object)
  pr <- pr[stringr::str_starts(pr, "get_", TRUE)]
  cat(str(object = props(object, pr)))
  invisible(object)

}

method(print, parameter_formatter) <- function(object, ...) {
  cli::cli_h2(S7_class(object)@name)
  # cat("<", S7_class(object)@name, ">\n", sep = "")
  print(object@get_tibble)
  invisible(object)

}


the$parameter_formatter <- parameter_formatter()


#' Set defaults for apa7 package
#'
#' @param accuracy numeric (default: .01)
#' @param font_family font family
#' @param intercept_text what to call the intercept
#' @param parameter_formatter column formatting functions
#' @param reset if `TRUE`, reset all defaults (except as specified)
#'
#' @returns previous defaults
#' @export
#'
#' @examples
#' apa7_defaults(accuracy = .001)
#' # Reset to package defaults
#' apa7_defaults(reset = TRUE)
apa7_defaults <- function(accuracy = NULL,
                          font_family = NULL,
                          intercept_text = NULL,
                          parameter_formatter = NULL,
                          reset = FALSE) {
  old <- the
  if (reset) {
    the$accuracy = .01
    the$parameter_formatter <- parameter_formatter()
    the$intercept_text <- "Constant"
    the$font_family = "Times New Roman"
  }

  if (!is.null(accuracy)) the$accuracy = accuracy
  if (!is.null(font_family)) the$font_family = font_family
  if (!is.null(intercept_text)) the$intercept_text = intercept_text
  if (!is.null(parameter_formatter)) the$parameter_formatter = parameter_formatter

  invisible(old)
}


#' Format data columns
#'
#' @param data data set (data.frame or tibble)
#' @param formatter `apa_parameter_formatter` object. If NULL, the current default formatter set with [apa7_defaults()] will be used.
#' @param columns (optional) vector of columns to format
#' @param rename_headers if `TRUE`, rename headers with markdown or latex
#' @param latex_headers if `TRUE`, rename headers with latex instead of markdown
#'
#' @returns tibble
#' @export
#'
#' @examples
#' lm(mpg ~ cyl + wt, data = mtcars) |>
#'   parameters::parameters() |>
#'   apa_format_columns() |>
#'   apa_flextable() |>
#'   colformat_md(part = "header")
apa_format_columns <- function(
    data,
    formatter = NULL,
    columns = NULL,
    rename_headers = TRUE,
    latex_headers = FALSE) {
  if (is.null(formatter)) formatter <- the$parameter_formatter


  data <- tibble::as_tibble(data)
  data_names <- colnames(data)
  format_names <- names(formatter)
  cls <- intersect(data_names, format_names)
  if (!is.null(columns)) {
    cls <- intersect(cls, columns)
  }

  ci <- .95


  if (all(c("CI", "CI_low", "CI_high") %in% data_names) &&
      !is.null(formatter$CI) & ("CI" %in% cls)) {
    ci <- max(data$CI, na.rm = TRUE)
    formatter$CI@header <- glue::glue(formatter$CI@header) |> as.character()
    formatter$CI@latex <- glue::glue(formatter$CI@latex) |> as.character()
    data <- data |>
      dplyr::mutate(CI = purrr::map2_chr(CI_low, CI_high, formatter$CI@formatter)) |>
      dplyr::select(-CI_high, -CI_low)


    cls <- cls[cls != "CI"]

  }

  if (all(c("t", "df_error") %in% data_names) &&
      !is.null(formatter$CI) && ("t" %in% cls)) {
    df <- paste0("(", align_chr(max(data$df_error, na.rm = TRUE), accuracy = the$accuracy), ")")
    formatter$t@header <- paste0(formatter$t@header, df)

    if (stringr::str_detect(formatter$t@latex, "\\$")) {
      formatter$t@latex <- stringr::str_replace(
        formatter$t@latex,
        "\\$$",
        paste0(df, "$"))
    } else {
      formatter$t@latex <- paste0(formatter$t@latex, df)
    }

    data <- data |>
      dplyr::select(-df_error)


    cls <- cls[cls != "df_error"]

  }

  for (cl in cls) {
    data <- dplyr::mutate(
      data,
      dplyr::across(dplyr::any_of(cl),
                    formatter[[cl]]@formatter))
  }

  if (rename_headers) {
    if (latex_headers) {
      rcls <- formatter@get_header_rename_latex
    } else {
      rcls <- formatter@get_header_rename

    }
    dplyr::rename(data, dplyr::any_of(rcls[rcls %in% cls]))

  } else {
    data
  }


}

#' Prefix text with figure spaces to balance star text
#'
#' @param x character vector
#' @param star star text
#' @param superscript Place superscript text if `TRUE`
#'
#' @returns character vector
#' @export
#'
#' @examples
#'
#' star_balance(".05\\^\\*\\*\\^")
star_balance <- function(x, star = "\\*", superscript = TRUE) {
  k <- str_count(x, star)
  ss <- strrep(ifelse(superscript, "^", ""), (k > 0) * 1)
  paste0(ss, strrep("&numsp;", k), ss, x)
}
