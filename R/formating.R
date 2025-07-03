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
apa_p <- function(p,
                  inline = TRUE,
                  plain = FALSE) {

  if (inline) {
    if (p < .001) {
      if (plain) {
        return("p < .001")
      } else {
        return("*p*&nbsp;<&nbsp;.001")
      }
    }
  }

  digit <- ifelse(p < .0095, 3, 2)
  p_formatted <- purrr::map2_chr(p, digit, function(pp, dd) formatC(round(pp, dd + 1), digits = dd, format = "f"))
  p_formatted <- sub(x = p_formatted ,
                     pattern = "^0\\.",
                     replacement = ".")

  operator <- ifelse(p < .001, "<", ifelse(p >= .995, ">", "="))
  sep <- ifelse(plain, " ", "&nbsp;")
  p_symbol <- ifelse(plain, "p", "*p*")

  prefix <- paste0(p_symbol, sep,operator, sep)
  if (inline) {
    p_formatted[p < .001] <- ".001"
    p_formatted[p >= .995] <- ".99"
  } else {
    p_formatted[p < .001] <- "<.001"
    p_formatted[p >= .995] <- ">.99"
  }

  paste0(ifelse(inline, prefix, ""), p_formatted)
}

