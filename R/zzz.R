.onLoad <- function(...) {
  suppressMessages(S7::methods_register()) # nocov
  # Pre-load ftExtra namespace silently so its S3 method overwrite message
  # does not appear mid-session on first use of apa_chisq / apa_cor / etc.
  if (!isNamespaceLoaded("ftExtra")) {     # nocov
    suppressMessages(loadNamespace("ftExtra"))  # nocov
  }
}

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL
