.onLoad <- function(...) {
  if ("S7_on_load" %in% getNamespaceExports("S7")) {
    suppressMessages(getExportedValue("S7", "S7_on_load")()) # nocov
  } else {
    suppressMessages(S7::methods_register()) # nocov
  }
  # Pre-load ftExtra namespace silently so its S3 method overwrite message
  # does not appear mid-session on first use of apa_chisq / apa_cor / etc.
  if (!isNamespaceLoaded("ftExtra")) {     # nocov
    suppressMessages(loadNamespace("ftExtra"))  # nocov
  }
}

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

.onUnload <- function(...) {
  if ("S7_on_unload" %in% getNamespaceExports("S7")) {
    getExportedValue("S7", "S7_on_unload")()
  }
}

if ("S7_on_build" %in% getNamespaceExports("S7")) {
  getExportedValue("S7", "S7_on_build")()
}
