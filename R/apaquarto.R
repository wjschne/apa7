#' install the apaquarto extension from https://github.com/wjschne/apaquarto
#'
#' @param make_project After importing apaquarto to folder, make that folder a project.
#' @export
#' @examples
#' install_apaquarto
install_apaquarto <- function(make_project = TRUE) {
  rstudioapi::verifyAvailable()

  path <- rstudioapi::selectDirectory(caption = "Select a directory (i.e., folder) , in which to install apaquarto.")
  if (!is.null(path) && dir.exists(path)) {
    old <- getwd()
    setwd(path)
    quarto::quarto_add_extension("wjschne/apaquarto", no_prompt = TRUE)
    purrr::walk(c(
      "template.qmd",
      "example.qmd",
      "apa.csl",
      "bibliography.bib",
      "sampleimage.png"
    ), function(myfile) {
      m <- ifelse(myfile == "sampleimage.png", "wb", "w")
      utils::download.file(
        url = fs::path("https://raw.github.com/wjschne/apaquarto/main/", myfile),
        destfile = fs::path(path, myfile),
        mode = m
      )
    })

    if (make_project) {
      if (length(dir(path, ".Rproj$")) > 0) {
        dirpath <- fs::path_split(path)[[1]]
        quarto::quarto_create_project(dirpath[length(dirpath)],
                                      type = "default",
                                      dir = fs::path_dir(path) )

      }
      rstudioapi::openProject(path, newSession = TRUE)
    }
  }

}

#' p-value in APA format
#'
#' @param p probability
#' @param inline If TRUE (default), returns statistic (e.g., p = .04), otherwise just the number (e.g., .04)
#'
#' @return character vector
#' @export
#'
#' @examples
#' # Values less than .001 are <.001
#' apa_p(.0002, inline = FALSE)
#' # Values between .001 and .01 are rounded to 3 digits
#' apa_p(.002, inline = FALSE)
#' # Values between .01 and .995 are rounded to 2 digits
#' apa_p(.02, inline = FALSE)
#' apa_p(.22, inline = FALSE)
#' apa_p(.994, inline = FALSE)
#' # Values above .995 are >.99
#' apa_p(.999, inline = FALSE)
apa_p <- function(p,
                  inline = TRUE) {

  digit <- ifelse(p < .0095, 3, 2)
  p_formatted <- sapply(p, function(x) formatC(round(x, digit + 1), digit, format = "f"))

  p_formatted <- sub(x = p_formatted ,
                     pattern = "^0\\.",
                     replacement = ".")
  p_formatted <- paste0(ifelse(inline, "*p*&nbsp;=&nbsp;", ""), p_formatted)
  p_formatted[p < 0.001 & inline] <- "*p*&nbsp;<&nbsp;.001"
  p_formatted[p < 0.001 & !inline] <- "<.001"
  p_formatted[p >= 0.995 & inline] <- "*p*&nbsp;>&nbsp;.99"
  p_formatted[p >= 0.995 & !inline] <- ">.99"
  p_formatted
}

