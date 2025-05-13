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
#' @param inline If TRUE (default), returns statistic (e.g.,e p = .04), otherwise just the number (e.g., .04)
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
                  inline = TRUE) {

  if (p < 0.001 && inline) return("*p*&nbsp;<&nbsp;.001")
  if (p < 0.001 && !inline) return("<.001")
  digit <- ifelse(p < .0095, 3, 2)
  p_formatted <- sapply(p, function(x) formatC(round(x, digit + 1), digit, format = "f"))
  p_formatted <- sub(x = p_formatted ,
                     pattern = "^0\\.",
                     replacement = ".")
  paste0(ifelse(inline, "*p*&nbsp;=&nbsp;", ""), p_formatted)
}

