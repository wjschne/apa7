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
      download.file(
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

