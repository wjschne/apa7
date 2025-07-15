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

apa_template <- function(font_family = the$font_family, paper_size = c("us-letter", "a4")) {
  paper_size <- match.arg(paper_size, c("us-letter", "a4"))
  paper_size
}


apa_unzip <- function(file = "inst/apaquarto.docx",
                      exdir = "inst/sourcedocx") {

  docx_fs <- zip::unzip(file, exdir = exdir)
  docx_fs
}
# setwd("C:/Users/renee/Dropbox/packages/apa7")

# dir(system.file("sourcedocx", package = "apa7", mustWork = T))

# docx_fs <- apa_unzip()
# getwd()
# setwd("inst/sourcedocx")
# zip::zip("../../asdf2.docx", dir(), include_directories = F)
# setwd("C:/Users/renee/Dropbox/packages/apa7")


