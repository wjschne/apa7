#' Run shiny app to make a document in APA style via Quarto
#'
#' A wrapper for `shiny::runGitHub`
#' @param launch.browser run shiny app in default browser
#' @returns Runs a shiny app that creates apaquarto documents
#' @export
#'
#' @examples
#' \dontrun{
#' make_apaquarto()
#' }
make_apaquarto <- function(launch.browser = TRUE) {
  shiny::runGitHub(repo = "apa7maker",
                   username = "wjschne",
                   launch.browser = launch.browser)
}



#' Installs the apaquarto extension.
#'
#' A wrapper for `quarto::quarto_add_extension`
#' @param no_prompt Do not prompt to confirm approval to download external extension.
#' @param quiet Suppress warning and other messages
#' @param quarto_args Character vector of other quarto CLI arguments to append to the Quarto command executed by this function.
#'
#' @returns installs the apaquarto Quarto extension
#' @export
#'
#' @examples
#' \dontrun{
#' install_apaquarto()
#' }
install_apaquarto <- function(no_prompt = FALSE,
                              quiet = FALSE,
                              quarto_args = NULL) {
  quarto::quarto_add_extension(
    extension = "wjschne/apaquarto",
    no_prompt = no_prompt,
    quiet = quiet,
    quarto_args = quarto_args
  )
}

