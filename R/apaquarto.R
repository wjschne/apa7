#' Run shiny app to make a document in APA style via Quarto
#'
#' A wrapper for `shiny::runGitHub` Note that running this function will install any missing packages needed to run the app: bsicons, bslib, conflicted, dplyr, fresh, purrr, rclipboard, readr, shiny, shinyWidgets, snakecase, tibble, tidyr, tippy, toastui, yaml
#' @param launch.browser run shiny app in default browser
#' @returns Runs a shiny app that creates apaquarto documents
#' @export
#'
#' @examples
#' \dontrun{
#' make_apaquarto()
#' }
make_apaquarto <- function(launch.browser = TRUE) {
  # nocov start
  # https://stackoverflow.com/a/44660688/4513316
  using <- function(...) {
    libs <- unlist(list(...))
    req <- unlist(lapply(libs, require, character.only = TRUE))
    need <- libs[req == FALSE]
    if (length(need) > 0) {
      response <- utils::askYesNo(
        paste0(
          "Some missing packages are needed to run this app. Install ",
          xfun::join_words(need),
          "?"
        )
      )
      if (isTRUE(response)) {
        utils::install.packages(need)
      } else {
        print("Action cancelled.")
      }
    }
    invisible(need)
  }
  installed <- using(
    "conflicted",
    "toastui",
    "shiny",
    "dplyr",
    "tippy",
    "bslib",
    "bsicons",
    "shinyWidgets",
    "tidyr",
    "purrr",
    "yaml",
    "rclipboard",
    "readr",
    "snakecase",
    "tibble",
    "fresh"
  )

  shiny::runGitHub(
    repo = "apa7maker",
    username = "wjschne",
    launch.browser = launch.browser
  )
} # nocov end

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
install_apaquarto <- function(
  no_prompt = FALSE,
  quiet = FALSE,
  quarto_args = NULL
) {
  # nocov start
  quarto::quarto_add_extension(
    extension = "wjschne/apaquarto",
    no_prompt = no_prompt,
    quiet = quiet,
    quarto_args = quarto_args
  )
} # nocov end
