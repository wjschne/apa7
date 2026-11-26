# nocov start
# Check for missing packages and offer to install them.
# The list of packages is printed as a message, and the question itself is
# kept short so that it fits on one line in any console (e.g., R on macOS,
# where askYesNo prompts in the console rather than in a dialog box).
install_missing_packages <- function(pkgs, ask = TRUE) {
  need <- pkgs[!vapply(pkgs, rlang::is_installed, logical(1))]
  if (length(need) == 0) {
    return(invisible(need))
  }

  install_call <- paste0(
    "install.packages(c(",
    paste0('"', need, '"', collapse = ", "),
    "))"
  )

  if (ask) {
    if (!interactive()) {
      stop(
        "These packages are needed but not installed: ",
        paste0(need, collapse = ", "),
        "\nInstall them with:\n",
        install_call,
        call. = FALSE
      )
    }
    message(
      "These packages are needed but not installed:\n  ",
      paste0(need, collapse = ", "),
      "\n"
    )
    response <- utils::askYesNo("Install them now?")
  } else {
    response <- TRUE
  }

  if (isTRUE(response)) {
    utils::install.packages(need)
  } else {
    stop(
      "Installation cancelled. To install the missing packages, run:\n",
      install_call,
      call. = FALSE
    )
  }
  invisible(need)
}
# nocov end

#' Run shiny app to make a document in APA style via Quarto
#'
#' A wrapper for `shiny::runGitHub` Note that running this function will install any missing packages needed to run the app: bsicons, bslib, conflicted, dplyr, fresh, purrr, ragg, rclipboard, readr, shiny, shinyWidgets, snakecase, svglite, tibble, tidyr, tippy, toastui, yaml
#' @param launch.browser run shiny app in default browser
#' @param ask if `TRUE` (default), ask before installing missing packages. If `FALSE`, install missing packages without asking.
#' @returns Runs a shiny app that creates apaquarto documents
#' @export
#'
#' @examples
#' \dontrun{
#' make_apaquarto()
#' }
make_apaquarto <- function(launch.browser = TRUE, ask = TRUE) {
  # nocov start
  install_missing_packages(
    c(
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
      "fresh",
      "ragg",
      "svglite"
    ),
    ask = ask
  )

  shiny::runGitHub(
    repo = "apa7maker",
    username = "wjschne",
    launch.browser = launch.browser
  )
} # nocov end

#' Install the apaquarto extension
#'
#' A wrapper for `quarto::quarto_add_extension`
#' Two separate questions can be asked: whether to install R packages that
#' apaquarto needs (quarto, ragg, and svglite), controlled by `ask`, and
#' whether to trust and download the extension from GitHub, a question Quarto
#' itself asks, controlled by `no_prompt`.
#'
#' @param no_prompt if `TRUE`, Quarto installs the extension without asking
#'   you to confirm the download. If `FALSE` (default), it asks.
#' @param quiet if `TRUE`, Quarto's messages and warnings are suppressed
#' @param quarto_args character vector of other Quarto command-line arguments, appended to the `quarto add` command
#' @param ask if `TRUE` (default), ask before installing missing R packages (quarto, ragg, and svglite). If `FALSE`, install them without asking.
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
  quarto_args = NULL,
  ask = TRUE
) {
  # nocov start
  install_missing_packages(c("quarto", "ragg", "svglite"), ask = ask)

  quarto::quarto_add_extension(
    extension = "wjschne/apaquarto",
    no_prompt = no_prompt,
    quiet = quiet,
    quarto_args = quarto_args
  )
} # nocov end
