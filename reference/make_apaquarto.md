# Run shiny app to make a document in APA style via Quarto

A wrapper for
[`shiny::runGitHub`](https://rdrr.io/pkg/shiny/man/runUrl.html) Note
that running this function will install any missing packages needed to
run the app: bsicons, bslib, conflicted, dplyr, fresh, purrr, ragg,
rclipboard, readr, shiny, shinyWidgets, snakecase, svglite, tibble,
tidyr, tippy, toastui, yaml

## Usage

``` r
make_apaquarto(launch.browser = TRUE, ask = TRUE)
```

## Arguments

- launch.browser:

  run shiny app in default browser

- ask:

  if `TRUE` (default), ask before installing missing packages. If
  `FALSE`, install missing packages without asking.

## Value

Runs a shiny app that creates apaquarto documents

## Examples

``` r
if (FALSE) { # \dontrun{
make_apaquarto()
} # }
```
