# Install the apaquarto extension

A wrapper for
[`quarto::quarto_add_extension`](https://quarto-dev.github.io/quarto-r/reference/quarto_add_extension.html)
Two separate questions can be asked: whether to install R packages that
apaquarto needs (quarto, ragg, and svglite), controlled by `ask`, and
whether to trust and download the extension from GitHub, a question
Quarto itself asks, controlled by `no_prompt`.

## Usage

``` r
install_apaquarto(
  no_prompt = FALSE,
  quiet = FALSE,
  quarto_args = NULL,
  ask = TRUE
)
```

## Arguments

- no_prompt:

  if `TRUE`, Quarto installs the extension without asking you to confirm
  the download. If `FALSE` (default), it asks.

- quiet:

  if `TRUE`, Quarto's messages and warnings are suppressed

- quarto_args:

  character vector of other Quarto command-line arguments, appended to
  the `quarto add` command

- ask:

  if `TRUE` (default), ask before installing missing R packages (quarto,
  ragg, and svglite). If `FALSE`, install them without asking.

## Value

installs the apaquarto Quarto extension

## Examples

``` r
if (FALSE) { # \dontrun{
install_apaquarto()
} # }
```
