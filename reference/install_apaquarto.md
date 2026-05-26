# Installs the apaquarto extension.

A wrapper for
[`quarto::quarto_add_extension`](https://quarto-dev.github.io/quarto-r/reference/quarto_add_extension.html)

## Usage

``` r
install_apaquarto(no_prompt = FALSE, quiet = FALSE, quarto_args = NULL)
```

## Arguments

- no_prompt:

  Do not prompt to confirm approval to download external extension.

- quiet:

  Suppress warning and other messages

- quarto_args:

  Character vector of other quarto CLI arguments to append to the Quarto
  command executed by this function.

## Value

installs the apaquarto Quarto extension

## Examples

``` r
if (FALSE) { # \dontrun{
install_apaquarto()
} # }
```
