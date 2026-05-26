# Make star notes for p-values

Make star notes for p-values

## Usage

``` r
apa_p_star_note(x = c(0.05, 0.01, 0.001), first_alpha_marginal = FALSE)
```

## Arguments

- x:

  vector of alpha values (p-value thresholds)

- first_alpha_marginal:

  if TRUE, the first alpha value is treated as marginal and gets a
  dagger instead of a star

## Value

character vector

## Examples

``` r
apa_p_star_note()
#> [1] "^\\* ^*p* < .05. ^\\*\\* ^*p* < .01. ^\\*\\*\\* ^*p* < .001"
apa_p_star_note(x = c(.10, .05, .01, .001), first_alpha_marginal = TRUE)
#> [1] "^† ^*p* < .1. ^\\* ^*p* < .05. ^\\*\\* ^*p* < .01. ^\\*\\*\\* ^*p* < .001"
```
