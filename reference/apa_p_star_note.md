# Make star notes for p-values

Make star notes for p-values

## Usage

``` r
apa_p_star_note(alpha = c(0.05, 0.01, 0.001), first_alpha_marginal = FALSE, x)
```

## Arguments

- alpha:

  vector of alpha values (*p*-value thresholds), from largest to
  smallest. Each gets one more star.

- first_alpha_marginal:

  if TRUE, the first alpha value is treated as marginal and gets a
  dagger instead of a star

- x:

  deprecated; use `alpha`

## Value

character vector

## Examples

``` r
apa_p_star_note()
#> [1] "^\\* ^*p* < .05. ^\\*\\* ^*p* < .01. ^\\*\\*\\* ^*p* < .001"
apa_p_star_note(alpha = c(0.10, 0.05, 0.01, 0.001),
                first_alpha_marginal = TRUE)
#> [1] "^† ^*p* < .1. ^\\* ^*p* < .05. ^\\*\\* ^*p* < .01. ^\\*\\*\\* ^*p* < .001"
```
