# Subgroup analysis table for a meta-analysis

Makes an APA-style table of a subgroup analysis of a meta-analysis fit
with
[`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html):
the pooled estimate of each subgroup, followed by the overall pooled
estimate, with the test of subgroup differences in the note.

## Usage

``` r
apa_subgroups(fit, subgroup, tau2 = c("separate", "common"), transform = TRUE)
```

## Arguments

- fit:

  a meta-analysis without moderators fit with
  [`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)

- subgroup:

  the subgroup variable: the name of a column in the data of `fit`
  (quoted or unquoted), or a vector with one value for each effect size.
  Effect sizes whose subgroup is missing are left out of the subgroups
  (but not of the overall estimate).

- tau2:

  `"separate"` (default) for a \\\tau^2\\ for each subgroup, or
  `"common"` for one \\\tau^2\\ shared by all subgroups

- transform:

  if `TRUE` (default), estimates computed on a transformed scale are
  shown on their original scale, as in
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)

## Value

`apa7_tbl` (tibble)

## Details

With `tau2 = "separate"` (default), each subgroup is pooled with its own
random-effects model (with the estimator of \\\tau^2\\ of `fit`), and
the subgroup estimates are compared with \\Q\_{\text{between}}\\, the
heterogeneity statistic of an equal-effects meta-analysis of the
subgroup estimates. With `tau2 = "common"`, the subgroups share one
\\\tau^2\\: the estimates come from a mixed-effects meta-regression with
the subgroup as a categorical moderator, and the subgroups are compared
with the omnibus test of the moderator (\\Q\_{\text{M}}\\). The separate
approach suits subgroups that may differ in heterogeneity; the common
approach gives more stable estimates of \\\tau^2\\ when subgroups have
few studies (Borenstein et al., 2009, chap. 19).

Estimates are shown on the same scale as in
[`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
(e.g., risk ratios are exponentiated, and Fisher's *z* values are
converted back to correlations).

## References

Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R.
(2009). *Introduction to meta-analysis*. Wiley.
[doi:10.1002/9780470743386](https://doi.org/10.1002/9780470743386)

## Examples

``` r
# \donttest{
if (requireNamespace("metafor", quietly = TRUE) &&
    requireNamespace("metadat", quietly = TRUE)) {
  d_bcg <- metafor::escalc(
    measure = "RR",
    ai = tpos, bi = tneg, ci = cpos, di = cneg,
    data = metadat::dat.bcg
  )
  fit_meta <- metafor::rma(yi, vi, data = d_bcg)
  apa_subgroups(fit_meta, alloc)
}
#> # A tibble: 4 × 6
#>   Alloc      `*k*` `*RR*` `95% CI`     `*p*` `*I*^2^ (%)`
#>   <chr>      <chr> <chr>  <chr>        <chr> <chr>       
#> 1 alternate  2     0.58   [0.34, 1.01]  .06  82.0        
#> 2 random     7     0.38   [0.22, 0.65] <.001 89.9        
#> 3 systematic 4     0.65   [0.32, 1.32]  .24  86.4        
#> 4 Overall    13    0.49   [0.34, 0.70] <.001 92.2        
# }
```
