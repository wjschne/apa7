# Publication bias table for a meta-analysis

Makes an APA-style table of tests and corrections for publication bias
(small-study effects) in a meta-analysis fit with
[`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html),
as computed by metafor.

## Usage

``` r
apa_publication_bias(
  fit,
  methods = c("egger", "rank", "trimfill"),
  transform = TRUE
)
```

## Arguments

- fit:

  a meta-analysis fit with
  [`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)
  (without moderators)

- methods:

  methods to display, in order: any of `"egger"`, `"rank"`,
  `"trimfill"`, and `"fsn"`

- transform:

  if `TRUE` (default), estimates computed on a transformed scale are
  shown on their original scale, as in
  [`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)

## Value

`apa7_tbl` (tibble)

## Details

The methods (rows) are:

- `"egger"`: Egger's regression test
  ([`metafor::regtest()`](https://wviechtb.github.io/metafor/reference/regtest.html)),
  a meta-regression of the effect sizes on their standard errors. The
  test statistic is the test of the standard error's coefficient, and
  the adjusted estimate is the limit estimate (the predicted effect as
  the standard error approaches zero, as in PET).

- `"rank"`: Begg and Mazumdar's rank correlation test
  ([`metafor::ranktest()`](https://wviechtb.github.io/metafor/reference/ranktest.html)),
  Kendall's \\\tau\\ between the standardized effect sizes and their
  variances.

- `"trimfill"`: the trim-and-fill method
  ([`metafor::trimfill()`](https://wviechtb.github.io/metafor/reference/trimfill.html)),
  which imputes the studies (\\k_0\\) that seem to be missing from one
  side of the funnel plot and gives the pooled estimate with them
  included.

- `"fsn"`: the fail-safe *N*
  ([`metafor::fsn()`](https://wviechtb.github.io/metafor/reference/fsn.html)),
  the number of additional studies with an average effect of zero that
  would make the pooled estimate nonsignificant. It is widely criticized
  and is not shown by default.

Adjusted estimates are shown on the same scale as in
[`apa_parameters()`](https://wjschne.github.io/apa7/reference/apa_parameters.md)
(e.g., risk ratios are exponentiated, and Fisher's *z* values are
converted back to correlations), and
[`apa_note()`](https://wjschne.github.io/apa7/reference/apa_note.md)
gives the unadjusted pooled estimate for comparison and describes each
method.

These methods are available for models fit with
[`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html),
not for multilevel models
([`metafor::rma.mv()`](https://wviechtb.github.io/metafor/reference/rma.mv.html)),
for which the standard error (or sampling variance) can be added as a
moderator instead.

## References

Begg, C. B., & Mazumdar, M. (1994). Operating characteristics of a rank
correlation test for publication bias. *Biometrics, 50*(4), 1088–1101.
[doi:10.2307/2533446](https://doi.org/10.2307/2533446)

Duval, S., & Tweedie, R. (2000). Trim and fill: A simple
funnel-plot-based method of testing and adjusting for publication bias
in meta-analysis. *Biometrics, 56*(2), 455–463.
[doi:10.1111/j.0006-341X.2000.00455.x](https://doi.org/10.1111/j.0006-341X.2000.00455.x)

Egger, M., Davey Smith, G., Schneider, M., & Minder, C. (1997). Bias in
meta-analysis detected by a simple, graphical test. *BMJ, 315*(7109),
629–634.
[doi:10.1136/bmj.315.7109.629](https://doi.org/10.1136/bmj.315.7109.629)

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
  apa_publication_bias(fit_meta)
}
#> # A tibble: 3 × 6
#>   Method                  Statistic Value `*p*` `Adjusted *RR*` `95% CI`      
#>   <chr>                   <chr>     <chr> <chr> <chr>           <chr>         
#> 1 Egger's regression test *z*       −0.80 ".42" "0.60"          "[0.33, 1.10]"
#> 2 Rank correlation test   *&tau;*     .03 ".95" ""              ""            
#> 3 Trim and fill           *k*~0~     1    ""    "0.52"          "[0.37, 0.74]"
# }
```
