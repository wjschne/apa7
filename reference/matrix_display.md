# Display latex matrices

Display latex matrices

## Usage

``` r
matrix_display(x, bracket = c("[", "(", "|", ""), include_names = TRUE)
```

## Arguments

- x:

  a matrix

- bracket:

  type of bracket: bmatrix, Bmatrix, pmatrix, vmatrix, Vmatrix, , matrix

## Value

character

## Examples

``` r
matrix_display(diag(2))
#> [1] "\\begin{array}{c c}\n& \\begin{array} {@{} c c @{}}\n    \n  \\end{array}\\\\\n\\begin{array}{c}\n  \n\\end{array}\\hspace{-1em} &\n\\left[\n \\begin{array} {@{} c c @{}}\n    1 & 0\\\\\n    0 & 1\n  \\end{array}\\right]\n\\end{array}"
#> attr(,"class")
#> [1] "latex"     "character"
```
