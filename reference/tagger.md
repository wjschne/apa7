# Surrounds text with tags unless empty

Surrounds text with tags unless empty

## Usage

``` r
tagger(x, tag = "<span>", right_tag = gsub("^<", "</", tag))

bold_md(x)

italic_md(x)

superscript_md(x)

subscript_md(x)

header_md(x, level = 1)
```

## Arguments

- x:

  character vector

- tag:

  opening tag, e.g., `<span>`

- right_tag:

  closing tag, e.g., `</span>`. Defaults to the same value as the
  opening tag.

- level:

  heading level

## Value

character vector

## Examples

``` r
x <- c("hello", "", NA)
tagger(x, "<span>")
#> [1] "<span>hello</span>" ""                   NA                  
bold_md(x)
#> [1] "**hello**" ""          NA         
italic_md(x)
#> [1] "*hello*" ""        NA       
superscript_md(x)
#> [1] "^hello^" ""        NA       
subscript_md(x)
#> [1] "~hello~" ""        NA       
header_md("Level 1")
#> [1] "# Level 1"
header_md("Level 2", 2)
#> [1] "## Level 2"
```
