# format model performance metrics in APA style

format model performance metrics in APA style

## Usage

``` r
apa_performance(fit, metrics = c("R2", "Sigma"), column_formats = NULL)

# S3 method for class 'lm'
apa_performance(fit, metrics = c("R2", "Sigma"), column_formats = NULL)
```

## Arguments

- fit:

  model fit object

- metrics:

  performance metrics. Default is R2 and Sigma

- column_formats:

  column_formats object to format columns. If NULL, the default
  column_formats is used.

## Value

tibble

## Examples

``` r
lm(mpg ~ cyl + wt, data = mtcars) |>
   apa_performance() |>
   apa_flextable()


.cl-2913596e{}.cl-290d203a{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-290d204e{font-family:'Times New Roman';font-size:7.2pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;vertical-align:super;}.cl-290d204f{font-family:'Times New Roman';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-290d2058{font-family:'Times New Roman';font-size:7.2pt;font-weight:normal;font-style:italic;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;vertical-align:sub;}.cl-290fe086{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-290fe09a{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:8pt;padding-top:8pt;padding-left:3pt;padding-right:3pt;line-height: 1;background-color:transparent;}.cl-290fff80{width:2.875in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-290fff8a{width:3.625in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0.5pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-290fff8b{width:2.875in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-290fff94{width:3.625in;background-color:transparent;vertical-align: top;border-bottom: 0.5pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


R2
```
