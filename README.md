# quickresults_dragdrop

Install with vignettes:

``` r
devtools::install_github("odb01/quickresults_dragdrop", build_vignettes = TRUE)
```

Example code

``` r
library(quickresults)
predictors <- c("disp", "hp", "drat", "cyl")
M1 <- c("wt", "qsec")
example <- quickresults::run_linreg(imp_mtcars, predictors, "mpg", M1)
```
