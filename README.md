
<!-- README.md is generated from README.Rmd. Please edit that file -->
hoffr: higher order functions for r
===================================

``` r

library(hoffr)

meanf <- hof(mean)
mean_noNA <- meanf(na.rm = TRUE)
mean_NA <- meanf(na.rm = FALSE)
mean_trim <- meanf(trim = 0.25)

mean_noNA(c(1, NA, 3))
#> [1] 2
mean_NA(c(1, NA, 3))
#> [1] NA
mean_trim(c(-3.5, 0, 2, 100))
#> [1] 1

mean_trim_noNA <- mean_trim(na.rm = TRUE)
mean_trim_noNA(c(-3.5, 0, 2, NA, 100))
#> [1] 1
```

Installation
------------

You can install hoffr from github with:

``` r
# install.packages("devtools")
devtools::install_github("hughjonesd/hoffr")
```
