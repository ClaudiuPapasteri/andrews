
<!-- README.md is generated from README.Rmd. Please edit that file -->

# andrews

<!-- badges: start -->
<!-- badges: end -->

Compute Andrews curves

### Description:

Compute data for Andrews plot, for visualizing clusters of multivariate
data with ggplot2.

### Usage

``` r
library(ggplot2)

df <- andrews(iris, "Species")

ggplot(df, aes(x = t, y = values, color = class_column, group = sample)) +
  geom_line() +
  scale_x_continuous(n.breaks = 7)
```

![](README_files/figure-gfm/cars-1.png)<!-- -->
