# PvalVis

<!-- badges: start -->
<!-- badges: end -->

The goal of PvalVis is to give a quick and easy visualization tool for p-values in the setting of basic hypothesis tests. Especially for beginners the visualization of the p-values might improve the understanding not only of the p-value but also of the assumed distribution for the specific test.

The following underlying distributions are currently available:

 * Normal distributions (norm_pval)
 * student-t distributions (t_pval)
 * F distributions (F_pval)
 * $\chi^2$ distributions (chisq_pval)
 
For more inforation on each of the functions visit their individual help pages. For example:

``` r
library(PvalVis)
?norm_pval
```

## Installation

You can install the released version of PvalVis from Github in R with the following line of code:

``` r
devtools::install_github("EmanuelSommer/PvalVis")
```

## Example

These are basic examples which show you how the functions display and highlight the p-values and the assumed density:

``` r
library(PvalVis)
norm_pval(z_value=2.3,mean=0,sd=2,direction="both")
F_pval(F_value = 3, df1 = 5, df2 = 30)
```




