# lemonutil

 R package of utility functions
 
# How to Install

 To install this package, you can use devtools.
 
 In the R console, with devtools installed, run the following line of code:
 
 ```devtools::install_github('lemonlinn/lemonutil')```
 
 This should download the lemonutil package as well as its dependencies.
 
# Overview

 To view the functions contained in this package, run the following code:
 
 ```
 library('lemonutil')
 
 ls('package:lemonutil')
 ```
 
 Here's a brief overview of the most often used functions:
 
 * `crosstable_ext`: Generates a flextable for a cross tabulation, including the chi-square statistic and p-value
 * `unitable`: Generates a flextable for the distribution of a single variable
 * `flex2df`: Converts a flextable produced by either `lemonutil::unitable` or `lemonutil::crosstable` into a data.frame object
