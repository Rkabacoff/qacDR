# Dimension Reduction

![](image.jpg)

The `qacDR` package provides functions for principal components and common factor analysis. Some cluster analysis tools are also provided.

Based on the William Revelle’s comprehensive [psych](https://cran.r-project.org/web/packages/psych/index.html) package, the `qacDR` package provides simplified input, intuitive output, and easily interpretable graphs, making these techniques more accessible to data analysts new to these analyses.

To download this package use the following code:

    if(!require(remotes)){
       install.packages("remotes")
    }
    remotes::install_github("rkabacoff/qacDR")
