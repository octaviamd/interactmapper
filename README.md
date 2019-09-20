
<!-- README.md is generated from README.Rmd. Please edit that file -->

# interactmapper

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/octaviamd/interactmapper.svg?branch=master)](https://travis-ci.org/octaviamd/interactmapper?branch=master)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/octaviamd/interact-mapper?branch=master&svg=true)](https://ci.appveyor.com/project/octaviamd/interact-mapper)
<!-- badges: end -->

The goal of interactmapper is to make analyzing dimension reduction
plots more intuitive and user-friendly, by allowing users to overlay
features of interest of the dataset onto the plot via color. interactmapper currently offers UMAP and PCA as dimension reduction methods.

## Installation

You can install the released version of interactmapper from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("interactmapper")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("octaviamd/interactmapper")
```

## interact_qual

Here is a walkthrough of how to use the different functions in the interactmapper package. For greater ease, the `iris` dataset will be used to show how the functions can be used. Illustrative gifs showing the kinds of interactive plots that can be generated will be provided soon!

The `interact_qual` function is best suited to represent qualitative features of your data (ex: race/ethnicity of patients, smoker status, treatment groups, cell types, etc...). In the example below, the dimension reduction method `UMAP` is applied to the `iris` dataset, which is plotted and color coded based on `iris$Species`, representing the Species of each iris represented in the dataset. Upon mouse hover, the qualitative feature of interest is presented in a text box, as well as the sample name. 
``` r
library(interactmapper)
#interactmapper::interact_qual(iris[,1:4], iris$Species, "UMAP")
```
<img src="man/figures/interact_qual_iris_plot.png" width="100%" />

 
The plot generated by `interact_qual` also has a helpful side-bar that displays all of the variant options of your qualitative feature of interest, and allows you to select which variant(s) you want represented on your plot. By clicking on the feature option names, you can either show or hide the points associated with those features. In the UMAP plot below, setosa and versicolor have been hidden in order to have a better look at the virginica irises.

<img src="man/figures/interact_qual_select.png" width="100%" />

Here is an example of applying PCA instead of UMAP to the same data, looking at the same feature, `iris$Species`.

``` r
#interactmapper::interact_qual(iris[,1:4], iris$Species, "PCA")
```
<img src="man/figures/interact_qual_pca.png" width="100%" />


## interact_quant

The `interact_quant` function is used to represent features that are quantitative in nature (gene expression, DNA methylation, height, etc...).

``` r

```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

