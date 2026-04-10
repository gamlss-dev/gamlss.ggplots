---
editor_options: 
  markdown: 
    wrap: 72
---

# Introduction

The package `gamlss.ggplots` is a `R` package to allow the use of
several plotting functions for objects created by the the packages `gamlss` and `gamlss2`. It uses the plotting functions of the package`ggplot2`. Both  packages `gamlss` and `gamlss2`  are implementations of the
Generalised Additive Models for Location, Scale and Shape (GAMLSS) of
Rigby and Stasinopoulos (2005), Appl. Statist., 54, pp. 507-554).

There are three book available for more information about GAMLSS;

1)  "Flexible Regression and Smoothing: Using GAMLSS in R" explaining
    how the models can be used in R.

2)  "Distributions for modeling location, scale and shape: Using GAMLSS
    in R" explaining the explicit and generated distributions available
    in the package gamlss.dist

3)  "Generized Additive Models for Location Scale and Shape: A
    distributional regression approach with applications" explaining the
    different method for fitting GAMLSS i.e. penalised Likelihood,
    Bayesian and Boosting.

More more information about books and papers related to GAMLSS can be
found in <https://www.gamlss.com/>.

The GitHub repository is now hosted under the new `gamlss-dev`
organization: <https://github.com/gamlss-dev/gamlss.ggplots/>.


# Version 2.1-12

-   the package was withdraw from CRAN, but now version 2.1.12
    reintroduce it.

# Version 2.1-13

-   all `resid_` functions are now compatible with the objecrs creates
    with the `gamlss2()` function

-   `family_cdf` a bug was corrected when plotting only one curve

-   `family.pdf` the `aes_string()` was removed (it is depreciated from
    `ggplot2`)

# Version 2.1-14

-   `fitted_pdf` and `fitted_cdf` are working with `gamlss2` object but
    the binomial responses needed checking (not fixed yet)

# Version 2.1-15

-   Most of the function are working now with `gamlss2` object (need
    checking)

# Version 2.1-16

- The function `model_TD()` and `model_TD_lollipop()` is introduced but not in the help file yet  


# Version 2.1-17

- The function `pe_pdf_fv()` is introduced but not in the help file yet  


# Version 2.1-18

- the function `fitted_pdf_legend()` is introduced but not exported in the NAMSPACE. To use it use 
  `gamlss.ggplots:::fitted_pdf_legend()`.
  
  
# Version 2.1-26

Function for helping modelling for more than one variables as the response are added;

- function `xy_median()` to calculate medians in 2-dimensions.

- functions `xy_scatter()` and `xy_pit_scatter()'  for plotting the original or PIT transform values of two
continuous variable

- functions `xy_hist()` and `xy_pit_hist()' for plotting histograms in two dimensions

- functions `xy_density()` and `xy_density_plain()' and  `xy_pit_density()'for plotting kernal density estimators in two dimensions

- functions `xy_ECDF_hist()` and `xy_ECDF_contour(x,y)' for plotting empirical csdf's histograms and contour plots in two dimensions


# Version 2.1-27



- function `bivar_fun()`:  for plotting any 2-dimensional  function $f(x,y)$.

- functions `bivar_pdf()`:   for plotting any 2-dimensional pdf function $f(x,y)$.




  
## NOTE: 

the package vignette can be found in <https://mstasinopoulos.github.io/Functions-from-packages/.>    