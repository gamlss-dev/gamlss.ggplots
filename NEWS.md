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
  
- the package vignette can be found in <https://mstasinopoulos.github.io/Functions-from-packages/.>    