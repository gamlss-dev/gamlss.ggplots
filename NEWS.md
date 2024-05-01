---
editor_options: 
  markdown: 
    wrap: 72
---

# Introduction

The package `gamlss.ggplots` is a `R` package to allow the use of
several plotting functions of the the package `gamlss` to be plotting
using `ggplot2`. The package `gamlss` is an implementation of the
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

- all `resid_` functions are now compatible with the objecrs creates with the 
`gamlss2()` function

- `family_cdf` a bug was corrected when plotting only one curve

- `family.pdf` the `aes_string()` was removed (it is depreciated from `ggplot2`)