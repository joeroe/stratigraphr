# stratigraphr <img src="man/figures/logo.svg" align="right" style="float: right; height: 180px; margin-left: 1em">

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN status](https://www.r-pkg.org/badges/version/stratigraphr)](https://CRAN.R-project.org/package=stratigraphr)
[![R-CMD-check](https://github.com/joeroe/stratigraphr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joeroe/stratigraphr/actions/workflows/R-CMD-check.yaml)
[![Test coverage](https://codecov.io/gh/joeroe/stratigraphr/graph/badge.svg)](https://app.codecov.io/gh/joeroe/stratigraphr)
<!-- badges: end -->

stratigraphr is a tidy framework for working with archaeological stratigraphy and chronology in R.
It includes tools for reading, analysing, and visualising stratigraphies (Harris matrices) and sequences as directed graphs;
helper functions for using radiocarbon dates in a tidy data analysis; 
and an R interface to OxCal's Chronological Query Language (CQL).

## Installation

You can install the development version of stratigraphr from GitHub:

```r
# install.packages("devtools")
devtools::install_github("joeroe/stratigraphr")
```

Please note that this package is in an early stage of development.
It is functional, but many features are missing and future versions are likely to contain breaking changes.

## Usage

* Graph-bases stratigraphic analysis: see `vignette("stratigraph")`.
* Tidy analysis of radiocarbon dates: see `vignette("tidy_radiocarbon")`.
* Chronological query language: see `vignette("cql")`.
