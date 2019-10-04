
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LCBC eprimeParser <img src="man/figures/hex.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![CircleCI build
status](https://circleci.com/gh/LCBC-UiO/eprimeParser.svg?style=svg&circle-token=95b5ef8cfafcca1062c91538ba2ec75dbeb199a2)](https://circleci.com/gh/LCBC-UiO/eprimeParser)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/LCBC-UiO/eprimeParser.svg?branch=master)](https://travis-ci.org/LCBC-UiO/eprimeParser)
[![Codecov test
coverage](https://codecov.io/gh/LCBC-UiO/eprimeParser/branch/master/graph/badge.svg)](https://codecov.io/gh/LCBC-UiO/eprimeParser?branch=master)
<!-- badges: end -->

The goal of {eprimeParser} is to find, sort and store parset files from
LCBC’s eprime experiments. The end result should be tab-separated files
with correct ID’s and dates for each participant, stored in folders for
each distinct experiment. The parser will do checks not only on ID’s,
but also on whether those ID’s match the ID’s that should have been
tested on a given day by referring to the Mother-of-all-Spreadsheets
(MOAS) data.

## Installation

You can install the released version of eprimeParser from
[github](https://github.org) with:

``` r
# install.packages("remotes")
remotes::install_packages_github("LCBC-UiO/eprimeParser")
```

# Procedure

The functions in this package require a connection to the LCBC
lagringshotell at the University of Oslo, and that you know the paths to
the following data:  
\- The MOAS  
\- The raw experiment data  
\- The location for output
