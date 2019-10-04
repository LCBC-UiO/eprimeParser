
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LCBC eprimeParser <img src="man/figures/hex.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
**CircleCI build:** [![CircleCI build
status](https://circleci.com/gh/LCBC-UiO/eprimeParser.svg?style=svg&circle-token=95b5ef8cfafcca1062c91538ba2ec75dbeb199a2)](https://circleci.com/gh/LCBC-UiO/eprimeParser)
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
the following data: - The MOAS - The raw experiment data - The location
for output

# Old description by James

# Original description by James Michael Roe, before package conversion

This Hard Disk should be exclusively used for retrieving data from the
computerised Eprime tasks we run here at LCBC.

To automate this process, the directory requires a set structure

The only files and folders here should be:

## — 01-COPYDIR

``` 
        -- TestRoom1
        -- TestRoom2
        -- Laptop
```

## — 02-SCRIPTS

``` 
        -- 02-find_eprime_files.sh
        -- 03-LCBC_eprime_parser.R
        -- 04-LCBC_error_checker.R
```

## — MOAS.Rdata

(replace with newest MOAS, will be overwritten by MOAS\_safe.Rdata)

## — README

(Pre-existing) \#\# — $task \#\# — $task… (Created by script)

# Steps:

**1) Copy a directory into 01-COPYDIR/$location-copied-from**

  - It does not matter how high up the directory is in the tree, so long
    as it contains a subdirectory at some point containing .edat and
    .txt files created by Eprime

**2) open 02-find\_eprime\_files.sh**

  - change $task as necessary
  - change copydir to correspond with *location-copied-from* (NB\! upper
    directory in tree should be *location* or informative in some way)
  - type *" 02-find\_eprime\_files.sh "* into the terminal on a
    unix-based system

**3) open 03-LCBC\_eprime\_parser.R**

  - change path/task as needed
  - “source” script within Rstudio to run OR type *" Rscript
    03-LCBC\_eprime\_parser.R "* in terminal

(*Errors are produced if saved ID does not match with MOAS ID on
Test\_Date. Thus, for a file to “process without error”: ID and test
date corresponds with MOAS ID and Test\_Date*)

**4) open 04-LCBC\_error\_checker.R** - change path/task as needed -
“source” script within Rstudio to run OR type *"
04-LCBC\_error\_checker.R "* in terminal (though Rstudio is recommended
here). This will run an interactive program that is dependent on user
input to correct the processing errors.

## ————– DIRECTORY STRUCTURE PER TASK ————–

**–\> 1-ID\_dataframes** data .csv file per subID saved here (see
COMPLETE.log)

**–\> 2-COMPLETE\_etxt** etxt files that process without error moved
here

**–\> 3-COMPLETE\_edat** edat files that match filename with processed
etxt file moved here

**–\> 4-ERROR\_check** etxt files and linked edats that do not process
successfully moved to new dir here (see ERROR-check.log)

**–\> 5-COPY\_etxt** etxt files to process

**–\> 6-COPY\_edat** edat files yet to be linked

**–\> 7-FATAL** etxt files/edats with irrecoverable data loss (see
FATAL.log)

**–\> 8-LOGS** log per etxt

**ALL\_FILES / COMPLETE / ERROR-check / FATAL / SESSION.log
————————————————————–(full script output)**

Questions to James
