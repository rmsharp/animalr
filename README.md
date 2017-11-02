
[![Rdoc](http://www.rdocumentation.org/badges/version/roxygen2)](http://www.rdocumentation.org/packages/roxygen2) [![Build Status](https://travis-ci.org/rmsharp/animalr.svg?branch=master)](https://travis-ci.org/rmsharp/animalr) [![codecov](https://codecov.io/gh/rmsharp/animalr/branch/master/graph/badge.svg)](https://codecov.io/gh/rmsharp/animalr)

<!-- README.md is generated from README.Rmd. Please edit that file -->
animalr
=======

Installation
------------

### Installation from Source

**rmsutilityr** is a local source packages that must be installed prior to installing **animalr**.

For example, a very manual approach is to use the following code, which assumes you have all of the dependencies already installed:

``` r
install_path <- "c:/R Library"
source_path <- "d:Labkey data"
source <- "rmsutilityr.1.0.56.tar.gz"
install.packages(paste0(source_path, "/", source), type = "source", repos = NULL,
                     lib = install_path)
source <- "animalr.0.1.86.tar.gz"
install.packages(paste0(source_path, "/", source), type = "source", repos = NULL,
                     lib = install_path)
```

One or more of these will fail if you do not have the dependencies already installed, but the error message will provide the name(s) of the packages needed. However, as soon as one of the source packages is updated that code no longer work because the filename is wrong.

Assuming you have a version of **rmsutilityr** installed you can simply use the following code, which will find all of the dependencies, install them, locate the most recent source versions of the packages **rmsutilityr**, **animalr**, and **animalr**, find th.

``` r
library(rmsutilityr)
source_names <- c("rmsutilityr", "animalr")
source_path <- "d:Labkey data/"
## dependencies becomes the collection of packages needed to support our
## custom packages
dependencies <- make_package_dependency_list(source_names, source_path)
## We are installing binary packages first and then our local source
dependencies <- remove_these_str(dependencies, expunge = source_names)
install.packages(dependencies, type = "binary",
                 repos = "https://cran.revolutionanalytics.com",
                 lib = lib_path)
update.packages(lib.loc = lib_path,
                repos = "https://cran.revolutionanalytics.com", ask = FALSE)

install_from_source(source_names, source_path, install_path)
```

### Github.com Installation

It is much easier to install directly from [github.com/rmsharp/animalr](https://github.com/rmsharp/animalr) as all of the dependencies are automatically installed.

You can install **animalr** from github with:

``` r
install.packages("devtools")
devtools::install_github("rmsharp/animalr")
```

All missing dependencies should be automatically installed.
