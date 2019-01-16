
<img align="right" src="https://raw.githubusercontent.com/quayau/fxtract/master/man/figures/hexagon.svg?sanitize=true" width="125px">

[![Build Status](https://travis-ci.org/QuayAu/fxtract.svg?branch=master)](https://travis-ci.org/QuayAu/fxtract) [![codecov](https://codecov.io/gh/QuayAu/fxtract/branch/master/graph/badge.svg)](https://codecov.io/gh/QuayAu/fxtract) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/QuayAu/fxtract?branch=master&svg=true)](https://ci.appveyor.com/project/QuayAu/fxtract)

fxtract
=======

See the [tutorial](https://quayau.github.io/fxtract/docs/index.html) for an introduction into the package.

Introduction
============

`fxtract` helps you to extract user-defined features from raw data. For example, if you want to calculate features (e.g. mean or sd of some columns) out of timeseries data of many IDs: ![](man/figures/fxtract_main.svg) So, if you have different datasets of many IDs and you want to calculate features (covariates) for each ID out of these datasets, then `fxtract` might be for you.

This package utilizes the functionality of the [batchtools](https://mllg.github.io/batchtools/articles/batchtools.html)-package while keeping an easy to access API. The user only needs to define functions which have a dataset as input and named vector with the desired features as output. The whole data wrangling (calculating the features for each ID, possibly in parallel) is handled by `fxtract`. This package works for very large datasets with many different IDs and the main functionality is written in [R6](https://r6.r-lib.org/articles/Introduction.html). Some helpful functions for preprocessing (timestamped) data are also supplied.

### Why don't just use `dplyr`?

At first glance it looks like we just rewrote the `summarize()` functionality of `dplyr`. For small datasets and few (easy to calculate) features, using `fxtract` may indeed be a little overkill (and slower too).

However, this package was especially designed for projects with large datasets, many IDs, and many different feature functions. Once your dataset (with all IDs) becomes too big for memory, or if some feature functions fail on some IDs, using our package can save you many lines of code.

Installation
============

For the development version, use [devtools](https://cran.r-project.org/package=devtools):

``` r
devtools::install_github("QuayAu/fxtract")
```

Usage
=====

``` r
library(fxtract)

# user-defined function:
fun = function(data) {
  c(mean_sepal_length = mean(data$Sepal.Length),
    sd_sepal_length = sd(data$Sepal.Length))
}

# R6 object:
my_project = Project$new("my_project")
my_project$add_data(iris, group_by = "Species")
my_project$add_feature(fun)
my_project$calc_features()
my_project$results
```

    ##      Species mean_sepal_length sd_sepal_length
    ## 1     setosa             5.006       0.3524897
    ## 2 versicolor             5.936       0.5161711
    ## 3  virginica             6.588       0.6358796

Features
========

-   Unit-tested functions.
-   Extracting features from raw data of many different users/IDs/etc:
    -   Either with the function `calc_feature`.
        -   Quick and easy calculation of features just by defining a function on the dataset which has a named vector as output.
        -   Suitable for small projects.
    -   Or with the R6 Class `project`.
        -   For larger projects (many participants, lots of data, many features), it is highly advised to use the R6 class and its methods.
        -   Advantages:
            -   No more code bloat thanks to R6.
            -   Very large datasets are supported, since data is only read into RAM when needed. Minimum requirement: Datasets for each participant individually must be small enough to be read into memory.
            -   Internally, [batchtools](https://mllg.github.io/batchtools/articles/batchtools.html) is used for parallelization and error handling.
            -   Features will be calculated for each participant individually.
            -   If one feature on one participant throws an error, this will not stop the whole process (like in a traditional R script). A log file with the error message is created (by batchtools), and the remaining features will still be calculated.
            -   Individual features can be deleted or updated easily.
            -   Calculation of features can be done in parallel and the process can be monitored. It is also possible to stop and return the calculation at a later time.
            -   Results can be easily collected in one final dataframe.
-   Timestamp to date & time converters:
    -   This is useful for filtering datasets, e.g. if one is interested in filtering the morning hours from a dataset, or distinguishing working days from weekends.
