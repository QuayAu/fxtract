<img align="right" src="https://raw.githubusercontent.com/quayau/fxtract/master/man/figures/hexagon.svg?sanitize=true" width="125px">

[![Build Status](https://travis-ci.org/QuayAu/fxtract.svg?branch=master)](https://travis-ci.org/QuayAu/fxtract)
[![codecov](https://codecov.io/gh/QuayAu/fxtract/branch/master/graph/badge.svg)](https://codecov.io/gh/QuayAu/fxtract)

# fxtract [working title]  
This is work in progress!
[Tutorial](https://quayau.github.io/fxtract/docs/index.html)

fxtract helps you to extract user-defined features from timestamped data.

![Image description](man/figures/fxtract_main.svg)

# Installation
For the development version, use [devtools](https://cran.r-project.org/package=devtools):
```{R}
devtools::install_github("QuayAu/fxtract")
```
## Introduction

## Features

* Extracting features from timeseries data of many different users/IDs/etc.
* Timestamp to date & time converters. 
    * This is useful for filtering datasets, e.g. if one is interested in filtering the morning hours from a dataset, or distinguishing working days from weekends.
* divideDataIntoIntervals
* sliding window function
