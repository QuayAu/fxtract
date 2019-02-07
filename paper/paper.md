---
title: 'fxtract: An R Package for Extracting Features of Grouped Data'
tags:
  - R
  - machine learning
  - feature extraction
  - feature engineering
  - big data
authors:
  - name: Quay Au
    orcid: 0000-0002-5252-8902
    affiliation: 1
  - name: Clemens Stachl
    orcid: 0000-0000-0000-0000
    affiliation: 2
  - name: Ramona Schoedel
    orcid: 0000-0000-0000-0000
    affiliation: 2
  - name: Theresa Ullmann
    orcid: 0000-0000-0000-0000
    affiliation: 1
  - name: Andreas Hofheinz
    orcid: 0000-0000-0000-0000
    affiliation: 1
  - name: Bernd Bischl
    orcid: 0000-0001-6002-6980
    affiliation: 1
affiliations:
 - name: Institute of Statistics, Ludwigs-Maximilians-University München
   index: 1
 - name: Department of Psychology, Methods and Assessment, Ludwig-Maximilians-Universität München
   index: 2
date: 07 February 2019
bibliography: paper.bib
---

# Summary

Features which are created with domain-specific knowledge often play an important role in a successful machine learning project.
However, raw data is not always in the right form for machine learning models.
This makes a feature engineering step indispensable (see [@James2013; @Guyon2006]).
``fxtract`` provides an easy to use API to facilitate the extraction of user-defined features from grouped data.

Suppose there are $n$ instances with datasets $D_i$, $i = 1,...,n$. 
These datasets can differ in the number of rows or even in the number of columns.
``fxtract`` helps in the process of extracting $m$ features (covariates) $x_{i, 1}, ..., x_{i, m}$ for each instance $i = 1, ..., n$.

With growing numbers $n$ and $m$, the process of extracting features can become burdonsome very quickly. 
The whole dataset $\{D_1, ..., D_n\}$ could be too big to be read into memory. Or a function $f_j$, which calculates a feature $x_{i, j}$ for each dataset $D_i, i = 1,..., n$ could throw an exception just for one dataset $D_k$, $k \in \{1, ..., n\}$. 

This whole data wrangling process of calculating features for each instance and collecting the features in one dataset is handled by ``fxtract``. Large datasets are supported, since each individual dataset $D_i$ is only read into memory, when needed. If functions throw exceptions on single datasets, calculation on the remaining datasets will still continue while the error messages are made available for debugging purposes. The user only needs to define functions, which calculate the features $x_{1}, ..., x_{m}$ from their data. We do not provide any automated features. This was a deliberate design choice, because we wanted to give the user the freedom to extract their own features.

This package is written in ``R6``[@R6], which means that data, features, and methods (adding data and features, calculation, error handling, monitoring, retrieving results) are tidily kept in one object. Parallelization is possible by using the R-Package ``future``[@future].

# References
