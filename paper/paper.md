---
title: 'fxtract: An R Package for Extracting Features of Grouped Data'
tags:
  - R
  - machine learning
  - feature extraction
  - feature engineering
  - big data
  - repeated measures
authors:
  - name: Quay Au
    orcid: 0000-0002-5252-8902
    affiliation: 1
  - name: Clemens Stachl
    orcid: 0000-0002-4498-3067
    affiliation: 2
  - name: Ramona Schoedel
    orcid: 0000-0001-7275-0626
    affiliation: 2
  - name: Theresa Ullmann
    orcid: 0000-0003-1215-8561
    affiliation: 1
  - name: Andreas Hofheinz
    orcid: 0000-0003-1798-0589
    affiliation: 1
  - name: Bernd Bischl
    orcid: 0000-0001-6002-6980
    affiliation: 1
affiliations:
 - name: Institute of Statistics, Ludwigs-Maximilians-University München
   index: 1
 - name: Department of Psychology, Methods and Assessment, Ludwigs-Maximilians-University München
   index: 2
date: 13 February 2019
bibliography: paper.bib
---

# Summary
Unlike some years ago, researchers and practitioners nowadays have access to large, longitudinal, fine-grained, and distributed datasets. Very often these datasets include timestamp-coded events that are hard to interpret on their own and are difficult to model in the raw form. Examples are system logs, data from sensors, or time series.
Often, these datasets need to be aggregated by groups (e.g. by each system, user [@Hoppe2018; @Stachl2017; @Schoedel2018], or month [@Romilly2005]) in order to utilize them in statistical analyses and to make them understandable for users. However, this process can be difficult and is often prone to errors. Furthermore, with large datasets and little personal computing-power, custom-made aggregation functions often cause memory to overflow which leads to a frustrating user experience. 

One key application of those aggregation functions is in the field of machine learning. In this context, user-defined variables based on domain-specific knowledge, which are called *features* in the machine learning community, play a decisive role [@Domingos2012]. Before modeling, features have to be engineered and extracted, which means that the raw data has to be transformed to an appropriate format. 
At best, these features should be interpretable in themselves in order to make models easier to understand [@molnar].
This indispensable process [@James2013; @Guyon2006] of extracting features from raw data led us to the name ``fxtract``. 
However, this package is not limited to applications in the machine learning context. It can be used for any kind of dataset that needs to be aggregated by groups. 

``fxtract`` provides an easy to use API to facilitate the extraction of user-defined features from grouped data.
Feature calculation for each group and collection of the features in a single dataset is handled by ``fxtract``.
Large datasets are supported, since each individual dataset for each group is only read into memory, when needed. If functions throw exceptions on single datasets, calculation on the remaining datasets will still continue while error messages are made available for debugging purposes. 
The user only needs to define functions, which calculate the features from their data, without worrying about how to apply this function on each group and how to collect the results.

This package is written in ``R6``[@R6], which means that data, features, and methods for adding data and features, calculation, error handling, monitoring and retrieving results, are tidily kept in one object. Parallelization is possible by using the R-Package ``future``[@future].

# References
