# R Packages

This repository contains a collection of R packages that I have developed, spanning natural language processing, statistical analysis, data visualization, and text analysis. I am putting them into a single repository because I am not doing a lot of active work on them but still want to keep them supported and updated. A single repository makes it easier for me to track issues and for others to find the set of packages that are actively supported.

## Packages

### cleanNLP

**A Tidy Data Model for Natural Language Processing**

Provides fast tools for converting textual corpora into normalized tables. Supports multiple backends including 'udpipe' (no external dependencies) and Python backends with 'spaCy'. Features include tokenization, part of speech tagging, named entity recognition, and dependency parsing.

### dgof

**Discrete Goodness-of-Fit Tests**

Enhanced goodness-of-fit tests for discrete distributions, extending R's `ks.test()` function with features necessary for one-sample tests with hypothesized discrete distributions. Also includes `cvm.test()` for Cramer-von Mises tests.

### ggimg

**Graphics Layers for Plotting Image Data with 'ggplot2'**

Extends ggplot2 with new geometries (`geom_rect_img` and `geom_point_img`) for displaying images as layers within the Grammar of Graphics framework. Supports local files, URLs, and raster data.

### ggmaptile

**Add Map Images from a Tile Server with ggplot2**

Provides functions to grab, store, and display map tiles from tile servers within ggplot2 objects, enabling easy integration of map backgrounds into spatial visualizations.

### hdir

**Functions for the Humanities Data in R Book**

Companion package for "Humanities Data in R (2e)" book, providing helper functions that simplify code examples while maintaining educational transparency for R learners working with humanities data.

### leaderCluster

**Leader Clustering Algorithm**

Implements Hartigan's leader clustering algorithm, which clusters data points based on a specified radius rather than a predetermined number of clusters. Supports various distance metrics including spatial distances using the Haversine formula.

### sotu

**United States Presidential State of the Union Addresses**

Text corpus containing all U.S. Presidential State of the Union addresses through 2016, designed for text analysis examples and research. Includes comprehensive metadata such as year, president, party, and format.

## Installation

You can install any of these packages directly from GitHub using the `remotes` package:

```r
# Install remotes if you haven't already
install.packages("remotes")

# Install individual packages
remotes::install_github("taylor-arnold/rpkg", subdir = "cleanNLP")
remotes::install_github("taylor-arnold/rpkg", subdir = "dgof")
remotes::install_github("taylor-arnold/rpkg", subdir = "ggimg")
remotes::install_github("taylor-arnold/rpkg", subdir = "ggmaptile")
remotes::install_github("taylor-arnold/rpkg", subdir = "hdir")
remotes::install_github("taylor-arnold/rpkg", subdir = "leaderCluster")
remotes::install_github("taylor-arnold/rpkg", subdir = "sotu")
```

Many of these are also available directly on CRAN.

## Citations

If you use any of the following packages in your research, please consider citing the relevant publications:

**cleanNLP:**

```
Arnold, Taylor (2017). "A Tidy Data Model for Natural Language Processing using cleanNLP." *The R Journal*, 9(2), 1-20.
```

**dgof:**

```
Arnold, Taylor, John W. Emerson (2011). "Nonparametric Goodness-of-Fit Tests for Discrete Null Distributions." *The R Journal*, 3(2), 34-39.
```

**hdir**, **ggimg**, and **ggmaptile:**

Arnold, Taylor, Lauren Tilton (2024). *Humanities Data in R: Exploring Networks, Geospatial Data, Images, and Text (2nd)*, Springer.


