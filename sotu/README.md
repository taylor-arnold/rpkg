[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/sotu)](https://cran.r-project.org/package=sotu) ![Downloads](http://cranlogs.r-pkg.org/badges/sotu) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/statsmaths/sotu?branch=master&svg=true)](https://ci.appveyor.com/project/statsmaths/sotu) [![Travis-CI Build Status](https://travis-ci.org/statsmaths/sotu.svg?branch=master)](https://travis-ci.org/statsmaths/sotu)


## Installation

The **sotu** package provides the raw text of every State of
the Union Address through 2016. To install, either load from
CRAN:
```{r}
install.packages("sotu")
```
Or you may grab the development version using devtools:
```{r}
devtools::install_github("statsmaths/sotu")
```
It is not expected that the package will change very frequently,
though we will occasionally update as new speeches are given.

## Basic usage

The package contains just two datasets and one function. The
dataset `sotu_meta` is a data frame with one row for each
state of the union and the dataset. The dataset `sotu_text`
is a character vector with one address in each element. Rows
in the former match with elements of the latter. Speeches
are ordered temporally.

There is also a function `sotu_dir` that writes all of the
files to a location on disk, with one address per file,
and returns a vector of files pointing to these files.
For example, the following saves all of the files to disk
and then loads the 200th back into R:
```{r}
fp <- sotu_dir()
txt <- readLines(fp[200])
txt
```
The primary purpose of this is to test or demo text analysis
functions that read files from disk rather than from memory.

For a more detailed example of how this package can be used,
see the vignette for the package cleanNLP:
[Exploring the State of the Union Addresses](https://cran.r-project.org/web/packages/cleanNLP/).

## Note

Please note that this project is released with a
[Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.


