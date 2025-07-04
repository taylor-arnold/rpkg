% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funs.R
\name{hdir_init_pipeline}
\alias{hdir_init_pipeline}
\title{Initialize the Pipeline of Annotations}
\usage{
hdir_init_pipeline(annotations = c("object", "face", "pose", "embed"))
}
\arguments{
\item{annotations}{a vector of annotations to run. Possible
options are "object", "face", "pose", and
"embed".}
}
\value{
nothing
}
\description{
Sets up the annotations and makes sure that all of the 
models are setup and working.
}
