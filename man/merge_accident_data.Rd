% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_accident_data.R
\name{merge_accident_data}
\alias{merge_accident_data}
\alias{merge_accident_data.accident_data}
\alias{merge_accident_data.list}
\title{Merge accident data}
\usage{
merge_accident_data(...)

\method{merge_accident_data}{accident_data}(...)

\method{merge_accident_data}{list}(data_list, ...)
}
\arguments{
\item{...}{\code{accident_data} objects to merge.}

\item{data_list}{A list of \code{accident_data} objects to merge.}
}
\value{
A merged \code{accident_data} object.
}
\description{
\code{merge_accident_data()} merges multiple \code{accident_data} objects into a
single \code{accident_data} object.
}
