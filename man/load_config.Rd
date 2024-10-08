% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config.R
\name{load_config}
\alias{load_config}
\title{Load configuration from YAML file}
\usage{
load_config(config_path)
}
\arguments{
\item{config_path}{Path to the YAML file containing the configuration.}
}
\description{
This function loads the accident data configuration from a YAML file.
}
\examples{
\dontrun{
load_config("path/to/config.yaml")
}
}
\seealso{
\code{\link[=get_config]{get_config()}} for retrieving the configuration.

\code{\link[=reset_config]{reset_config()}} for resetting the configuration.
to the default values.
}
