% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot2-helpers.R
\name{reverselog_trans}
\alias{reverselog_trans}
\title{Transform axis to reversed log scale}
\source{
https://stackoverflow.com/a/11054781/1198125
}
\usage{
reverselog_trans(base = exp(1))
}
\arguments{
\item{base}{base for the log transformation; default is to use natural
logarithm (base exp(1)).}
}
\description{
Transform axis to reversed log scale.
}
\seealso{
\code{\link[scales]{trans_new}},\code{\link[scales]{breaks_log}}
}
