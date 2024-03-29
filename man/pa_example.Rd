% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{pa_example}
\alias{pa_example}
\title{Sample dataset pa_example}
\format{
An object of class \code{data.frame} with 100 rows and 4 columns.
}
\usage{
pa_example
}
\description{
A sample dataset for fitting a path analysis model.
}
\details{
Four variables (x1 to x4), 100 cases.

Sample model to fit (in \link[lavaan:model.syntax]{lavaan::model.syntax} notation)

\if{html}{\out{<div class="sourceCode">}}\preformatted{mod <-
 'x1 ~~ x2
  x3 ~  x1 + x2
  x4 ~  x1 + x3
 '
}\if{html}{\out{</div>}}
}
\keyword{datasets}
