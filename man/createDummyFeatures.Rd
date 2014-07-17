\name{createDummyFeatures}
\alias{createDummyFeatures}
\title{Generate dummy variables for factor features.}
\usage{
createDummyFeatures(obj, target = character(0L), method = "1-of-n",
  exclude = character(0L))
}
\arguments{
  \item{obj}{[\code{data.frame} |
  \code{\link{SupervisedTask}}]\cr Input data.}

  \item{target}{[\code{character()}]\cr Name of the
  column(s) specifying the response.  Only used when
  \code{obj} is a data.frame, otherwise ignored.}

  \item{method}{[\code{character(1)}]\cr Available are:\cr
  \dQuote{1-of-n}: For n factor levels there will be n
  dummy variables.\cr \dQuote{reference}: There will be n-1
  dummy variables leaving out the first factor level of
  each variable.\cr}

  \item{exclude}{[\code{character}]\cr Names of the columns
  to exclude.  The target does not have to be included
  here.  Default is none.}
}
\value{
[\code{data.frame} | \code{\link{SupervisedTask}}]. Same type as
  \code{obj}.
}
\description{
Replace all factor features with their dummy variables. Internally \code{\link{model.matrix}} is used.
Non factor features will be left untouched and passed to the result.
}
\seealso{
\code{\link{model.matrix}}
}

