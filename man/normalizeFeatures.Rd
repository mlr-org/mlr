\name{normalizeFeatures}
\alias{normalizeFeatures}
\title{Normalize features}
\usage{
normalizeFeatures(obj, target = character(0L), method = "standardize",
  exclude = character(0L), range = c(0, 1))
}
\arguments{
  \item{obj}{[\code{data.frame} |
  \code{\link{SupervisedTask}}]\cr Input data.}

  \item{target}{[\code{character(1)}]\cr Name of the
  column(s) specifying the response.  Only used when
  \code{obj} is a data.frame, otherwise ignored.}

  \item{method}{[\code{character(1)}]\cr Normalizing
  method.\cr Available are:\cr \dQuote{center}: centering
  of each feature\cr \dQuote{scale}: scaling of each
  feature\cr \dQuote{standardize}: centering and scaling\cr
  \dQuote{range}: Scale the data to a given range.\cr}

  \item{exclude}{[\code{character}]\cr Names of the columns
  to exclude.  The target does not have to be included
  here.  Default is none.}

  \item{range}{[\code{numeric(2)}]\cr Range the features
  should be scaled to. Default is \code{c(0,1)}.}
}
\value{
[\code{data.frame} | \code{\link{SupervisedTask}}]. Same type as \code{obj}.
}
\description{
Normalize features by different methods. Internally \code{\link{normalize}} is used.
Non numerical features will be left untouched and passed to the result.
}
\seealso{
\code{\link{normalize}}
}

