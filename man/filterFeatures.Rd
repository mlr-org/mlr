\name{filterFeatures}
\alias{filterFeatures}
\title{Returns the filtered features.}
\usage{
filterFeatures(obj, target, feat.importance, n, threshold = 0)
}
\arguments{
  \item{obj}{[\code{\link{data.frame}} |
  \code{\link{ClassifTask}}]\cr The data set or task.}

  \item{target}{[\code{character}]\cr Name of the column(s)
  specifying the response if you passed a
  \code{data.frame}.  User input is ignored if you pass a
  task and \code{target} is automatically set.  Never
  removed.}

  \item{feat.importance}{[\code{numeric}]\cr Result of
  \code{\link{getFeatureFilterValues}}.}

  \item{n}{[\code{integer(1)}]\cr Number of features
  ordered by the information value to select.}

  \item{threshold}{[\code{numeric(1)}]\cr Information value
  as to be greater then the threshold. Default is 0.}
}
\value{
[\code{data.frame} | \code{\link{ClassifTask}}]. Same type
as \code{obj}.
}
\description{
Returns the selected Features according to their
importance. Features with an importance value lower than
threshold will be left out regardless of the given
\code{n}.
}

