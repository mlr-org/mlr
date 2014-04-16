\name{filterFeatures}
\alias{filterFeatures}
\title{Returns the filtered features.}
\usage{
filterFeatures(feat.importance, n, threshold = 0)
}
\arguments{
  \item{feat.importance}{[\code{numeric}]\cr Result of
  \code{\link{filterFeatures}}.}

  \item{n}{[\code{integer(1)}]\cr Number of features
  ordered by the information value to select.}

  \item{threshold}{[\code{numeric(1)}]\cr Information value
  as to be greater then the threshold. Default is 0.}
}
\value{
[\code{character}]
}
\description{
Returns the selected Features according to their
importance. Features with an importance value of 0 will be
left out regardeless of the given \code{n}.
}

