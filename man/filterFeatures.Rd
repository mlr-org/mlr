\name{filterFeatures}
\alias{filterFeatures}
\title{Filter features by using a numerical importance criterion.
Calculates numerical importance values for all features.
Thresholding of these values can be used to select \dQuote{useful} features.
Look at package FSelector for details on the filter algorithms.}
\usage{
  filterFeatures(task, method = "random.forest.importance")
}
\arguments{
  \item{task}{[\code{\link{SupervisedTask}}]\cr The task.}

  \item{method}{[\code{character(1)}]\cr Filter method.
  Available are: \dQuote{linear.correlation},
  \dQuote{rank.correlation}, \dQuote{information.gain},
  \dQuote{gain.ratio}, \dQuote{symmetrical.uncertainty},
  \dQuote{chi.squared}, \dQuote{random.forest.importance},
  \dQuote{relief}, \dQuote{oneR} Default is
  \dQuote{random.forest.importance}.}
}
\value{
  [\code{numeric}]. A named numeric vector that contains an
  importance value for each feature.
}
\description{
  Filter features by using a numerical importance
  criterion. Calculates numerical importance values for all
  features. Thresholding of these values can be used to
  select \dQuote{useful} features. Look at package
  FSelector for details on the filter algorithms.
}

