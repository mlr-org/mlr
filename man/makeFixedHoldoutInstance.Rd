\name{makeFixedHoldoutInstance}
\alias{makeFixedHoldoutInstance}
\title{Generate a fixed holdout instance for resampling.}
\usage{
makeFixedHoldoutInstance(train.inds, test.inds, size)
}
\arguments{
  \item{train.inds}{[\code{integer}]\cr Indices for
  training set.}

  \item{test.inds}{[\code{integer}]\cr Indices for test
  set.}

  \item{size}{[\code{integer(1)}]\cr Size of the data set
  to resample.}
}
\value{
[\code{\link{ResampleInstance}}].
}
\description{
Generate a fixed holdout instance for resampling.
}

