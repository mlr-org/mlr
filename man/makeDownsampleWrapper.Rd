\name{makeDownsampleWrapper}
\alias{makeDownsampleWrapper}
\title{Fuse learner with simple downsampling (subsampling).}
\usage{
makeDownsampleWrapper(learner, dw.perc = 1, dw.stratify = FALSE)
}
\arguments{
  \item{learner}{[\code{\link{Learner}} |
  \code{character(1)}]\cr The learner.  If you pass a
  string the learner will be created via
  \code{\link{makeLearner}}.}

  \item{dw.perc}{[\code{numeric(1)}]\cr See
  \code{\link{downsample}}.}

  \item{dw.stratify}{[\code{logical(1)}]\cr See
  \code{\link{downsample}}.}
}
\value{
[\code{\link{Learner}}].
}
\description{
Creates a learner object, which can be
used like any other learner object.
It will only be trained on a subset of the original data to save computational time.
}
\seealso{
Other downsample: \code{\link{downsample}}
}

