\name{makeSMOTEWrapper}
\alias{makeSMOTEWrapper}
\title{Fuse learner with SMOTE oversampling for imbalancy correction in binary classification.}
\usage{
makeSMOTEWrapper(learner, sw.rate, sw.nn = 5L)
}
\arguments{
  \item{learner}{[\code{\link{Learner}} |
  \code{character(1)}]\cr The learner.  If you pass a
  string the learner will be created via
  \code{\link{makeLearner}}.}

  \item{sw.rate}{[\code{numeric(1)}]\cr Factor to
  oversample the smaller class. Must be between 1 and
  \code{Inf}, where 1 means no oversampling and 2 would
  mean doubling the class size.}

  \item{sw.nn}{[\code{integer(1)}]\cr Number of nearest
  neighbors to consider.  Default is 5.}
}
\value{
[\code{\link{Learner}}].
}
\description{
Creates a learner object, which can be
used like any other learner object.
Internally uses \code{\link{smote}} before every model fit.

Note that observation weights do not influence the sampling and are simply passed
down to the next learner.
}

