\name{makeFeatSelWrapper}
\alias{makeFeatSelWrapper}
\title{Fuse learner with variable selection.}
\usage{
makeFeatSelWrapper(learner, resampling, measures, bit.names, bits.to.features,
  control, show.info = TRUE)
}
\arguments{
  \item{learner}{[\code{\link{Learner}} or string]\cr
  Learning algorithm. See \code{\link{learners}}.}

  \item{resampling}{[\code{\link{ResampleDesc}} |
  \code{\link{ResampleInstance}}]\cr Resampling strategy to
  evaluate feature sets.}

  \item{measures}{[list of \code{\link{Measure}}]\cr
  Performance measures to evaluate. The first measure,
  aggregated by the first aggregation function is optimized
  during feature selection, others are simply evaluated.}

  \item{bit.names}{[\code{character}]\cr Names of bits
  encoding the solutions. Also defines the total number of
  bits in the encoding.  Per default these are the feature
  names of the task.}

  \item{bits.to.features}{[\code{function(x, task)}]\cr
  Function which transforms an integer-0-1 vector into a
  character vector of selected features.  Per default a
  value of 1 in the ith bit selects the ith feature to be
  in the candidate solution.}

  \item{control}{[\code{\link{FeatSelControl}}] Control
  object for search method. Also selects the optimization
  algorithm for feature selection.}

  \item{show.info}{[\code{logical(1)}]\cr Show info message
  after each feature set evaluation?  Default is
  \code{TRUE}.}
}
\value{
[\code{\link{Learner}}].
}
\description{
Fuses a base learner with a search strategy to select
variables. Creates a learner object, which can be used like
any other learner object, but which internally uses varsel.
If the train function is called on it, the search strategy
and resampling are invoked to select an optimal set of
variables. Finally, a model is fitted on the complete
training data with these variables and returned. See
\code{\link{selectFeatures}} for more details.
}

