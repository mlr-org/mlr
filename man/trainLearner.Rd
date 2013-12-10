\name{trainLearner}
\alias{trainLearner}
\title{Train an R learner.}
\usage{
  trainLearner(.learner, .task, .subset, ...)
}
\arguments{
  \item{.learner}{[\code{\link{RLearner}}]\cr Wrapped
  learner.}

  \item{.task}{[\code{\link{SupervisedTask}}]\cr Task to
  train learner on.}

  \item{.subset}{[\code{integer}]\cr Subset of cases for
  training set, index the task with this.  You probably
  want to use \code{\link{getTaskData}} for this purpose.}

  \item{...}{[any]\cr Additional (hyper)parameters, which
  need to be passed to the underlying train function.}
}
\value{
  [any]. Model of the underlying learner.
}
\description{
  Mainly for internal use. Trains a wrapped learner on a
  given training set. You have to implement this method if
  you want to add another learner to this package.
}
\details{
  Your implementation must adhere to the following: The
  model must be fitted on the subset of \code{.task} given
  by \code{.subset}. All parameters must in \code{...} must
  be passed to the underlying training function.
}

