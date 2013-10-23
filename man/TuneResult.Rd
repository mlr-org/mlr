\name{TuneResult}
\alias{TuneResult}
\title{Result of tuning.}
\description{
  Container for results of hyperparameter tuning. Contains
  the obtained point in search space, its performance
  values and the optimization path which lead there.
}
\details{
  Object members: \describe{ \item{learner
  [\code{\link[mlr]{Learner}}]}{Learner that was
  optimized.} \item{control [\code{\link{TuneControl}}]}{
  Control object from tuning.} \item{x [\code{list}]}{Named
  list of hyperparameter values identified as optimal.}
  \item{y [\code{numeric}]}{Performance values for optimal
  \code{x}.} \item{opt.path
  [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path
  which lead to \code{x}.} }
}

