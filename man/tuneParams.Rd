\name{tuneParams}
\alias{tuneParams}
\title{Hyperparameter tuning.}
\usage{
tuneParams(learner, task, resampling, measures, par.set, control,
  show.info = TRUE)
}
\arguments{
  \item{learner}{[\code{\link{Learner}}]\cr The learner.}

  \item{task}{[\code{\link{SupervisedTask}}]\cr The task.}

  \item{resampling}{[\code{\link{ResampleInstance}} |
  \code{\link{ResampleDesc}}]\cr Resampling strategy to
  evaluate points in hyperparameter space. If you pass a
  description, it is instantiated once at the beginning by
  default, so all points are evaluated on the same
  training/test sets.  If you want to change that
  behaviour, look at \code{\link{TuneControl}}.}

  \item{par.set}{[\code{\link[ParamHelpers]{ParamSet}}]\cr
  Collection of parameters and their constraints for
  optimization.}

  \item{control}{[\code{\link{TuneControl}}]\cr Control
  object for search method. Also selects the optimization
  algorithm for tuning.}

  \item{measures}{[\code{\link{Measure}} | list of
  \code{\link{Measure}}]\cr Performance measures to
  evaluate. The first measure, aggregated by the first
  aggregation function is optimized during tuning, others
  are simply evaluated.}

  \item{show.info}{[\code{logical(1)}]\cr Show info message
  after each hyperparameter evaluation?  Default is
  \code{TRUE}.}
}
\value{
[\code{\link{TuneResult}}].
}
\description{
Optimizes the hyperparameters of a learner for a
classification or regression problem. Allows for different
optimization methods, such as grid search, evolutionary
strategies, etc. You can select such an algorithm (and its
settings) by passing a corresponding control object. For a
complete list of implemented algorithms look at
\code{\link{TuneControl}}.
}
\details{
Note that if tranformations are associated with the
parameters, the returned result will contain transformed
values in the optimal result and the path.
}

