\name{tuneParams}
\alias{tuneParams}
\title{Hyperparameter tuning.}
\usage{
tuneParams(learner, task, resampling, measures, par.set, control,
  show.info = getMlrOption("show.info"))
}
\arguments{
  \item{learner}{[\code{\link{Learner}} |
  \code{character(1)}]\cr The learner.  If you pass a
  string the learner will be created via
  \code{\link{makeLearner}}.}

  \item{task}{[\code{\link{SupervisedTask}}]\cr The task.}

  \item{resampling}{[\code{\link{ResampleInstance}} |
  \code{\link{ResampleDesc}}]\cr Resampling strategy to
  evaluate points in hyperparameter space. If you pass a
  description, it is instantiated once at the beginning by
  default, so all points are evaluated on the same
  training/test sets.  If you want to change that
  behaviour, look at \code{\link{TuneControl}}.}

  \item{measures}{[list of \code{\link{Measure}} |
  \code{\link{Measure}}]\cr Performance measures to
  evaluate. The first measure, aggregated by the first
  aggregation function is optimized, others are simply
  evaluated.}

  \item{par.set}{[\code{\link[ParamHelpers]{ParamSet}}]\cr
  Collection of parameters and their constraints for
  optimization.}

  \item{control}{[\code{\link{TuneControl}}]\cr Control
  object for search method. Also selects the optimization
  algorithm for tuning.}

  \item{show.info}{[\code{logical(1)}]\cr Print verbose
  output on console?  Default is set via
  \code{\link{configureMlr}}.}
}
\value{
[\code{\link{TuneResult}}].
}
\description{
Optimizes the hyperparameters of a learner.
Allows for different optimization methods, such as grid search, evolutionary strategies, etc.
You can select such an algorithm (and its settings)
by passing a corresponding control object. For a complete list of implemented algorithms look at
\code{\link{TuneControl}}.

Note that if tranformations are associated with the parameters, the returned result will contain
transformed values in the optimal result and the path.
}
\seealso{
Other tune: \code{\link{ModelMultiplexer}},
  \code{\link{makeModelMultiplexer}};
  \code{\link{TuneControl}},
  \code{\link{TuneControlCMAES}},
  \code{\link{TuneControlGrid}},
  \code{\link{TuneControlIrace}},
  \code{\link{TuneControlOptim}},
  \code{\link{TuneControlRandom}},
  \code{\link{makeTuneControlCMAES}},
  \code{\link{makeTuneControlGenSA}},
  \code{\link{makeTuneControlGrid}},
  \code{\link{makeTuneControlIrace}},
  \code{\link{makeTuneControlRandom}};
  \code{\link{getTuneResult}};
  \code{\link{makeModelMultiplexerParamSet}};
  \code{\link{makeTuneWrapper}};
  \code{\link{tuneThreshold}}
}

