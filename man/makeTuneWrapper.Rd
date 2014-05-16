\name{makeTuneWrapper}
\alias{makeTuneWrapper}
\title{Fuse learner with tuning.}
\usage{
makeTuneWrapper(learner, resampling, measures, par.set, control,
  show.info = TRUE)
}
\arguments{
  \item{learner}{[\code{\link{Learner}}]\cr The learner.}

  \item{resampling}{[\code{\link{ResampleInstance}} |
  \code{\link{ResampleDesc}}]\cr Resampling strategy to
  evaluate points in hyperparameter space. If you pass a
  description, it is instantiated once at the beginning by
  default, so all points are evaluated on the same
  training/test sets.  If you want to change that
  behaviour, look at \code{\link{TuneControl}}.}

  \item{measures}{[\code{\link{Measure}} | list of
  \code{\link{Measure}}]\cr Performance measures to
  evaluate. The first measure, aggregated by the first
  aggregation function is optimized during tuning, others
  are simply evaluated.}

  \item{par.set}{[\code{\link[ParamHelpers]{ParamSet}}] \cr
  Collection of parameters and their constraints for
  optimization.}

  \item{control}{[\code{\link{TuneControl}}] \cr Control
  object for search method. Also selects the optimization
  algorithm for tuning.}

  \item{show.info}{[\code{logical(1)}]\cr Show info message
  after each hyperparameter evaluation?  Default is
  \code{TRUE}.}
}
\value{
[\code{\link{Learner}}].
}
\description{
Fuses a base learner with a search strategy to select its
hyperparameters. Creates a learner object, which can be
used like any other learner object, but which internally
uses \code{\link{tuneParams}}. If the train function is
called on it, the search strategy and resampling are
invoked to select an optimal set of hyperparameter values.
Finally, a model is fitted on the complete training data
with these optimal hyperparameters and returned. See
\code{\link{tuneParams}} for more details.

After training, the optimal hyperparameters (and other
related information) can be retrieved with
\code{\link{getTuneResult}}.
}
\examples{
task = makeClassifTask(data = iris, target = "Species")
lrn = makeLearner("classif.ksvm")
# stupid mini grid
ps = makeParamSet(
  makeDiscreteParam("C", values = 1:2),
  makeDiscreteParam("sigma", values = 1:2)
)
ctrl = makeTuneControlGrid()
inner = makeResampleDesc("Holdout")
outer = makeResampleDesc("CV", iters = 2)
lrn = makeTuneWrapper(lrn, resampling = inner, par.set = ps, control = ctrl)
mod = train(lrn, task)
print(getTuneResult(mod))
# nested resampling for evaluation
# we also extract tuned hyper pars in each iteration
r = resample(lrn, task, outer, extract = getTuneResult)
print(r$extract)
}

