\name{makeFilterWrapper}
\alias{makeFilterWrapper}
\title{Fuse learner with filter method.}
\usage{
  makeFilterWrapper(learner,
    fw.method = "random.forest.importance", fw.perc = 1)
}
\arguments{
  \item{learner}{[\code{\link{Learner}}]\cr The learner.}

  \item{fw.method}{[\code{character(1)}]\cr Filter method.
  Available are: linear.correlation, rank.correlation,
  information.gain, gain.ratio, symmetrical.uncertainty,
  chi.squared, random.forest.importance, relief, oneR
  Default is random.forest.importance.}

  \item{fw.perc}{[\code{numeric(1)}]\cr Percentage of
  highest ranking features to select after filtering.
  Default is 1 (=100 percent).}
}
\value{
  [\code{\link{Learner}}].
}
\description{
  Fuses a base learner with a filter method. Creates a
  learner object, which can be used like any other learner
  object. Internally Uses \code{\link{filterFeatures}}
  before every model fit.
}
\details{
  Look at package FSelector for details on the filter
  algorithms.

  After training, the selected features can be retrieved
  with \code{\link{getTuneResult}}.
}
\examples{
task = makeClassifTask(data=iris, target="Species")
lrn = makeLearner("classif.lda")
inner = makeResampleDesc("Holdout")
outer = makeResampleDesc("CV", iters = 2)
lrn = makeFilterWrapper(lrn, fw.perc=0.5)
mod = train(lrn, task)
print(getFilteredFeatures(mod))
r = resample(lrn, task, outer, extract = function(model) {
getFilteredFeatures(model)
})
print(r$extract)
}

