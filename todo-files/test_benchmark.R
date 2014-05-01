library(mlr)
library(testthat)
library(parallelMap)

source("inst/tests/helper_objects.R")
source("todo-files/benchmark.R")

tasks = list(binaryclass.task, multiclass.task)
learners.txt = c("classif.fnn", "classif.rpart")
learners = lapply(learners.txt, function(x) makeLearner(cl=x, id=paste0("Learner.",x)))

rin = makeResampleDesc("CV", iters = 3L)
ps = makeParamSet(makeDiscreteLearnerParam("k", values=c(1,3,5)))

learners = c(learners, list(
  makeFeatSelWrapper(learners[[1]], resampling = rin, control = makeFeatSelControlRandom(maxit = 3)),
  makeTuneWrapper(learners[[1]], resampling = rin, par.set = ps, control = makeTuneControlGrid())))

resamplings = list(rin, makeResampleDesc("Bootstrap", iters=3))

measures = list(mmce, acc)

result = benchmark(learners=learners, tasks=tasks, resamplings=resamplings, measures=measures)
result
extractPrediction.benchmark.result(result) #FIXME extractPrediction(result) does not work
getFeatSelResult.benchmark.result(result) #FIXME getFeatSelResult(result) does not work as intended
getTuneResult.benchmark.result(result) #FIXME getTuneResult(result) does not work as intended

