library(mlr)
library(testthat)
library(parallelMap)

source("inst/tests/helper_objects.R")
source("todo-files/benchmark.R")

tasks = list(binaryclass.task, multiclass.task)
learners.txt = lapply(tasks, listLearnersForTask)
learners.txt = intersect(learners.txt[[1]], learners.txt[[2]])[2:5]
#learners.txt = setdiff(learners.txt, c("classif.plsDA","classif.qda","classif.quaDA")) #throw away not working ones
learners = lapply(learners.txt, function(x) makeLearner(cl=x, id=paste0("Learner.",x)))
resamplings = list(makeResampleDesc("CV", iters=3), makeResampleDesc("Bootstrap", iters=3))
configureMlr(on.learner.error="warn")
measures = list(mmce, acc)
result = benchmark(learners=learners, tasks=tasks, resamplings=resamplings, measures=measures)
result$result.df
str(result$results)