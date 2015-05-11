library(BatchExperiments)
library(OpenML)

oml.data.ids = c(61)
problem.ids = paste0("d", oml.data.ids)

resample.sizes = c(90L, 100L)
folds = c(2L, 8L)
cv.reps = c(1L, 10L)
cv.stratifies = c(TRUE, FALSE)
repls = 2L


learner = "classif.naiveBayes"

unlink("dps-files", recursive = TRUE)
reg =  makeExperimentRegistry(id = "dps",
  packages = c("mlr", "OpenML")
)

for (i in seq_along(oml.data.ids)) {
  addProblem(reg, id = problem.ids[i],
    static = list(oml.data.id = oml.data.ids[i]),
    dynamic = function(static, resample.size) {
      library(mlr)
      oml.data = getOMLDataSet(static$oml.data.id)
      task = toMlr(oml.data)
      resample.set = sample(getTaskSize(task), resample.size, replace = FALSE)
      list(task = task, resample.set = resample.set)
    }
  )
}

estimPerf = function(static, dynamic, do.subset, resampling) {
  library(mlr)
  task = dynamic$task
  if (do.subset)
    task = subsetTask(task, dynamic$resample.set)
  res = resample(learner, task, resampling)
  return(list(phat = res$aggr[[1L]], res = res))
}

addAlgorithm(reg, "cv", fun = function(static, dynamic, cv.reps, folds, cv.stratify) {
  library(mlr)
  if (cv.reps ==  1L)
    rdesc = makeResampleDesc("CV", iters = folds, stratify = cv.stratify)
  else
    rdesc = makeResampleDesc("RepCV", folds = folds, reps = cv.reps, stratify = cv.stratify)
  estimPerf(static, dynamic, do.subset = TRUE, resampling = rdesc)
})

# addAlgorithm("dps", fun = function(static, dynamic, iters) {
  # rdesc = makeResampleInstance("DPS", iters = iters)
  # estimPerf(static, dynamic, rin)
# })

addAlgorithm(reg, "true", fun = function(static, dynamic, iters) {
  size = getTaskSize(dynamic$task)
  train.inds = dynamic$resample.set
  test.inds = setdiff(1:size, train.inds)
  rin = makeFixedHoldoutInstance(train.inds, test.inds, size)
  estimPerf(static, dynamic, do.subset = FALSE, resampling = rin)
})

pdes = lapply(problem.ids, function(id) makeDesign(id,
  exhaustive = list(resample.size = 100L)))
ades.cv = makeDesign("cv", exhaustive = list(folds = folds, cv.reps = cv.reps, cv.stratify = cv.stratifies))
addExperiments(reg, prob.des = pdes, algo.des = ades.cv, repls = repls)
addExperiments(reg, prob.des = pdes, algo.des = "true", repls = repls)

batchExport(reg, estimPerf = estimPerf)

submitJobs(reg)
waitForJobs(reg)
res1 = reduceResultsExperiments(reg, fun = function(job, res) res["phat"])
library(plyr)
res2 = ddply(res, c(getResultVars(res, prob), "repl"), function(d) {
  j = whoich(d$algo == "true")
  ptrue = d[j, ""]
  p.true = whi
  bias =
})
