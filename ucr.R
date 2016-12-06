library(batchtools)
library(farff)
# library()


data.idx = 2:3
fs = list.dirs("ucr_data", full.names = TRUE, recursive = FALSE)[data.idx]

unlink("ucr_benchmark", recursive = TRUE)
reg = makeExperimentRegistry("ucr_benchmark", packages = "mlr", source = "ucr_defs.R")

for (f in fs) {
  f0 = basename(f)
  messagef("dir = %s", f)
  fp1 = file.path(f, paste0(f0, "_TRAIN.arff"))
  fp2 = file.path(f, paste0(f0, "_TEST.arff"))
  data.train = readARFF(fp1)
  n.train = nrow(data.train)
  data.test = readARFF(fp2)
  n.test = nrow(data.test)
  data.all = rbind(data.train, data.test)
  n = nrow(data.all)
  print(colnames(data.all))
  task = makeTimeSeriesClassifTask(data = data.all, target = "target")
  rin = makeFixedHoldoutInstance(train.inds = 1:n.train, test.inds = (n.train+1L):n, size = n)
  prob = list(train.filepath = fp1, test.filepath = fp2, task = task, rin = rin)
  addProblem(name = f0, data = prob)
}

addAlgorithm("naive_learner", fun = function(job, data, instance, base.lrn) {
  lrn = makeLearner(base.lrn)
  r = resample(lrn, data$task, data$rin, measures = MEASURES)
  as.list(r$aggr)
})

ades = list(naive_learner = data.table(base.lrn = BASE_LEARNERS))
addExperiments(algo.designs = ades)

# testJob(1)

submitJobs()
# r = loadResult(1L)
jp = getJobPars()
res = reduceResultsDataTable()
res = merge(jp, res)

print(res)

