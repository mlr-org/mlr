setwd("~/work/mlr2/mlr")
load_all()
source("~/work/mlr2/mlr/tests/testthat/helper_objects.R")


par.set = makeParamSet(
  makeLogicalParam("scaled"),
  makeLogicalParam("shrinking"),
  makeNumericParam("C")
)
lrn = makeLearner("classif.ksvm", kernel = "vanilladot")
rdesc = makeResampleDesc("Holdout", split = 0.3, stratify = TRUE)
ctrl = makeTuneControlIrace(maxExperiments = 20, nbIterations = 1, minNbSurvival = 1)
task = subsetTask(multiclass.task, c(1:10, 50:60, 100:110))
# tr = tuneParams(lrn, task, rdesc, par.set = ps, control = ctrl)


types = getParamTypes(par.set, use.names = TRUE)
x = list(scaled = "FALSE", shrinking = "FALSE")
j = types %in% c("logical", "logicalvector")
if (any(j)) {
  x[j] = lapply(x[j], function(xs) {
    as.logical(xs)
  })
}
class(x[[1L]])