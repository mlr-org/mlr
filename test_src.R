library(devtools)
library(testthat)

load_all(".")

# configureMlr(on.par.out.of.bounds = "quiet")
# lrn = makeLearner("classif.rpart", predict.type = "prob", cp = 3)
# rdesc = makeResampleDesc("Holdout")
# ctrl = makeTuneControlGrid(tune.threshold = TRUE)

# res = tuneParams(lrn, binaryclass.task, resampling = rdesc, measures = list(mmce, auc), par.set = ps, control = ctrl)

# r = resample(lrn, sonar.task, rdesc)
# tr = tuneThreshold(r$pred, measure = mmce)
# p2 = setThreshold(r$pred, 0.7)

# also check with infeasible stuff

lrn = makeLearner("classif.rpart", predict.type = "prob")
rdesc = makeResampleDesc("Holdout")
ps = makeParamSet(
  makeDiscreteParam("cp", values = c(0.1, -1))
)
ctrl = makeTuneControlGrid(tune.threshold = TRUE)
res = tuneParams(lrn, sonar.task, resampling = rdesc, measures = list(mmce, auc), par.set = ps, control = ctrl)



