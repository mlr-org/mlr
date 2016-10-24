setwd("~/work/mlr-org/mlr")
load_all()
tsk = bh.task
lrn = makeLearner("regr.rpart")
mod = train(lrn, tsk)
pred = predict(mod, tsk)

plotResiduals(pred, type = "hist")






lrns = lapply(c("regr.rpart", "regr.randomForest", "regr.penalized.lasso"), makeLearner)
tsks = list(bh.task)
bmr = benchmark(lrns, tsks)

plotResiduals(bmr)

plotResiduals(bmr, type = "scatterplot", loess.smooth = FALSE)
