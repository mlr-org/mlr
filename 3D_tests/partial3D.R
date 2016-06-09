lrn = makeLearner("regr.rpart")
fit = train(lrn, bh.task)
getTaskFeatureNames(bh.task)
pd = generatePartialPredictionData(fit, bh.task, c("crim", "zn"), interaction = T)
plotPartialPredictionPlotly(pd)

lrn = makeLearner("classif.rpart", predict.type = "prob")
fit = train(lrn, iris.task)
pd = generatePartialPredictionData(fit, iris.task, c("Sepal.Length", "Petal.Length"), interaction = T)
plotPartialPredictionPlotly(pd)

pd = generatePartialPredictionData(fit, iris.task, c("Petal.Width", "Petal.Length"), interaction = T)
plotPartialPredictionPlotly(pd)
