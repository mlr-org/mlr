d = getTaskData(yeast.task)
# drop some labels so example runs faster
d = d[, c(1:3, 15:117)]
task = makeMultilabelTask(data = d, target = c("label1", "label2", "label3"))
lrn = makeLearner("classif.rpart")
lrn = makeMultilabelBinaryRelevanceWrapper(lrn)
lrn = setPredictType(lrn, "prob")
# train, predict and evaluate
mod = train(lrn, yeast.task)
pred = predict(mod, yeast.task)
performance(pred, measure = list(multilabel.hamloss, multilabel.subset01, multilabel.f1))
getMultilabelBinaryPerformances(pred, measures = list(mmce, auc))
# above works also with predictions from resample!

