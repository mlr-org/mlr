load_all()

lrn1 = makeLearner("classif.logreg", predict.type = "prob")
lrn2 = makeLearner("classif.rpart", predict.type = "prob")

# p1 = holdout(lrn1, pid.task, measures = auc)$pred
# p2 = holdout(lrn2, pid.task, measures = auc)$pred
# ps = list(aa = p1, bb = p2)
# b = benchmark(list(lrn1, lrn2), pid.task)
# p = getBMRPredictions(b)
# p = getBMRPerformances(b)
# print(head(p))
plotViperCharts(b, chart = "lift", browser = F)

