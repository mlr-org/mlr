load_all()

set.seed(1)


tt = iris.task
m = matrix(c(0, 1, 1, 1, 0, 1, 10000, 10000, 0), 3, 3, byrow = T)
colnames(m) = rownames(m) = getTaskClassLevels(tt)
print(m)
cm = makeCostMeasure(costs = m, task = tt, combine = mean)
# tt = subsetTask(tt, c(1:50, 51:60, 101:130))
lrn = makeLearner("classif.multinom", predict.type = "prob")
r = holdout(lrn, tt, split = 0.1)
p = r$pred
z = tuneThreshold(p, measure = cm)
print(z)
print(getConfMatrix(p))
pp = setThreshold(p, z$th)
print(getConfMatrix(pp))
# pp = setThreshold(p, c(setosa = 1, versicolor = 1 , virginica = 1))
# print(getConfMatrix(pp))
# print(performance(p, cm))
