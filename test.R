
load_all()

# p = plotLearnerPrediction("classif.rpart", iris.task, cv = 0L)

# p = plotLearnerPrediction("cluster.kmeans", agri.task, cv = 0L)

p = plotLearnerPrediction("regr.lm", bh.task, features = getTaskFeatureNames(bh.task)[1L], cv = 0L)

# plotLearnerPrediction("regr.lm", bh.task)

print(p)

