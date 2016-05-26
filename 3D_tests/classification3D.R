# Learner: ksvm
classif.ksvm = makeLearner("classif.ksvm")


# for binary classif, with 2 features, plot surfacce of P(y = 1 | x)
plotLearnerPredictionPlotly(classif.ksvm, sonar.task, features = getTaskFeatureNames(sonar.task)[1:3])

plotLearnerPrediction(classif.ksvm, sonar.task, features = getTaskFeatureNames(sonar.task)[1:2])


# iris.task
plotLearnerPredictionPlotly(classif.ksvm, iris.task)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, features = c("Petal.Width", "Petal.Length", "Sepal.Width"))
plotLearnerPredictionPlotly(classif.ksvm, iris.task, greyscale = T)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, pointsize = 10)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, pointsize = 10, alpha = 0.6)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, pointsize = 10, alpha = 0.6, err.alpha = 1)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, err.size = 5)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, err.col = "red")
plotLearnerPredictionPlotly(classif.ksvm, iris.task, err.size = 5, err.col = "grey")
plotLearnerPredictionPlotly(classif.ksvm, iris.task, greyscale = T, err.col = "red", err.size = 5)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, show.point.legend = F)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, show.err.legend = F)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, bounding.point = T)


# pid.task
plotLearnerPredictionPlotly(classif.ksvm, pid.task)
plotLearnerPredictionPlotly(classif.ksvm, pid.task, features = c("pressure", "triceps", "age"))
plotLearnerPredictionPlotly(classif.ksvm, pid.task, greyscale = T)
plotLearnerPredictionPlotly(classif.ksvm, pid.task, err.size = 4)
plotLearnerPredictionPlotly(classif.ksvm, pid.task, err.col = "red")
plotLearnerPredictionPlotly(classif.ksvm, pid.task, err.size = 4, err.col = "green")
plotLearnerPredictionPlotly(classif.ksvm, pid.task, greyscale = T, err.size = 4, err.col = "green")


# Learner: cvglmnet
classif.cvglmnet = makeLearner("classif.cvglmnet")

plotLearnerPredictionPlotly(classif.cvglmnet, iris.task)
plotLearnerPredictionPlotly(classif.cvglmnet, iris.task, bounding.point = T)

plotLearnerPredictionPlotly(classif.cvglmnet, pid.task)


# Learner: randomForest
classif.randomForest = makeLearner("classif.randomForest")

# bugs for randomForest
plotLearnerPredictionPlotly(classif.randomForest, iris.task)
plotLearnerPredictionPlotly(classif.randomForest, pid.task)
plotLearnerPredictionPlotly(classif.randomForest, bc.task)
plotLearnerPredictionPlotly(classif.randomForest, iris.task, bounding.point = T)
