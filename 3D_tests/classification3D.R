# Learner: ksvm
classif.ksvm = makeLearner("classif.ksvm")


# for binary classif, with 2 features, plot surfacce of P(y = 1 | x)
plotLearnerPredictionPlotly(classif.ksvm, sonar.task, features = getTaskFeatureNames(sonar.task)[1:3], pointsize = 4)
plotLearnerPredictionPlotly(classif.ksvm, sonar.task, features = getTaskFeatureNames(sonar.task)[1:3],
                            show = "bounding.region", bounding.region.alphahull = 3)
plotLearnerPredictionPlotly(classif.ksvm, sonar.task, features = getTaskFeatureNames(sonar.task)[1:3],
                            show = "region")

plotLearnerPredictionPlotly(classif.ksvm, sonar.task, features = getTaskFeatureNames(sonar.task)[1:2])


# iris.task: 2 features
plotLearnerPredictionPlotly(classif.ksvm, iris.task)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, pointsize = 4)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, show.point = F)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, show.colorbar = F)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, show.point.legend = F)


# iris.task
plotLearnerPredictionPlotly(classif.ksvm, iris.task, features = getTaskFeatureNames(iris.task)[1:3])
plotLearnerPredictionPlotly(classif.ksvm, iris.task, features = c("Petal.Width", "Petal.Length", "Sepal.Width"))
plotLearnerPredictionPlotly(classif.ksvm, iris.task, features = getTaskFeatureNames(iris.task)[1:3], 
                            pointsize = 10)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, features = getTaskFeatureNames(iris.task)[1:3], 
                            pointsize = 10, alpha = 0.6)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, features = getTaskFeatureNames(iris.task)[1:3], 
                            err.col = "red")
plotLearnerPredictionPlotly(classif.ksvm, iris.task, features = getTaskFeatureNames(iris.task)[1:3], 
                            show.point.legend = F)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, features = getTaskFeatureNames(iris.task)[1:3], 
                            show = "bounding.point")
plotLearnerPredictionPlotly(classif.ksvm, iris.task, features = getTaskFeatureNames(iris.task)[1:3], 
                            show = "bounding.point", bounding.alpha = 1)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, features = getTaskFeatureNames(iris.task)[1:3], 
                            show = "bounding.point", bounding.alpha = 1, show.point = F)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, features = getTaskFeatureNames(iris.task)[1:3], 
                            show = "bounding.point", bounding.alpha = 1, show.point = F, bounding.point.legend = T)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, features = getTaskFeatureNames(iris.task)[1:3], 
                            show = "bounding.region")
plotLearnerPredictionPlotly(classif.ksvm, iris.task, features = getTaskFeatureNames(iris.task)[1:3], 
                            show = "bounding.region", bounding.region.alphahull = 3)
plotLearnerPredictionPlotly(classif.ksvm, iris.task, features = getTaskFeatureNames(iris.task)[1:3], 
                            show = "region")
plotLearnerPredictionPlotly(classif.ksvm, iris.task, features = getTaskFeatureNames(iris.task)[1:3], 
                            show = "region", show.point = F)


# pid.task
plotLearnerPredictionPlotly(classif.ksvm, pid.task)
plotLearnerPredictionPlotly(classif.ksvm, pid.task, features = c("pressure", "triceps", "age"))
plotLearnerPredictionPlotly(classif.ksvm, pid.task, features = getTaskFeatureNames(pid.task)[1:3],
                            show = "region")
plotLearnerPredictionPlotly(classif.ksvm, pid.task, features = getTaskFeatureNames(pid.task)[1:3],
                            show = "region", region.alpha = 1)
plotLearnerPredictionPlotly(classif.ksvm, pid.task, features = getTaskFeatureNames(pid.task)[1:3],
                            show = "bounding.region", bounding.region.alphahull = 3)
plotLearnerPredictionPlotly(classif.ksvm, pid.task, greyscale = T)
plotLearnerPredictionPlotly(classif.ksvm, pid.task, err.size = 4)
plotLearnerPredictionPlotly(classif.ksvm, pid.task, err.col = "red")
plotLearnerPredictionPlotly(classif.ksvm, pid.task, err.size = 4, err.col = "green")
plotLearnerPredictionPlotly(classif.ksvm, pid.task, greyscale = T, err.size = 4, err.col = "green")


# Learner: cvglmnet
classif.cvglmnet = makeLearner("classif.cvglmnet")

plotLearnerPredictionPlotly(classif.cvglmnet, iris.task)
plotLearnerPredictionPlotly(classif.cvglmnet, iris.task, features = getTaskFeatureNames(iris.task)[1:3],
                            show = "region")

plotLearnerPredictionPlotly(classif.cvglmnet, pid.task)


# Learner: randomForest
classif.randomForest = makeLearner("classif.randomForest")

# bugs for randomForest
plotLearnerPredictionPlotly(classif.randomForest, iris.task)
plotLearnerPredictionPlotly(classif.randomForest, iris.task, features = getTaskFeatureNames(iris.task)[1:3],
                            show = "region", region.alpha = 0.8)
plotLearnerPredictionPlotly(classif.randomForest, pid.task)
plotLearnerPredictionPlotly(classif.randomForest, bc.task)
plotLearnerPredictionPlotly(classif.randomForest, iris.task, show = "region")
