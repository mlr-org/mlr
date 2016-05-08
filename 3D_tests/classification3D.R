# Learner: ksvm
classif.ksvm = makeLearner("classif.ksvm")

# iris.task
plotLearnerPrediction(classif.ksvm, iris.task, three.d = T)
plotLearnerPrediction(classif.ksvm, iris.task, three.d = T, features = c("Petal.Width", "Petal.Length", "Sepal.Width"))
plotLearnerPrediction(classif.ksvm, iris.task, three.d = T, greyscale = T)
plotLearnerPrediction(classif.ksvm, iris.task, three.d = T, pointsize = 10)
plotLearnerPrediction(classif.ksvm, iris.task, three.d = T, pointsize = 10, alpha = 0.6)
plotLearnerPrediction(classif.ksvm, iris.task, three.d = T, pointsize = 10, alpha = 0.6, err.alpha = 1)
plotLearnerPrediction(classif.ksvm, iris.task, three.d = T, err.size = 5)
plotLearnerPrediction(classif.ksvm, iris.task, three.d = T, err.col = "red")
plotLearnerPrediction(classif.ksvm, iris.task, three.d = T, err.size = 5, err.col = "grey")
plotLearnerPrediction(classif.ksvm, iris.task, three.d = T, greyscale = T, err.col = "red", err.size = 5)
# pid.task
plotLearnerPrediction(classif.ksvm, pid.task, three.d = T)
plotLearnerPrediction(classif.ksvm, pid.task, three.d = T, features = c("pressure", "triceps", "age"))
plotLearnerPrediction(classif.ksvm, pid.task, three.d = T, greyscale = T)
plotLearnerPrediction(classif.ksvm, pid.task, three.d = T, err.size = 4)
plotLearnerPrediction(classif.ksvm, pid.task, three.d = T, err.col = "red")
plotLearnerPrediction(classif.ksvm, pid.task, three.d = T, err.size = 4, err.col = "green")
plotLearnerPrediction(classif.ksvm, pid.task, three.d = T, greyscale = T, err.size = 4, err.col = "green")


# Learner: cvglmnet
classif.cvglmnet = makeLearner("classif.cvglmnet")

plotLearnerPrediction(classif.cvglmnet, iris.task, three.d = T)

plotLearnerPrediction(classif.cvglmnet, pid.task, three.d = T)


# Learner: randomForest
classif.randomForest = makeLearner("classif.randomForest")

# bugs for randomForest
plotLearnerPrediction(classif.randomForest, iris.task, three.d = T)
plotLearnerPrediction(classif.randomForest, pid.task, three.d = T)
plotLearnerPrediction(classif.randomForest, bc.task, three.d = T)
