


classif.ksvm = makeLearner("classif.ksvm")

plotLearnerPrediction(classif.ksvm, iris.task, three.d = T)
plotLearnerPrediction(classif.ksvm, iris.task, three.d = T, greyscale = T)
plotLearnerPrediction(classif.ksvm, iris.task, three.d = T, err.size = 5)
plotLearnerPrediction(classif.ksvm, iris.task, three.d = T, err.col = "red")

plotLearnerPrediction(classif.ksvm, pid.task, three.d = T)


classif.cvglmnet = makeLearner("classif.cvglmnet")

plotLearnerPrediction(classif.cvglmnet, iris.task, three.d = T)
plotLearnerPrediction(classif.cvglmnet, pid.task, three.d = T)
