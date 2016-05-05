
setwd("mlr")
devtools::load_all()


getTaskFeatureNames(bh.task)

# Learner: kknn
regr.kknn = makeLearner("regr.kknn")

plotLearnerPrediction(learner = regr.kknn, task = bh.task, three.d = T)
plotLearnerPrediction(learner = regr.kknn, task = bh.task, three.d = T, features = c("indus", "age"))
plotLearnerPrediction(learner = regr.kknn, task = bh.task, three.d = T, pointsize = 5)
plotLearnerPrediction(learner = regr.kknn, task = bh.task, three.d = T, greyscale = T)

# Learner: lm
regr.lm = makeLearner("regr.lm")

plotLearnerPrediction(learner = regr.lm, task = bh.task, three.d = T)
plotLearnerPrediction(learner = regr.lm, task = bh.task, three.d = T, features = c("indus", "age"))
plotLearnerPrediction(learner = regr.lm, task = bh.task, three.d = T, gridsize = 10)
plotLearnerPrediction(learner = regr.lm, task = bh.task, three.d = T, pointsize = 5)
plotLearnerPrediction(learner = regr.lm, task = bh.task, three.d = T, pretty.names = F)
plotLearnerPrediction(learner = regr.lm, task = bh.task, three.d = T, greyscale = T)

# Learner: randomForest
regr.rf = makeLearner("regr.randomForest")

plotLearnerPrediction(learner = regr.rf, task = bh.task, three.d = T)
plotLearnerPrediction(learner = regr.rf, task = bh.task, three.d = T, features = c("indus", "age"))
