# Learner: kknn
regr.kknn = makeLearner("regr.kknn")

plotLearnerPredictionPlotly(learner = regr.kknn, task = bh.task)
plotLearnerPredictionPlotly(learner = regr.kknn, task = bh.task, features = c("lstat", "rm"))
plotLearnerPredictionPlotly(learner = regr.kknn, task = bh.task, features = c("dis", "rm"), show.point = F)
plotLearnerPredictionPlotly(learner = regr.kknn, task = bh.task, gridsize = 10)
plotLearnerPredictionPlotly(learner = regr.kknn, task = bh.task, pointsize = 5)
plotLearnerPredictionPlotly(learner = regr.kknn, task = bh.task, pretty.names = F)
plotLearnerPredictionPlotly(learner = regr.kknn, task = bh.task, regr.greyscale = T)

# Learner: lm
regr.lm = makeLearner("regr.lm")

plotLearnerPredictionPlotly(learner = regr.lm, task = bh.task)
plotLearnerPredictionPlotly(learner = regr.lm, task = bh.task, features = c("tax", "b"))
plotLearnerPredictionPlotly(learner = regr.lm, task = bh.task, features = c("tax", "b"), show.point = F)
plotLearnerPredictionPlotly(learner = regr.lm, task = bh.task, gridsize = 10)
plotLearnerPredictionPlotly(learner = regr.lm, task = bh.task, pointsize = 5)
plotLearnerPredictionPlotly(learner = regr.lm, task = bh.task, pretty.names = F)
plotLearnerPredictionPlotly(learner = regr.lm, task = bh.task, greyscale = T)

# Learner: randomForest
regr.rf = makeLearner("regr.randomForest")

plotLearnerPredictionPlotly(learner = regr.rf, task = bh.task)
plotLearnerPredictionPlotly(learner = regr.rf, task = bh.task, features = c("indus", "age"))
plotLearnerPredictionPlotly(learner = regr.rf, task = bh.task, features = c("indus", "age"), show.point = F)
plotLearnerPredictionPlotly(learner = regr.rf, task = bh.task, gridsize = 10)
plotLearnerPredictionPlotly(learner = regr.rf, task = bh.task, pointsize = 5)
plotLearnerPredictionPlotly(learner = regr.rf, task = bh.task, pretty.names = F)
plotLearnerPredictionPlotly(learner = regr.rf, task = bh.task, greyscale = T)
