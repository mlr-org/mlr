context("plotLearnerPrediction")

test_that("plotLearnerPrediction", {
  gs = 10
  plotLearnerPrediction("classif.rpart", multiclass.task, gridsize = gs)
  plotLearnerPrediction("classif.rpart", binaryclass.task, gridsize = gs)

  plotLearnerPrediction("regr.rpart", regr.task, gridsize = gs)
  plotLearnerPrediction("regr.lm", regr.task,
    features = getTaskFeatureNames(regr.task)[1], gridsize = gs)

  plotLearnerPrediction("cluster.kmeans", noclass.task, gridsize = gs)
})
