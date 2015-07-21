context("plotLearnerPrediction")

test_that("plotLearnerPrediction", {
  gs = 10
  plotLearnerPrediction("classif.rpart", multiclass.task, gridsize = gs)
  ggsave(tempfile(fileext = ".png"))
  plotLearnerPrediction("classif.rpart", binaryclass.task, gridsize = gs)
  ggsave(tempfile(fileext = ".png"))

  plotLearnerPrediction("regr.rpart", regr.task, gridsize = gs)
  ggsave(tempfile(fileext = ".png"))
  plotLearnerPrediction("regr.lm", regr.task,
    features = getTaskFeatureNames(regr.task)[1], gridsize = gs)
  ggsave(tempfile(fileext = ".png"))

  plotLearnerPrediction("cluster.kmeans", noclass.task, gridsize = gs)
  ggsave(tempfile(fileext = ".png"))
})
