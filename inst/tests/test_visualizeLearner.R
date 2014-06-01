context("visualizeLearner")

test_that("visualizeLearner", {
  gs = 10
  visualizeLearner("classif.rpart", multiclass.task, gridsize = gs)

  visualizeLearner("regr.rpart", regr.task, gridsize = gs)
  visualizeLearner("regr.lm", regr.task,
    features = getTaskFeatureNames(regr.task)[1], gridsize = gs)
})
