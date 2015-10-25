context("ClassificationViaRegressionWrapper")

test_that("ClassificationViaRegressionWrapper predicts with response", {
  lrn1 = makeLearner("regr.rpart")
  lrn2 = makeClassificationViaRegressionWrapper(lrn1)

  m = train(lrn2, binaryclass.task, subset = binaryclass.train.inds)
  expect_true(!inherits(m, "FailureModel"))

  p = predict(m, task = binaryclass.task, subset = binaryclass.test.inds)
  expect_less_than(0, performance(p, measures = mmce))
  expect_less_than(performance(p, measures = mmce), 1)
  expect_equal(length(binaryclass.test.inds), length(getPredictionResponse(p)))

  m = train(lrn2, multiclass.task, subset = multiclass.train.inds)
  expect_true(!inherits(m, "FailureModel"))

  p = predict(m, task = multiclass.task, subset = multiclass.test.inds)
  expect_less_than(0, performance(p, measures = mmce))
  expect_less_than(performance(p, measures = mmce), 1)
  expect_equal(length(multiclass.test.inds), length(getPredictionResponse(p)))
})

test_that("ClassificationViaRegressionWrapper predicts with prob", {
  lrn1 = makeLearner("regr.rpart")
  lrn2 = makeClassificationViaRegressionWrapper(lrn1, predict.type = "prob")
  
  m = train(lrn2, binaryclass.task, subset = binaryclass.train.inds)
  expect_true(!inherits(m, "FailureModel"))

  p = predict(m, task = binaryclass.task, subset = binaryclass.test.inds)
  expect_less_than(0, performance(p, measures = mmce))
  expect_less_than(performance(p, measures = mmce), 1)
  expect_equal(length(binaryclass.test.inds), length(getPredictionResponse(p)))
  expect_equal(length(binaryclass.test.inds), length(getPredictionProbabilities(p)))

  m = train(lrn2, multiclass.task, subset = multiclass.train.inds)
  expect_true(!inherits(m, "FailureModel"))

  p = predict(m, task = multiclass.task, subset = multiclass.test.inds)
  expect_less_than(0, performance(p, measures = mmce))
  expect_less_than(performance(p, measures = mmce), 1)
  expect_equal(length(multiclass.test.inds), length(getPredictionResponse(p)))
  expect_equal(length(multiclass.test.inds), nrow(getPredictionProbabilities(p)))
  expect_equal(length(levels(getTaskTargets(multiclass.task))), ncol(getPredictionProbabilities(p)))
})
