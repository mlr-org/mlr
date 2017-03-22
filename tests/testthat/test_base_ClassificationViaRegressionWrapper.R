context("ClassificationViaRegressionWrapper")

test_that("ClassificationViaRegressionWrapper predicts with response", {
  lrn1 = makeLearner("regr.rpart")
  lrn2 = makeClassificationViaRegressionWrapper(lrn1)

  m = train(lrn2, binaryclass.task, subset = binaryclass.train.inds)
  expect_true(!inherits(m, "FailureModel"))

  p = predict(m, task = binaryclass.task, subset = binaryclass.test.inds)
  expect_lt(0, performance(p, measures = mmce))
  expect_lt(performance(p, measures = mmce), 1)
  expect_equal(length(binaryclass.test.inds), length(getPredictionResponse(p)))
})

test_that("ClassificationViaRegressionWrapper predicts with prob", {
  lrn1 = makeLearner("regr.rpart")
  lrn2 = makeClassificationViaRegressionWrapper(lrn1, predict.type = "prob")

  m = train(lrn2, binaryclass.task, subset = binaryclass.train.inds)
  expect_true(!inherits(m, "FailureModel"))

  p = predict(m, task = binaryclass.task, subset = binaryclass.test.inds)
  expect_lt(0, performance(p, measures = mmce))
  expect_lt(performance(p, measures = mmce), 1)
  expect_equal(length(binaryclass.test.inds), length(getPredictionResponse(p)))
  expect_equal(length(binaryclass.test.inds), length(getPredictionProbabilities(p)))
})
