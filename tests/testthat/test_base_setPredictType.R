context("setPredictType")

test_that("predict.type gets propagated", {
  inner = makeResampleDesc("Holdout")
  lrn1 = makeLearner("classif.rpart")
  ps = makeParamSet(makeNumericParam("cp", lower = 0.1, upper = 1))
  ctrl = makeTuneControlRandom(maxit = 2)
  lrn2 = makeTuneWrapper(lrn1, resampling = inner, control = ctrl, par.set = ps,
    measures = getDefaultMeasure(iris.task))
  expect_equal(lrn2$predict.type, "response")
  lrn2 = setPredictType(lrn2, "prob")
  expect_equal(lrn2$predict.type, "prob")

  m = train(lrn2, iris.task)
  p = predict(m, iris.task)
  prob = getPredictionProbabilities(p)
  expect_true(is.data.frame(prob) && nrow(prob) == nrow(iris))
})

test_that("predict.type works with BaggingWrapper, special case", {
  lrn1 = makeLearner("classif.rpart", predict.type = "prob")
  expect_error(makeBaggingWrapper(lrn1, bw.iters = 3), "response")

  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeBaggingWrapper(lrn1, bw.iters = 3)
  expect_equal(lrn2$predict.type, "response")

  lrn2 = setPredictType(lrn2, "prob")
  expect_equal(lrn2$predict.type, "prob")
  expect_equal(lrn2$next.learner$predict.type, "response")

  m = train(lrn2, iris.task)
  p = predict(m, iris.task)
  prob = getPredictionProbabilities(p)
  expect_true(is.data.frame(prob) && nrow(prob) == nrow(iris))
})
