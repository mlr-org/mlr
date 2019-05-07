context("FailureModel")

test_that("FailureModel", {
  configureMlr(on.learner.error = "quiet", show.learner.output = FALSE)

  # classif: response
  lrn = makeLearner("classif.qda", predict.type = "prob")
  m = train(lrn, multiclass.task, subset = c(1, 51, 101))
  expect_true(inherits(m, "FailureModel"))
  expect_true(!is.null(m$learner.model))
  expect_true(grep("some group is too small", getFailureModelMsg(m)) == 1L)
  p = predict(m, newdata = iris)
  expect_true(all(is.na(p$data$response)))

  # classif: probs
  lrn = makeLearner("classif.qda", predict.type = "prob")
  m = train(lrn, multiclass.task, subset = c(1, 51, 101))
  expect_true(inherits(m, "FailureModel"))
  expect_true(grep("some group is too small", getFailureModelMsg(m)) == 1L)
  expect_true(!is.null(m$learner.model))
  p = predict(m, newdata = iris)
  expect_true(all(is.na(p$data$response)))
  prob = getPredictionProbabilities(p)
  expect_true(all(dim(prob) == c(150, 3)))
  expect_true(all(is.na(prob)))

  task = dropFeatures(regr.task, "chas")
  # regr: response
  wl = makeLearner("regr.km")
  m = train(wl, task, subset = 1:2)
  expect_true(inherits(m, "FailureModel"))
  expect_true(!is.null(m$learner.model))
  p = predict(m, task = task)
  expect_true(all(is.na(p$data$response)))

  # regr: se
  wl = makeLearner("regr.km", predict.type = "se")
  m = train(wl, task, subset = 1:2)
  expect_true(inherits(m, "FailureModel"))
  expect_true(!is.null(m$learner.model))
  p = predict(m, task = task)
  expect_true(all(is.na(p$data$response)))
  expect_true(all(is.na(p$data$se)))

  # costens: response
  lrn = makeCostSensClassifWrapper("classif.__mlrmocklearners__3")
  m = train(lrn, costsens.task)
  expect_true(isFailureModel(m))
  expect_true(!is.null(m$learner.model))
  expect_true(grep("foo", getFailureModelMsg(m)) == 1L)
  expect_output(print(m), "Training failed")
  expect_output(print(m), "foo")
  p = predict(m, newdata = iris)
  expect_true(all(is.na(p$data$response)))

  configureMlr(on.learner.error = "stop", show.learner.output = FALSE)
})
