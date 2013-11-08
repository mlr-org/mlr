context("FailureModel")

test_that("FailureModel", {
  configureMlr(on.learner.error="quiet", show.learner.output=FALSE)
  m = train(makeLearner("classif.qda"), multiclass.task, subset=c(1,51,101))	
  expect_true(inherits(m, "FailureModel"))
  expect_true(!is.null(m$learner.model))
  p = predict(m, newdata=iris)
  expect_true(all(is.na(p$data$response)))


  wl = makeLearner("regr.ksvm", epsilon=10)
  m = train(wl, regr.task)	
  expect_true(inherits(m, "FailureModel"))
  expect_true(!is.null(m$learner.model))
  p = predict(m, newdata=regr.df)
  expect_true(all(is.na(p$data$response)))
  configureMlr(on.learner.error="stop", show.learner.output=FALSE)
})
