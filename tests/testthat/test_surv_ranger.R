context("surv_ranger")

## TODO: Proper test required when predictions working
test_that("surv_ranger", {
  requirePackages("survival", default.method = "load")
  requirePackages("ranger", default.method = "load")
  
  lrn = makeLearner("surv.ranger", predict.type = "prob")
  task = makeSurvTask(data = surv.train, target = surv.target)
  
  set.seed(getOption("mlr.debug.seed"))
  m = mlr::train(lrn, task)

  expect_equal(m$learner.model$treetype, "Survival")
})
