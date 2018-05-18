context("resample_cv")

test_that("getNestedResamplingIndices works correctly", {

  task = makeClassifTask(data = iris, target = "Species")
  lrn = makeLearner("classif.rpart")
  # stupid mini grid
  ps = makeParamSet(
    makeDiscreteParam("cp", values = c(0.05, 0.1)),
    makeDiscreteParam("minsplit", values = c(10, 20))
  )
  ctrl = makeTuneControlGrid()
  inner = makeResampleDesc("Holdout")
  outer = makeResampleDesc("CV", iters = 2)
  lrn = makeTuneWrapper(lrn, resampling = inner, par.set = ps, control = ctrl)
  mod = train(lrn, task)
  print(getTuneResult(mod))
  # nested resampling for evaluation
  # we also extract tuned hyper pars in each iteration
  r = resample(lrn, task, outer, extract = getTuneResult)

  # check if inner test.inds are retrieved correctly
  expect_length(unique(unlist(getNestedResamplingIndices(r)[[1]]$test.inds)), 25)
})
