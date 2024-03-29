
test_that("chains extra", {
  lrn1 = makeLearner("classif.rpart", minsplit = 10)
  lrn2 = makePreprocWrapperRemoveOutliers(lrn1, ro.alpha = 1)
  lrn3 = makePreprocWrapperPCA(lrn2)
  lrn4 = makeFilterWrapper(lrn3, fw.val = 0.5)

  m = train(lrn4, multiclass.task)

  p = predict(m, multiclass.task)
  perf = performance(p, mmce)
  expect_true(perf < 0.1)

  outer = makeResampleDesc("Holdout")
  inner = makeResampleDesc("CV", iters = 2)

  ps = makeParamSet(
    makeDiscreteParam(id = "minsplit", values = c(5, 10)),
    makeDiscreteParam(id = "ro.alpha", values = c(0.9, 1)),
    makeDiscreteParam(id = "fw.val", values = c(0.8, 1))
  )

  lrn5 = makeTuneWrapper(lrn4, resampling = inner, par.set = ps,
    control = makeTuneControlGrid())
  m = train(lrn5, task = multiclass.task)
  p = predict(m, task = multiclass.task)
  or = m$learner.model$opt.result
  expect_equal(length(or$x), 3)
  expect_equal(getOptPathLength(or$opt.path), 2 * 2 * 2)
  perf = performance(p, mmce)
  expect_true(perf < 0.1)
})
