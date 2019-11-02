context("DummyFeaturesWrapper")

test_that("DummyFeaturesWrapper", {
  methods = c("1-of-n", "reference")

  for (m in methods) {
    lrn = makeLearner("classif.ksvm")
    lrn.w = makeDummyFeaturesWrapper(lrn, method = m)
    bc.task.dummy = createDummyFeatures(bc.task, method = m)

    # check if predict works
    mod = train(lrn, bc.task.dummy, subset = 1:400)
    mod.w = train(lrn.w, bc.task, subset = 1:400)
    expect_equal(getLearnerModel(mod.w)$features, getTaskFeatureNames(bc.task.dummy))
    expect_equal(getLearnerModel(mod.w)$features, attr(getLearnerModel(mod)@terms, "term.labels"))

    # check if predict works
    pred = predict(mod, bc.task.dummy, subset = 401:getTaskSize(bc.task))
    pred.w = predict(mod.w, bc.task, subset = 401:getTaskSize(bc.task))
    expect_equal(getPredictionResponse(pred), getPredictionResponse(pred.w))
  }
})
