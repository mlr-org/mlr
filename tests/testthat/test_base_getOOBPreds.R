context("getOOBPreds")

test_that("getOOBPreds", {
  lrns = list(
    makeLearner("classif.randomForest"),
    makeFilterWrapper(learner = "classif.randomForest",
      fw.method = "FSelectorRcpp_information.gain",
      fw.abs = 2))

  task = subsetTask(binaryclass.task, subset = c(10:20, 180:190),
    features = getTaskFeatureNames(binaryclass.task)[12:15])

  for (lrn in lrns) {
    mod = train(lrn, task)
    oob = getOOBPreds(mod, task)
    pred = predict(mod, task)
    expect_true(is.numeric(performance(oob, measures = list(acc))))
    expect_equal(dim(oob$data), dim(pred$data))
    expect_equal(names(oob$data), names(pred$data))
    expect_equal(names(oob), names(pred))
  }
})
