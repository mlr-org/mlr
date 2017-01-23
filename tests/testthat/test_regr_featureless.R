context("regr_featureless")

test_that("regr_featureless", {
  predictConstant = function(task, train.inds, fun) {
    y = getTaskTargets(subsetTask(task, train.inds))
    ntest = getTaskSize(task) - length(train.inds)
    rep(fun(y), ntest)
  }
  ps.list = list(
    list(method = "mean"),
    list(method = "median")
  )
  old.predicts.list = list(
    predictConstant(regr.task, regr.train.inds, mean),
    predictConstant(regr.task, regr.train.inds, median)
  )

  testSimpleParsets("regr.featureless", regr.df, regr.target,
    regr.train.inds, old.predicts.list, ps.list)

  # test that printers work correctly
  lrn = makeLearner("classif.featureless")
  expect_output(print(lrn), "featureless")
})
