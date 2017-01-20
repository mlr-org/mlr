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

  # check SE prediction
  lrn = makeLearner("regr.featureless", predict.type = "se")
  regr.task.train = subsetTask(regr.task, regr.train.inds)
  m = train(lrn, regr.task.train)
  pred.se = getPredictionSE(predict(m, subsetTask(regr.task, regr.test.inds)))
  expect_equal(unique(pred.se), sd(getTaskTargets(regr.task.train)))

  # test that printers work correctly
  lrn = makeLearner("classif.featureless")
  expect_output(print(lrn), "featureless")
})
