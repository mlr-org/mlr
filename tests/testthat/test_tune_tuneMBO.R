context("tuneMBO")

# FIXME remove if mbo is on cran
if (interactive()) {
test_that("tuneMBO", {
  skip_if_not_installed("mlrMBO")
  attachNamespace("mlrMBO")
  # FIXME change when mlrMBO is on cran
  #requirePackages("!mlrMBO")
  res = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeNumericParam("cp", lower = 0, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 20)
  )

  n1 = 10; n2 = 2;
  mbo.ctrl = makeMBOControl(init.design.points = n1, iters = n2, save.on.disk.at = integer(0L))
  ctrl = makeTuneControlMBO(learner = makeLearner("regr.lm"), mbo.control = mbo.ctrl)
  tr = tuneParams(makeLearner("classif.rpart"), multiclass.task, res, par.set = ps, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), n1+n2)
  expect_equal(dim(as.data.frame(tr$opt.path)), c(n1 + n2, 2 + 1 + 4))

  ctrl2 = makeTuneControlMBO(learner = makeLearner("regr.lm"), mbo.control = mbo.ctrl, budget = n1 + n2 + 3L)
  expect_identical(ctrl, ctrl2)

  expect_error(makeTuneControlMBO(learner = makeLearner("regr.lm"), mbo.control = mbo.ctrl, budget = n1 + n2 - 1L))
  expect_error(makeTuneControlMBO(learner = makeLearner("regr.lm"), mbo.control = mbo.ctrl, budget = n1 - 1L))

  ps = makeParamSet(
    makeNumericParam("sigma", lower = -10, upper = -1, trafo = function(x) 2^x)
  )
  tr = tuneParams("classif.ksvm", multiclass.task, res, par.set = ps, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), n1 + n2)
  expect_true(is.list(tr$x) && all(names(tr$x) == "sigma"))
  expect_true(tr$x$sigma > 0)
  df1 = as.data.frame(tr$opt.path)
  df2 = as.data.frame(trafoOptPath(tr$opt.path))
  expect_true(all(df1$sigma < 0))
  expect_true(all(df2$sigma > 0))

  ps = makeParamSet(
    makeIntegerParam("ntree", lower = 10, upper = 50),
    makeNumericVectorParam("cutoff", len = 3, lower = 0.001, upper = 1, trafo = function(x) 0.9*x/sum(x))
  )
  tr = tuneParams("classif.randomForest", multiclass.task, res, par.set = ps, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), n1 + n2)
})

}
