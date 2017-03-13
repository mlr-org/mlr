context("tuneMBO")

test_that("tuneMBO", {
  skip_on_cran() # FIXME remove if mbo is on cran
  skip_if_not_installed("mlrMBO")
  attachNamespace("mlrMBO")
  # FIXME change when mlrMBO is on cran
  #requirePackagesOrSkip("!mlrMBO")
  res = makeResampleDesc("Holdout")
  ps = makeParamSet(
    makeNumericParam("cp", lower = 0, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 20)
  )

  n1 = 10; n2 = 2;
  mbo.ctrl = makeMBOControl(save.on.disk.at = integer(0L))
  mbo.ctrl = setMBOControlTermination(mbo.ctrl, iters = n2)
  des = generateDesign(n1, ps, fun = lhs::maximinLHS)
  ctrl = makeTuneControlMBO(learner = makeLearner("regr.lm"), mbo.control = mbo.ctrl, mbo.design = des)
  tr = tuneParams(makeLearner("classif.rpart"), multiclass.task, res, par.set = ps, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), n1+n2)
  expect_equal(dim(as.data.frame(tr$opt.path)), c(n1 + n2, 2 + 1 + 4))

  ps = makeParamSet(
    makeNumericParam("sigma", lower = -10, upper = -1, trafo = function(x) 2^x)
  )
  des = generateDesign(n1, ps, fun = lhs::maximinLHS)
  ctrl = makeTuneControlMBO(learner = makeLearner("regr.lm"), mbo.control = mbo.ctrl, mbo.design = des)
  tr = tuneParams("classif.ksvm", multiclass.task, res, par.set = ps, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), n1 + n2)
  expect_true(is.list(tr$x) && all(names(tr$x) == "sigma"))
  expect_true(tr$x$sigma > 0)
  df1 = as.data.frame(tr$opt.path)
  df2 = as.data.frame(trafoOptPath(tr$opt.path))
  # deactivate because we store trafo'ed values in opt.path
  # expect_true(all(df1$sigma < 0))
  expect_true(all(df2$sigma > 0))

  ps = makeParamSet(
    makeIntegerParam("ntree", lower = 10, upper = 50),
    makeNumericVectorParam("cutoff", len = 3, lower = 0.001, upper = 1, trafo = function(x) 0.9*x/sum(x))
  )
  ctrl$mbo.design = generateDesign(n1, ps, fun = lhs::maximinLHS)
  tr = tuneParams("classif.randomForest", multiclass.task, res, par.set = ps, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), n1 + n2)
})
