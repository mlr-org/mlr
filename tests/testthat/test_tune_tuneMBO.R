context("tuneMBO")

test_that("tuneMBO", {
  n.des = 8
  n.iter = 2
  res = makeResampleDesc("Holdout")

  # tuning problem 1
  ps1 = makeParamSet(
    makeNumericParam("cp", lower = 0, upper = 1),
    makeIntegerParam("minsplit", lower = 1, upper = 20)
  )
  lrn1 = makeLearner("classif.rpart")

  # tuning problem 2
  ps2 = makeParamSet(
    makeNumericParam("sigma", lower = -10, upper = -1, trafo = function(x) 2^x)
  )
  lrn2 = makeLearner("classif.ksvm")
  des2 = generateDesign(n.des, ps2, fun = lhs::maximinLHS)

  # surrogate learner
  sur.lrn = makeLearner("regr.lm", predict.type = "se")

  # Problem 1 with manually defined mbo.ctrl and sur.lrn
  mbo.ctrl = mlrMBO::makeMBOControl()
  mbo.ctrl = mlrMBO::setMBOControlTermination(mbo.ctrl, iters = 2)
  mbo.ctrl = mlrMBO::setMBOControlInfill(mbo.ctrl, crit = crit.ei)
  ctrl = makeTuneControlMBO(learner = sur.lrn, mbo.control = mbo.ctrl)
  tr = tuneParams(lrn1, multiclass.task, res, par.set = ps1, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), n.des + n.iter)
  expect_equal(dim(as.data.frame(tr$opt.path)), c(n.des + n.iter, 2 + 1 + 4))

  # Problem 2 with manually given initial design
  ctrl = makeTuneControlMBO(mbo.design = des2, budget = n.iter + n.des)
  tr = tuneParams(lrn2, multiclass.task, res, par.set = ps2, control = ctrl)
  expect_equal(getOptPathLength(tr$opt.path), n.des + n.iter)
  expect_true(is.list(tr$x) && all(names(tr$x) == "sigma"))
  expect_true(tr$x$sigma > 0)
  df.op.trafo = as.data.frame(trafoOptPath(tr$opt.path))
  expect_true(all(df.op.trafo$sigma > 0))
})


test_that("tuneMBO works with tune.threshold", {
  # there was a bug here, see issue #2002
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  ps = makeParamSet(
    makeNumericParam("cp", lower = 0.1, upper = 1)
  )
  ctrl = makeTuneControlMBO(budget = 3L, tune.threshold = TRUE)
  lrn = makeTuneWrapper(lrn, hout, par.set = ps, control = ctrl)
  mod = train(lrn, sonar.task)
  expect_false(is.null(mod$learner.model$opt.result$threshold))
})

