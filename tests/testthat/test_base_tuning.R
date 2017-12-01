context("tuning")

test_that("tune", {
  requirePackagesOrSkip("e1071", default.method = "load")
  cp = c(0.05, 0.9)
  minsplit = 1:2
  ps1 = makeParamSet(
    makeDiscreteParam("cp", values = cp),
    makeDiscreteParam("minsplit", values = minsplit)
  )
  ctrl = makeTuneControlGrid()
  folds = 3

  tr = e1071::tune.rpart(formula = multiclass.formula, data = multiclass.df, cp = cp, minsplit = minsplit,
    tunecontrol = e1071::tune.control(sampling = "cross", cross = folds))
  lrn = makeLearner("classif.rpart")
  cv.instance = e1071CVToMlrCV(tr)
  m1 = setAggregation(mmce, test.mean)
  m2 = setAggregation(mmce, test.sd)
  tr2 = tuneParams(lrn, multiclass.task, cv.instance, par.set = ps1, control = ctrl, measures = list(m1, m2))
  pp = as.data.frame(tr2$opt.path)
  for (i in seq_len(nrow(tr$performances))) {
    cp = tr$performances[i, "cp"]
    ms = tr$performances[i, "minsplit"]
    j = which(pp$cp == cp & pp$minsplit == ms)
    expect_equal(tr$performances[i, "error"], pp[j, "mmce.test.mean"])
    expect_equal(tr$performances[i, "dispersion"], pp[j, "mmce.test.sd"])
  }
  # test printing
  expect_output(print(ctrl), "Imputation value: <worst>")
  ctrl$impute.val = 10
  expect_output(print(ctrl), "Imputation value: 10")
  expect_output(print(tr2), "mmce.test.mean=")

  # check multiple measures and binary thresholding
  rdesc = makeResampleDesc("Holdout")
  ms = c("acc", "mmce", "timefit")
  lrn2 = makeLearner("classif.rpart", predict.type = "prob")
  ctrl = makeTuneControlGrid(tune.threshold = TRUE, tune.threshold.args = list(nsub = 2L))
  tr2 = tuneParams(lrn2, binaryclass.task, rdesc, par.set = ps1, control = ctrl)
  expect_true(is.numeric(as.data.frame(tr2$opt.path)$threshold))
  expect_true(isScalarNumeric(tr2$threshold))

  # check multiclass thresholding
  ctrl = makeTuneControlGrid(tune.threshold = TRUE, tune.threshold.args = list(control = list(maxit = 2)))
  tr3 = tuneParams(lrn2, multiclass.task, rdesc, par.set = ps1, control = ctrl)
  op.df = as.data.frame(tr3$opt.path)
  op.df = op.df[, grepl("threshold_", colnames(op.df))]
  expect_true(all(sapply(op.df, is.numeric)))
  expect_true(is.numeric(tr3$threshold) && length(tr3$threshold) == 3L && !any(is.na(tr3$threshold)))

  expect_error(tuneParams(lrn, multiclass.task, cv.instance, par.set = makeParamSet(), control = ctrl))
})

test_that("tuning works with infeasible pars", {
  # i am not sure if we want that behavior always but currently we impute Inf when we eval
  # outside of constraints
  # and there was a bug in that code so we test now

  ps = makeParamSet(
    makeDiscreteParam("cp", values = c(0.05, 2))
  )
  lrn = makeLearner("classif.rpart")
  rdesc = makeResampleDesc("Holdout", split = 0.2)
  ctrl = makeTuneControlGrid()
  z = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl)
  d = as.data.frame(z$opt.path)
  expect_true(is.finite(d[1L, "mmce.test.mean"]))
  expect_true(is.na(d[1L, "error.message"]))
  expect_true(is.na(d[2L, "mmce.test.mean"]))
  expect_true(!is.na(d[2L, "error.message"]))
})

test_that("tuning works with errors", {
  configureMlr(on.learner.error = "quiet")
  ps = makeParamSet(
    makeDiscreteParam("alpha", values = c(1, 0))
  )
  lrn = makeLearner("classif.__mlrmocklearners__2")
  rdesc = makeResampleDesc("Holdout")
  ctrl = makeTuneControlGrid()
  z = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl)
  d = as.data.frame(z$opt.path)
  expect_true(is.finite(d[1L, "mmce.test.mean"]))
  expect_true(is.na(d[1L, "error.message"]))
  expect_true(is.na(d[2L, "mmce.test.mean"]))
  expect_true(grep("foo", d[2L, "error.message"]) == 1L)
  configureMlr(on.learner.error = "stop")
})

# see bug in issue 219
test_that("tuning works with tuneThreshold and multiple measures", {
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  rdesc = makeResampleDesc("Holdout")
  ctrl = makeTuneControlRandom(tune.threshold = TRUE, maxit = 2L)
  ps = makeParamSet(
    makeNumericParam("cp", lower = 0.1, upper = 0.2)
  )
  res = tuneParams(lrn, binaryclass.task, resampling = rdesc, measures = list(mmce, auc), par.set = ps, control = ctrl)
  expect_true(is.numeric(res$y) && length(res$y) == 2L && !any(is.na(res$y)))

# also check with infeasible stuff
  ps = makeParamSet(
    makeDiscreteParam("cp", values = c(0.1, -1))
  )
  ctrl = makeTuneControlGrid(tune.threshold = TRUE)
  res = tuneParams(lrn, sonar.task, resampling = rdesc, measures = list(mmce, auc),
    par.set = ps, control = ctrl)
  expect_true(is.numeric(res$y) && length(res$y) == 2L && !any(is.na(res$y)))
})

test_that("tuning allows usage of budget", {
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  rdesc = makeResampleDesc("Holdout")
  ctrl = makeTuneControlCMAES(budget = 18, lambda = 6, maxit = 3)
  ps = makeParamSet(
    makeNumericParam("cp", lower = 0.1, upper = 0.2),
    makeIntegerParam("minsplit", lower = 1, upper = 10)
  )
  res = tuneParams(lrn, binaryclass.task, resampling = rdesc, par.set = ps, control = ctrl)
  expect_true(is.numeric(res$y) && (length(res$y) == 1L) && !any(is.na(res$y)))

  # also check with infeasible stuff
  ps = makeParamSet(
    makeDiscreteParam("cp", values = c(0.1, -1))
  )
  ctrl = makeTuneControlGrid(tune.threshold = TRUE, budget = 2L)
  res = tuneParams(lrn, sonar.task, resampling = rdesc, measures = list(mmce, auc),
    par.set = ps, control = ctrl)
  expect_true(is.numeric(res$y) && length(res$y) == 2L && !any(is.na(res$y)))

  lrn = makeLearner("classif.rpart", predict.type = "prob")
  rdesc = makeResampleDesc("Holdout")
  ctrl = makeTuneControlRandom(tune.threshold = TRUE, maxit = NULL, budget = 3L)
  ps = makeParamSet(
    makeNumericParam("cp", lower = 0.1, upper = 0.2)
  )
  res = tuneParams(lrn, binaryclass.task, resampling = rdesc, measures = list(mmce, auc),
    par.set = ps, control = ctrl)
  expect_true(is.numeric(res$y) && length(res$y) == 2L && !any(is.na(res$y)))
  expect_identical(getOptPathLength(res$opt.path), 3L)
})

test_that("Learner defined with expression in param requires, see #369 and PH #52", {
  ps = makeParamSet(
    makeDiscreteLearnerParam(id = "a", values = c("x", "y")),
      makeNumericLearnerParam(id = "b", lower = 0.0, upper = 1.0, requires = expression(a == "x"))
  )

  rdesc = makeResampleDesc("Holdout")
  ctrl = makeTuneControlRandom()
  tuneParams("classif.__mlrmocklearners__5", binaryclass.task, resampling = rdesc, par.set = ps, control = ctrl)
})


test_that("tuning does not break with small discrete values, see bug in #1115", {
  ctrl  = makeTuneControlGrid()
  ps = makeParamSet(
    makeDiscreteParam("cp", values = c(1e-8, 1e-9))
  )
  # this next line created an exception in the bug
  tuneParams("classif.rpart", multiclass.task, hout, par.set = ps, control = ctrl)
})

test_that("tuning works with large param.sets", {
  lrn = makeLearner("classif.__mlrmocklearners__5")
  ctrl = makeTuneControlRandom(maxit = 3)
  # create long list of learner params
  ps.length = 200
  long.learner.params = do.call(base::c, lapply(seq_len(ps.length), function(x) {
    makeParamSet(makeIntegerLearnerParam(paste0("some.parameter", x), 1, 10))
  }))
  lrn$par.set = c(lrn$par.set, long.learner.params)
  tuneParams(lrn, pid.task, cv5, par.set = long.learner.params, control = ctrl, show.info = TRUE)
})

