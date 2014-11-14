context("tune")

test_that("tune", {
  library(e1071)
  cp = c(0.05, 0.9)
  minsplit = 1:3
  ps1 = makeParamSet(
    makeDiscreteParam("cp", values = cp),
    makeDiscreteParam("minsplit", values = minsplit)
  )
  ctrl = makeTuneControlGrid()
  folds = 3

  tr = tune.rpart(formula = multiclass.formula, data = multiclass.df, cp = cp, minsplit = minsplit,
    tunecontrol = tune.control(sampling = "cross", cross = folds))
  lrn = makeLearner("classif.rpart")
  cv.instance = e1071CVToMlrCV(tr)
  m1 = setAggregation(mmce, test.mean)
  m2 = setAggregation(mmce, test.sd)
  tr2 = tuneParams(lrn, multiclass.task, cv.instance, par.set = ps1, control = ctrl, measures = list(m1, m2))
  pp = as.data.frame(tr2$opt.path)
  for (i in 1:nrow(tr$performances)) {
    cp = tr$performances[i,"cp"]
    ms = tr$performances[i,"minsplit"]
    j = which(pp$cp == cp & pp$minsplit == ms )
    expect_equal(tr$performances[i,"error"], pp[j,"mmce.test.mean"])
    expect_equal(tr$performances[i,"dispersion"], pp[j,"mmce.test.sd"])
  }
  # test printing
  expect_output(print(tr2), "mmce.test.mean=")

  # check multiple measures and binary thresholding
  rdesc = makeResampleDesc("Holdout")
  ms = c("acc", "mmce", "timefit")
  lrn2 = makeLearner("classif.rpart", predict.type = "prob")
  ctrl = makeTuneControlGrid(tune.threshold = TRUE)
  tr2 = tuneParams(lrn2, binaryclass.task, rdesc, par.set = ps1, control = ctrl)
  expect_true(is.numeric(as.data.frame(tr2$opt.path)$threshold))

  # check multiclass thresholding
  tr3 = tuneParams(lrn2, multiclass.task, rdesc, par.set = ps1, control = ctrl)
  op.df = as.data.frame(tr3$opt.path)
  op.df = op.df[,grepl("threshold_", colnames(op.df))]
  expect_true(all(sapply(op.df, is.numeric)))

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
  lrn = makeLearner("classif.mock2")
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



