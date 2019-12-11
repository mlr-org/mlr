context("dump")

test_that("error dump is created in train", {
  lrn = makeLearner("classif.qda", predict.type = "prob",
    config = list(on.learner.error = "quiet", on.error.dump = TRUE))
  mod = train(lrn, iris.task, subset = c(1L, 51L, 101L))
  expect_class(getFailureModelDump(mod), "dump.frames")
})

test_that("error dump is created in predict", {
  lrn = makeLearner("classif.knn", config = list(on.learner.error = "quiet", on.error.dump = TRUE))
  lrn$properties = c(lrn$properties, "missings")
  task = makeClassifTask("test", data = Sonar, target = "Class")
  task$env$data$V1[1:2] = NA
  mod = train(lrn, task)
  p = predict(mod, task)
  expect_class(getPredictionDump(p), "dump.frames")
})

test_that("error dump is created in resample", {
  lrn = makeLearner("classif.__mlrmocklearners__2", alpha = 0, config = list(on.learner.error = "quiet", on.error.dump = TRUE))
  r = holdout(lrn, multiclass.task)
  expect_equal(length(getRRDump(r)), 1)
  expect_class(getRRDump(r)[[1]]$train, "dump.frames")

  lrn = makeLearner("classif.knn", config = list(on.learner.error = "quiet", on.error.dump = TRUE))
  lrn$properties = c(lrn$properties, "missings")
  task = makeClassifTask("test", data = Sonar, target = "Class")
  task$env$data$V1 = NA
  r = bootstrapB632plus(lrn, task, iters = 2)
  expect_equal(length(getRRDump(r)), 2)
  expect_class(getRRDump(r)[[1]]$predict.test, "dump.frames")
  expect_class(getRRDump(r)[[1]]$predict.train, "dump.frames")
})


test_that("error dump is created during tune", {
  mlr.options = getMlrOptions()
  ps = makeParamSet(
    makeDiscreteParam("alpha", values = c(1, 0))
  )
  configureMlr(on.learner.error = "quiet", on.error.dump = TRUE)
  ctrl = makeTuneControlGrid()
  lrn = makeLearner("classif.__mlrmocklearners__2")
  nodumplrn = makeLearner("classif.__mlrmocklearners__2", config = list(on.error.dump = FALSE))
  z = tuneParams(lrn, multiclass.task, hout, par.set = ps, control = ctrl,
    measures = getDefaultMeasure(multiclass.task))

  expect_equal(length(getOptPathEl(z$opt.path, 1)$extra$.dump), 1)
  expect_equal(getOptPathEl(z$opt.path, 1)$extra$.dump[[1]], list())
  expect_class(getOptPathEl(z$opt.path, 2)$extra$.dump[[1]]$train, "dump.frames")

  z = tuneParams(nodumplrn, multiclass.task, hout, par.set = ps, control = ctrl,
    measures = getDefaultMeasure(multiclass.task))

  expect_equal(length(getOptPathEl(z$opt.path, 1)$extra$.dump), 1)
  expect_equal(getOptPathEl(z$opt.path, 1)$extra$.dump[[1]], list())
  expect_equal(length(getOptPathEl(z$opt.path, 2)$extra$.dump), 1)
  expect_equal(getOptPathEl(z$opt.path, 2)$extra$.dump[[1]], list())

  lrn = makeLearner("classif.knn")
  lrn$properties = c(lrn$properties, "missings")
  task = makeClassifTask("test", data = Sonar, target = "Class")
  task$env$data$V1 = NA
  ctrl = makeTuneControlRandom(maxit = 2)
  ps = makeParamSet(
    makeIntegerParam("k", lower = 1, upper = 5)
  )
  z = tuneParams(lrn, task, makeResampleDesc("Bootstrap", predict = "both", iters = 2),
    par.set = ps, control = ctrl, measures = getDefaultMeasure(multiclass.task))
  expect_equal(length(getOptPathEl(z$opt.path, 1)$extra$.dump), 2)
  expect_class(getOptPathEl(z$opt.path, 1)$extra$.dump[[1]]$predict.test, "dump.frames")
  expect_class(getOptPathEl(z$opt.path, 1)$extra$.dump[[1]]$predict.train, "dump.frames")
  do.call(configureMlr, mlr.options)
})
