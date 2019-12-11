context("parallel_all")

test_that("parallel resampling", {
  doit = function(mode, level) {
    lrn = makeLearner("classif.rpart")
    rdesc = makeResampleDesc("CV", iters = 2L)
    on.exit(parallelStop())
    parallelStart(mode = mode, cpus = 2L, level = level, show.info = FALSE)
    r = resample(lrn, multiclass.task, rdesc)
    expect_true(!is.na(r$aggr[1]))
  }
  if (Sys.info()["sysname"] != "Windows") {
    doit("multicore", as.character(NA))
    doit("multicore", "mlr.resample")
    doit("multicore", "mlr.tuneParams")
  }
  doit("socket", as.character(NA))
  doit("socket", "mlr.resample")
  doit("socket", "mlr.tuneParams")
})

test_that("parallel tuning", {
  doit = function(mode, level) {
    lrn = makeLearner("classif.rpart")
    rdesc = makeResampleDesc("CV", iters = 2L)
    ps = makeParamSet(makeDiscreteParam("cp", values = c(0.01, 0.05)))
    ctrl = makeTuneControlGrid()
    on.exit(parallelStop())
    parallelStart(mode = mode, cpus = 2L, level = level, show.info = FALSE)
    res = tuneParams(lrn, multiclass.task, rdesc, par.set = ps, control = ctrl)
    expect_true(!is.na(res$y))
  }
  if (Sys.info()["sysname"] != "Windows") {
    doit("multicore", as.character(NA))
    doit("multicore", "mlr.resample")
    doit("multicore", "mlr.tuneParams")
  }
  doit("socket", as.character(NA))
  doit("socket", "mlr.resample")
  doit("socket", "mlr.tuneParams")
})

test_that("parallel featsel", {
  doit = function(mode, level) {
    lrn = makeLearner("classif.rpart")
    rdesc = makeResampleDesc("CV", iters = 2L)
    ctrl = makeFeatSelControlRandom(maxit = 2L)
    on.exit(parallelStop())
    parallelStart(mode = mode, cpus = 2L, level = level, show.info = FALSE)
    res = selectFeatures(lrn, multiclass.task, rdesc, control = ctrl)
    expect_true(!is.na(res$y))
  }
  if (Sys.info()["sysname"] != "Windows") {
    doit("multicore", as.character(NA))
    doit("multicore", "mlr.resample")
    doit("multicore", "mlr.tuneParams")
  }
  doit("socket", as.character(NA))
  doit("socket", "mlr.resample")
  doit("socket", "mlr.tuneParams")
})

test_that("parallel exporting of options works", {
  doit = function(mode, level) {

    data = iris
    data[, 1] = 1 # this is going to crash lda
    task = makeClassifTask(data = data, target = "Species")
    lrn = makeLearner("classif.lda")
    rdesc = makeResampleDesc("CV", iters = 3)
    configureMlr(on.learner.error = "warn")
    on.exit(configureMlr(on.learner.error = "stop"))
    parallelStart(mode = mode, cpus = 2L, level = level, show.info = FALSE)
    on.exit(parallelStop())
    # if the option is not exported, we cannot pass the next line without error
    # on slave
    r = resample(lrn, task, rdesc)
  }
  expect_silent(doit("socket", as.character(NA)))
  # make sure
  configureMlr(on.learner.error = "stop")
})

test_that("parallel partial dependence", {
  doit = function(mode) {
    lrn = makeLearner("regr.rpart")
    fit = train(lrn, regr.task)
    on.exit(parallelStop())
    parallelStart(mode = mode, cpus = 2L, show.info = FALSE)
    pd = generatePartialDependenceData(fit, regr.task, "lstat")
    expect_true(ncol(pd$data) == 2L)
  }
  if (Sys.info()["sysname"] != "Windows") {
    doit("multicore")
  }
  doit("socket")
})

test_that("parallel ensembles", {
  doit = function(mode, level) {

    on.exit(parallelStop())
    parallelStart(mode = mode, cpus = 2L, show.info = FALSE)

    ## bagging wrapper
    lrn = makeBaggingWrapper(makeLearner("regr.rpart"), bw.iters = 2L)
    fit = train(lrn, regr.task)
    models = getLearnerModel(fit, more.unwrap = TRUE)
    expect_equal(length(models), 2L)
    expect_equal(class(models[[1]]), "rpart")
    p = predict(fit, regr.task)

    ## multiclass wrapper
    lrn = makeMulticlassWrapper(makeLearner("classif.rpart"))
    fit = train(lrn, multiclass.task)
    models = getLearnerModel(fit)
    expect_equal(length(models), length(getTaskClassLevels(multiclass.task)))
    levs = do.call("rbind", extractSubList(models, "factor.levels"))
    expect_equal(unique(levs[, 1]), "-1")
    expect_equal(unique(levs[, 2]), "1")
    p = predict(fit, multiclass.task)

    ## overbagging wrapper
    lrn = makeOverBaggingWrapper(makeLearner("classif.rpart"), 2L)
    fit = train(lrn, binaryclass.task)
    models = getLearnerModel(fit)
    expect_equal(length(models), 2L)
    p = predict(fit, binaryclass.task) ## calls predictHomogeneousEnsemble

    ## costsensregrwrapper
    lrn = makeCostSensRegrWrapper(makeLearner("regr.rpart"))
    fit = train(lrn, costsens.task)
    models = getLearnerModel(fit)
    expect_equal(length(models), ncol(getTaskCosts(costsens.task)))
    p = predict(fit, costsens.task)

    ## MultilabelBinaryRelevanceWrapper
    lrn = makeMultilabelBinaryRelevanceWrapper("classif.rpart")
    lrn = setPredictType(lrn, "prob")
    fit = train(lrn, multilabel.task)
    p = predict(fit, multilabel.task)
  }

  ## CostSensWeightedPairsWrapper
  if (Sys.info()["sysname"] != "Windows") {
    doit("multicore", "mlr.ensemble")
  }
  doit("socket", "mlr.ensemble")
})
