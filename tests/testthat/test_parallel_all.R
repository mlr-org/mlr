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
    doit("mpi", as.character(NA))
    doit("mpi", "mlr.resample")
    doit("mpi", "mlr.tuneParams")
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
    doit("mpi", as.character(NA))
    doit("mpi", "mlr.resample")
    doit("mpi", "mlr.tuneParams")
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
    doit("mpi", as.character(NA))
    doit("mpi", "mlr.resample")
    doit("mpi", "mlr.tuneParams")
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
    # if the option is not exported, we cannot pass the next line without error on slave
    r = resample(lrn, task, rdesc)
  }
  doit("socket", as.character(NA))
  # make sure
  configureMlr(on.learner.error = "stop")
})

test_that("parallel partial predictions", {
  doit = function(level) {
    lrn = makeLearner("regr.rpart")
    fit = train(lrn, regr.task)
    on.exit(parallelStop())
    parallelStart(mode = mode, cpus = 2L, show.info = FALSE)
    pd = generatePartialPredictionData(fit, regr.task, "lstat", gridsize = 2L)
    expect_true(nrow(pd$data) == 2L)
  }
  if (Sys.info()["sysname"] != "Windows") {
    doit("multicore")
    doit("mpi")
  }
  doit("socket")
})
