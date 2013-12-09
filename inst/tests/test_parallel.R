context("parallel resampling")

if (isExpensiveExampleOk()) {

test_that("parallel resampling", {
  doit = function(mode, level) {
    lrn = makeLearner("classif.rpart")
    rdesc = makeResampleDesc("CV", iters=2L)
    parallelStart(mode=mode, cpus=2L, level=level)
    r = resample(lrn, multiclass.task, rdesc)
    parallelStop()
    expect_true(!is.na(r$aggr[1]))
  }
  doit("multicore", as.character(NA))
  doit("multicore", "resample")
  doit("multicore", "foo")
  doit("socket", as.character(NA))
  doit("socket", "resample")
  doit("socket", "foo")
  doit("mpi", as.character(NA))
  doit("mpi", "resample")
  doit("mpi", "foo")
})

test_that("parallel tuning", {
  doit = function(mode, level) {
    lrn = makeLearner("classif.rpart")
    rdesc = makeResampleDesc("CV", iters = 2L)
    ps = makeParamSet(makeDiscreteParam("cp", values = c(0.01, 0.05)))
    ctrl = makeTuneControlGrid()
    parallelStart(mode=mode, cpus=2L, level=level)
    res = tuneParams(lrn, multiclass.task, rdesc, par.set=ps, control=ctrl)
    parallelStop()
    expect_true(!is.na(res$y))
  }
  doit("multicore", as.character(NA))
  doit("multicore", "resample")
  doit("multicore", "tuneParams")
  doit("socket", as.character(NA))
  doit("socket", "resample")
  doit("socket", "tuneParams")
  doit("mpi", as.character(NA))
  doit("mpi", "resample")
  doit("mpi", "tuneParams")
})

test_that("parallel featsel", {
  doit = function(mode, level) {
    lrn = makeLearner("classif.rpart")
    rdesc = makeResampleDesc("CV", iters = 2L)
    ctrl = makeFeatSelControlRandom(maxit=2L)
    parallelStart(mode=mode, cpus=2L, level=level)
    res = selectFeatures(lrn, multiclass.task, rdesc, control=ctrl)
    parallelStop()
    expect_true(!is.na(res$y))
  }
  doit("multicore", as.character(NA))
  doit("multicore", "resample")
  doit("multicore", "selectFeatures")
  doit("socket", as.character(NA))
  doit("socket", "resample")
  doit("socket", "selectFeatures")
  doit("mpi", as.character(NA))
  doit("mpi", "resample")
  doit("mpi", "selectFeatures")
})

}
