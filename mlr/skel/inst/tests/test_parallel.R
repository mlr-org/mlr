context("parallel resampling")

if (interactive()) {
  
test_that("parallel resampling", {
  doit = function(mode, level) {
    lrn = makeLearner("classif.rpart")
    rdesc = makeResampleDesc("CV", iters = 2L)
    parallelStart(mode=mode, cpus = 2L, level=level)
    r = resample(lrn, multiclass.task, rdesc)
    parallelStop()
    expect_true(!is.na(r$aggr[1]))
  }
  doit("multicore", as.character(NA))
  doit("multicore", "resample")
  doit("multicore", "foo")
  doit("snowfall", as.character(NA))
  doit("snowfall", "resample")
  doit("snowfall", "foo")
})
          
}