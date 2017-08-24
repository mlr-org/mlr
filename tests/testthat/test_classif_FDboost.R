context("FDA_classif_FDboost")


test_that("classif_FDboost is equal to reference", {
  requirePackagesOrSkip("FDboost", default.method = "load")
  lrn = makeLearner("classif.FDboost", knots = 40L, df = 4L, mstop = 100L)
  set.seed(getOption("mlr.debug.seed"))
  #mlr.mod = train(lrn, fda.classif.fs.task)
})
