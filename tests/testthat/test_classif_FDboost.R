context("FDA_classif_FDboost")


test_that("test if classif_FDboost train works", {
  requirePackagesOrSkip("FDboost", default.method = "load")
  lrn = makeLearner("classif.FDboost", knots = 40L, df = 4L, mstop = 100L)
  set.seed(getOption("mlr.debug.seed"))
  mlr.mod = train(lrn, fda.binary.gp.task)
  prd = predict(mlr.mod, newdata = getTaskData(fda.binary.gp.task, functionals.as = "matrix"))

})
