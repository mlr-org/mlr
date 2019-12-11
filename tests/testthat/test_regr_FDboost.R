context("FDA_FDboost")

test_that("regr_FDboost is equal to reference", {
  requirePackagesOrSkip("FDboost", default.method = "load")

  lrn = makeLearner("regr.FDboost", knots = 40L, df = 4L, mstop = 100L)
  mlr.mod = train(lrn, fda.regr.fs.task)

  frm = as.formula(mlr.mod$learner.model$formulaFDboost)
  # Get the features in a data.frame and matrix column
  mat.list = getTaskData(fda.regr.fs.task, functionals.as = "matrix")
  # Add matricies for fd.grids
  fdns = colnames(getFunctionalFeatures(fda.regr.fs.task))
  fdg = namedList(fdns)
  fd.grids = lapply(fdns, function(name) seq_len(ncol(mat.list[, name])))
  names(fd.grids) = fdns
  fdg = setNames(fd.grids, stri_paste(fdns, ".grid"))
  mat.list = c(mat.list, fdg)
  ctrl = learnerArgsToControl(mboost::boost_control, mstop = 100L, nu = 0.1)
  true.mod = FDboost::FDboost(frm, data = mat.list,
    timeformula = ~ bols(1), control = ctrl, family = mboost::Gaussian())

  prd = predict(mlr.mod, newdata = getTaskData(fda.regr.fs.task,
    functionals.as = "matrix"))
  prd2 = predict(true.mod, as.list(getTaskData(fda.regr.fs.task,
    functionals.as = "matrix")))
  expect_equal(prd$data$response, prd2)
})
