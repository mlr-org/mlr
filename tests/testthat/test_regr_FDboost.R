context("FDA_regr_FDboost")
# predict the heat value of fossil fuels using spectral data, one spectrum is
# ultra-violet-visible (UV-VIS), measured at 1335 wavelengths(lambda = 1/f$), the ohter a near infrared
# spectrum(NIR), measured at 2307 wavelengths(lambda = 1/f$). The distance for both data are
# not equal distance in wavelegnths.

test_that("regr_FDboost is equal to reference", {
  requirePackagesOrSkip("FDboost", default.method = "load")
  lrn = makeLearner("regr.FDboost", knots = 40L, df = 4L, mstop = 100L)
  set.seed(getOption("mlr.debug.seed"))
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
  set.seed(getOption("mlr.debug.seed"))
  true.mod = FDboost::FDboost(frm, data = mat.list,
    timeformula = ~bols(1), control = ctrl, family = mboost::Gaussian())


  prd = predict(mlr.mod, newdata = getTaskData(fda.regr.fs.task,
    functionals.as = "matrix"))
  prd2 = predict(true.mod, as.list(getTaskData(fda.regr.fs.task,
    functionals.as = "matrix")))
  expect_equal(prd$data$response, prd2)


})
