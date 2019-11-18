context("RLearner_classif_FDboost")

test_that("test if classif_FDboost train works", {
  requirePackagesOrSkip("FDboost", default.method = "load")

  # MLR Learner
  lrn = makeLearner("classif.FDboost", knots = 40L, df = 4L, mstop = 100L)
  mlr.mod = train(lrn, fda.binary.gp.task)

  # Original Learner
  frm = as.formula(mlr.mod$learner.model$formulaFDboost)
  # Get the features in a data.frame and matrix column
  mat.list = getTaskData(fda.binary.gp.task, functionals.as = "matrix")
  # Add matricies for fd.grids
  fdns = colnames(getFunctionalFeatures(fda.binary.gp.task))
  fdg = namedList(fdns)
  fd.grids = lapply(fdns, function(name) seq_len(ncol(mat.list[, name])))
  names(fd.grids) = fdns
  fdg = setNames(fd.grids, stri_paste(fdns, ".grid"))
  mat.list = c(mat.list, fdg)
  ctrl = learnerArgsToControl(mboost::boost_control, mstop = 100L, nu = 0.1)
  set.seed(getOption("mlr.debug.seed"))
  true.mod = FDboost::FDboost(frm, data = mat.list,
    timeformula = ~ bols(1), control = ctrl, family = mboost::Binomial())

  prd = predict(mlr.mod, newdata = getTaskData(fda.binary.gp.task,
    functionals.as = "matrix"))
  prd2 = predict(true.mod, as.list(getTaskData(fda.binary.gp.task,
    functionals.as = "matrix")), type = "class")
  expect_equal(prd$data$response, prd2)

  # Check this also holds for probs
  lrn = makeLearner("classif.FDboost", knots = 40L, df = 4L, mstop = 100L, predict.type = "prob")
  mlr.mod = train(lrn, fda.binary.gp.task)

  prd = predict(mlr.mod, newdata = getTaskData(fda.binary.gp.task, functionals.as = "matrix"))
  prd2 = predict(true.mod, as.list(getTaskData(fda.binary.gp.task,
    functionals.as = "matrix")), type = "response")
  attributes(prd2) = NULL
  expect_equal(prd$data$prob.2, prd2)
})
