context("FDA_PreprocWrapper")

test_that("FDA_PreprocWrapper", {
  taskTs = makeFDAClassifTask(data = global_var4FDA_gp, target = "X1", positive = "1")
  taskFa = convertFDATaskToNormalTask(task = taskTs, method = "fourier", pars = list(fft.coeff = "amplitude"))
  taskFp = convertFDATaskToNormalTask(task = taskTs, method = "fourier", pars = list(fft.coeff = "phase"))
  A = getTaskData(taskFa)
  A = A[, !names(A) %in% "X1"]

  lrn = makeFDAPreprocWrapper(learner = makeLearner('classif.ranger'))

})
