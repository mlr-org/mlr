context("FDA_PreprocWrapper")

test_that("FDA_PreprocWrapper", {
  taskFa = convertFDATaskToNormalTask(task = gunpoint.task, method = "fourier",
    pars = list(fft.coeff = "amplitude"))
  taskFp = convertFDATaskToNormalTask(task = gunpoint.task, method = "fourier",
    pars = list(fft.coeff = "phase"))
  A = getTaskData(taskFa)
  A = A[, !names(A) %in% "X1"]

  lrn = makeFDAClassifPreprocWrapper(learner = makeLearner('classif.ranger'))
})
