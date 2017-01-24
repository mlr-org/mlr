context("TS_PreprocWrapper")

test_that("TS_PreprocWrapper", {
  taskTs = makeTimeSeriesClassifTask(data = global_var4TS_gp, target = "X1", positive = "1")
  taskFa = convertTSTaskToNormalTask(task = taskTs, method = "fourier", pars = list(fft.coeff = "amplitude"))
  taskFp = convertTSTaskToNormalTask(task = taskTs, method = "fourier", pars = list(fft.coeff = "phase"))
  A = getTaskData(taskFa)
  A = A[, !names(A) %in% "X1"]
  
  lrn = makeTSPreprocWrapper(learner = makeLearner('classif.ranger'))
  
})
