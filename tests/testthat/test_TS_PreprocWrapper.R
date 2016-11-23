context("TS_PreprocWrapper")

test_that("TS_PreprocWrapper", {
  
  
  taskTs = makeTimeSeriesClassifTask(data = gp, target = "X1", positive = "1")
  taskFa = makeTSFeaturesClassifTask(task = taskTs, method = "fourier", pars = list(fft.coeff = "amplitude"))
  taskFp = makeTSFeaturesClassifTask(task = taskTs, method = "fourier", pars = list(fft.coeff = "phase"))
  A = getTaskData(taskFa)
  A = A[, !names(A) %in% "X1"]
  
})
