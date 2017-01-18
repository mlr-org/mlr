context("Ts_Fourier")

test_that("Ts_Fourier", {

  #gp = data.frame(v1  =  t(1:4), X1= as.factor(1))
  gp = data.frame(v1 = 1:5, v2 = 2:6, v3 = 3:7, v4 = 4:8, X1= as.factor(c(-1,1,1,-1, 1)))

  taskTs = makeTimeSeriesClassifTask(data = gp, target = "X1", positive = "1")
  refData = getTaskData(taskTs, target.extra = TRUE)

  taskFa = convertTSTaskToNormalTask(task = taskTs, method = "fourier", pars = list(fft.coeff = "amplitude"))
  faData = getTaskData(taskFa, target.extra = TRUE)
  taskFp = convertTSTaskToNormalTask(task = taskTs, method = "fourier", pars = list(fft.coeff = "phase"))
  fpData = getTaskData(taskFp, target.extra = TRUE)

  expect_true(all(dim(refData$data) == dim(faData$data)))
  expect_true(all(dim(refData$data) == dim(fpData$data)))

  #phase (arctan(...) in range(-pi/2, pi/2) )
  expect_true(all(fpData$data < pi/2 & fpData$data > -pi/2))

  #amplitude sqrt(Re^2 + Im^2) >= 0
  expect_true(all(faData$data >= 0))

})
