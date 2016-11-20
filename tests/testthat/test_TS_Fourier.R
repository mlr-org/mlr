context("Ts_Fourier")

test_that("Ts_Fourier", {

  gp = data.frame(v1  =  t(1:4), X1= as.factor(1))
  #gp = data.frame(v1 = 1:5, v2 = 2:6, v3 = 3:7, v4 = 4:8, X1= as.factor(c(-1,1,1,-1, 1)))

  taskTs = makeTimeSeriesClassifTask(data = gp, target = "X1", positive = "1")
  taskFa = makeTSFeaturesClassifTask(task = taskTs, method = "fourier", pars = list(fft.coeff = "amplitude"))
  taskFp = makeTSFeaturesClassifTask(task = taskTs, method = "fourier", pars = list(fft.coeff = "phase"))

  raw = as.matrix(gp[, !names(gp) %in% "X1"])
  pp = as.data.frame(t(apply(raw,1, fft)))
  B = getFourierAmplitude(pp)
  C = getFourierPhase(pp)

  A = getTaskData(taskFa)
  A = A[, !names(A) %in% "X1"]
  D = getTaskData(taskFp)
  D = D[, !names(D) %in% "X1"]

  expect_true(all(A == B))
  expect_true(all(C == D))
})
