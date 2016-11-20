context("Ts_Wavelets")

test_that("Ts_Wavelets", {

  #gp = data.frame(v1  =  t(1:4), X1= as.factor(1))
  gp = data.frame( X1= as.factor(c(-1,1,1,-1, 1)) ,v1 = 1:5, v2 = 12:16, v3 = 3:7, v4 = 4:8)

  taskTs = makeTimeSeriesClassifTask(data = gp, target = "X1", positive = "1")
  taskW = makeTSFeaturesClassifTask(task = taskTs, method = "wavelets", pars = list(filter = "haar"))
  taskWb = makeTSFeaturesClassifTask(task = taskTs, method = "wavelets", pars = list( filter = "d4", boundary = "reflection"))


  raw = gp[, !names(gp) %in% "X1"]
  bb = NULL
  for (i in seq_row(raw)) {
    a = t(raw[i,])
    #browser()
    wt = wavelets::dwt(a, filter = "haar", boundary = "periodic")
    bb = rbind(bb, unlist(c(wt@W,wt@V[[wt@level]])))
  }

  dd = NULL
  for (i in seq_row(raw)) {
    a = t(raw[i,])
    wt = wavelets::dwt(a, filter = "d4", boundary = "reflection")
    dd = rbind(dd, unlist(c(wt@W,wt@V[[wt@level]])))
  }

  aa = getTaskData(taskW)
  aa = aa[, !names(aa) %in% "X1"]

  cc = getTaskData(taskWb)
  cc = cc[, !names(cc) %in% "X1"]

  expect_true(all(bb == aa))
  expect_true(all(cc == dd))
})
