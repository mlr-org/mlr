context("generateThreshVsPerfData")

test_that("generateThreshVsPerfData", {
  ## single prediction
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  mod = train(lrn, binaryclass.task)
  pred = predict(mod, binaryclass.task)
  pvs = generateThreshVsPerfData(pred, list(tpr, fpr))
  plotThreshVsPerf(pvs)
  ## plotThreshVsPerfGGVIS(pvs)

  ## resample prediction
  rdesc = makeResampleDesc("CV", iters = 3)
  r = resample(lrn, binaryclass.task, rdesc)
  pvs = generateThreshVsPerfData(r, list(tpr, fpr))
  plotThreshVsPerf(pvs)
  ## plotThreshVsPerfGGVIS(pvs)

  ## benchmark result
  lrns = list(lrn, makeLearner("classif.lda", predict.type = "prob"))
  rdesc = makeResampleDesc("CV", iters = 3)
  res = benchmark(lrns, binaryclass.task, rdesc, show.info = FALSE)
  pvs = generateThreshVsPerfData(res, list(tpr, fpr))
  plotThreshVsPerf(pvs)
  ## plotThreshVsPerfGGVIS(pvs)

  ## list of resample predictions
  rs = lapply(lrns, crossval, task = binaryclass.task)
  names(rs) = c("a", "b")
  pvs = generateThreshVsPerfData(rs, list(tpr, fpr))
  plotThreshVsPerf(pvs)
  ## plotThreshVsPerfGGVIS(pvs)
})
