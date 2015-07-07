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
  rdesc = makeResampleDesc("CV", iters = 2L)
  r = resample(lrn, binaryclass.task, rdesc)
  pvs = generateThreshVsPerfData(r, list(tpr, fpr))
  plotThreshVsPerf(pvs)
  ## plotThreshVsPerfGGVIS(pvs)

  ## benchmark result
  lrns = list(lrn, makeLearner("classif.lda", predict.type = "prob"))
  rdesc = makeResampleDesc("CV", iters = 2L)
  res = benchmark(lrns, binaryclass.task, rdesc, show.info = FALSE)
  pvs = generateThreshVsPerfData(res, list(tpr, fpr))
  plotThreshVsPerf(pvs)
  ## plotThreshVsPerfGGVIS(pvs)

  ## list of resample predictions
  rs = lapply(lrns, crossval, task = binaryclass.task, iters = 2L)
  names(rs) = c("a", "b")
  pvs = generateThreshVsPerfData(rs, list(tpr, fpr))
  plotThreshVsPerf(pvs)
  ## plotThreshVsPerfGGVIS(pvs)

  ## test prediction obj with custom measure
  classes = levels(getTaskTargets(binaryclass.task))
  mcm = matrix(sample(0:3, size = (length(classes))^2, TRUE), ncol = length(classes))
  rownames(mcm) = colnames(mcm) = classes
  costs = makeCostMeasure(id = "asym.costs", name = "Asymmetric costs",
                          minimize = TRUE, costs = mcm, binaryclass.task, combine = mean)
  pvs.custom = generateThreshVsPerfData(pred, costs)
  plotThreshVsPerf(pvs.custom)
  ## plotThreshVsPerfGGVIS(pvs.custom)
})
