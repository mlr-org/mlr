context("aggregations")

test_that("aggregations", {
  ms = list(mmce, acc, tp, fp, tn, fn, tpr, fpr, tnr, fnr, ppv, npv, mcc, f1, auc)
  rdesc = makeResampleDesc("CV", iters = 2)
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  r = resample(lrn, task = binaryclass.task, resampling = rdesc, measures = ms)
  a = r$aggr
  expect_equal(length(a), length(ms))
  expect_true(!any(is.na(as.logical(a))))
})

