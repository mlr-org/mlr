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

test_that("testgroup.mean", {
  perf.test = 1:4
  group = c(1, 1, 2, 2)

  expect_equal(testgroup.mean$fun(NA, perf.test, NA, mean, group, NA), mean(c(mean(1:2), mean(3:4))))
})

test_that("testgroup.sd", {
  perf.test = 1:10
  group = c(rep(1, 5), rep(2, 5))

  expect_equal(testgroup.sd$fun(NA, perf.test, NA, mean, group, NA), sd(c(mean(1:5), mean(6:10))))
})
