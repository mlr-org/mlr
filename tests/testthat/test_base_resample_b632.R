context("resample_b632")

test_that("b632", {
  res = makeResampleDesc("Bootstrap", iters = 2, predict = "both")
  m = setAggregation(mmce, b632)
  r = resample(makeLearner("classif.rpart"), task = binaryclass.task,
    resampling = res, measure = m)
  m1 = r$measures.train
  m2 = r$measures.test
  p = as.data.frame(r$pred)
  ls11 = p[p$set == "train" & p$iter == 1, c("truth", "response")]
  ls12 = p[p$set == "test" & p$iter == 1, c("truth", "response")]
  ls1 = 0.368 * mean(ls11[, 1] != ls11[, 2]) + 0.632 * mean(ls12[, 1] != ls12[, 2])
  ls21 = p[p$set == "train" & p$iter == 2, c("truth", "response")]
  ls22 = p[p$set == "test" & p$iter == 2, c("truth", "response")]
  ls2 = 0.368 * mean(ls21[, 1] != ls21[, 2]) + 0.632 * mean(ls22[, 1] != ls22[, 2])
  ag = r$aggr
  expect_equal(mean(c(ls1, ls2)), ag[["mmce.b632"]])
})
