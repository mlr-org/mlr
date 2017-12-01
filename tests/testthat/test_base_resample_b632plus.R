context("resample_b632+")

test_that("b632+", {
  res = makeResampleDesc("Bootstrap", iters = 3, predict = "both")
  m = setAggregation(mmce, b632plus)
  r = resample(makeLearner("classif.lda"), multiclass.task, res, measures = m)
  x = r$aggr[["mmce.b632plus"]]
  expect_true(is.numeric(x) & length(x) == 1 && !is.na(x))
})
