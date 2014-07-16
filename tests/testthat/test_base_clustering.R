context("clustering")

test_that("clustering resample",  {
  rdesc = makeResampleDesc("Bootstrap", iters = 5)
  lrn = makeLearner("cluster.XMeans")
  res = resample(lrn, noclass.task, rdesc)

  expect_true(all(!is.na(res$measures.test)))
  expect_false(is.na(res$aggr))
})
