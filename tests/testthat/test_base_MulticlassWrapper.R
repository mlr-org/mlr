context("MulticlassWrapper")

test_that("MulticlassWrapper", {
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeLearner("classif.lqa")
  lrn1.w = makeMulticlassWrapper(lrn1)
  lrn2.w = makeMulticlassWrapper(lrn2, mcw.method = "onevsone")
  m1 = train(lrn1.w, multiclass.task)
  m2 = train(lrn2.w, multiclass.task)
  expect_true(!inherits(m1, "FailureModel"))
  expect_true(!inherits(m2, "FailureModel"))
  rdesc = makeResampleDesc("CV", iters = 2)
  r1 = resample(lrn1.w, multiclass.task, rdesc)
  r2 = resample(lrn2.w, multiclass.task, rdesc)
  expect_true(r1$aggr[[1L]] < 0.2)
  expect_true(r2$aggr[[1L]] < 0.2)
})


