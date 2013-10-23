context("ROCR")

test_that("ROCR", {
  lrn = makeLearner("classif.lda", predict.type="prob")
  m = train(lrn, binaryclass.task)
  p = predict(m, binaryclass.task)
  a1 = performance(p, auc)
  p = asROCRPrediction(p)
  a2 = ROCR::performance(p, "auc")@y.values[[1]]
  expect_equal(a1, a2)
})