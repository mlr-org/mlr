context("rocr")

test_that("ROCR", {
  lrn = makeLearner("classif.lda", predict.type="prob")
  m = train(lrn, binaryclass.task)
  p = predict(m, binaryclass.task)
  a1 = performance(p, mlr::auc)
  p = asROCRPrediction(p)
  a2 = ROCR::performance(p, "auc")@y.values[[1]]
  expect_true(a1 == a2)
})
