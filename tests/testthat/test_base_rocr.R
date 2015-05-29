context("ROCR")

test_that("ROCR", {
  lrn = makeLearner("classif.lda", predict.type="prob")
  m = train(lrn, binaryclass.task)
  p = predict(m, binaryclass.task)
  a1 = performance(p, mlr::auc)
  p = asROCRPrediction(p)
  a2 = ROCR::performance(p, "auc")@y.values[[1]]
  expect_true(a1 == a2)
})

test_that("plotROCRCurves", {
  lrn1 = makeLearner("classif.rpart", predict.type = "prob")
  lrn2 = makeLearner("classif.lda", predict.type = "prob")
  lrns = list(lrn1, lrn2)
  m1 = train(lrn1, binaryclass.task)
  p1 = predict(m1, binaryclass.task)
  plotROCRCurves(p1)		## Prediction

  m2 = train(lrn2, binaryclass.task)
  p2 = predict(m2, binaryclass.task)
  plotROCRCurves(list("lda" = p1, "rpart" = p2))	## list of Predictions

  br = benchmark(lrn2, binaryclass.task, resampling = makeResampleDesc("Holdout"))
  plotROCRCurves(br)	## BenchmarkResult

  rs = lapply(lrns, holdout, task = binaryclass.task)
  names(rs) = c("a", "b")
  plotROCRCurves(rs)	## list of ResampleResult's
  plotROCRCurves(rs, avg = "none")
  plotROCRCurves(rs, avg = "horizontal")
  plotROCRCurves(rs, avg = "vertical")
})
