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

test_that("generateROCRCurvesData", {
  lrn1 = makeLearner("classif.rpart", predict.type = "prob")
  lrn2 = makeLearner("classif.lda", predict.type = "prob")
  lrns = list(lrn1, lrn2)
  m1 = train(lrn1, binaryclass.task)
  p1 = predict(m1, binaryclass.task)
  d = generateROCRCurvesData(p1)
  plotROCRCurves(d) # Prediction

  m2 = train(lrn2, binaryclass.task)
  p2 = predict(m2, binaryclass.task)
  d2 = generateROCRCurvesData(list("lda" = p1, "rpart" = p2)) # list of Predictions
  plotROCRCurves(d)
  plotROCRCurvesGGVIS(d)

  br = benchmark(lrn2, binaryclass.task, resampling = makeResampleDesc("Holdout"))
  d3 = generateROCRCurvesData(br)
  plotROCRCurves(d) # BenchmarkResult
  plotROCRCurvesGGVIS(d)

  rs = lapply(lrns, crossval, task = binaryclass.task)	# list of ResampleResult's
  names(rs) = c("a", "b")
  d = generateROCRCurvesData(rs)
  plotROCRCurves(d)
})
