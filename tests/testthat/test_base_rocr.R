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
  d1 = generateROCRCurvesData(p1)
  plotROCRCurves(d1) # Prediction

  m2 = train(lrn2, binaryclass.task)
  p2 = predict(m2, binaryclass.task)
  d2 = generateROCRCurvesData(list("lda" = p1, "rpart" = p2)) # list of Predictions
  plotROCRCurves(d2)

  br = benchmark(lrn2, binaryclass.task, resampling = makeResampleDesc("Holdout"))
  d3 = generateROCRCurvesData(br)
  plotROCRCurves(d3) # BenchmarkResult

  rs = lapply(lrns, holdout, task = binaryclass.task)	# list of ResampleResult's
  names(rs) = c("a", "b")
  d4 = generateROCRCurvesData(rs)
  dn = generateROCRCurvesData(rs, avg = "none")
  dh = generateROCRCurvesData(rs, avg = "horizontal")
  dv = generateROCRCurvesData(rs, avg = "vertical")
  plotROCRCurves(d4)
  plotROCRCurves(dn)
  plotROCRCurves(dh)
  plotROCRCurves(dv)
})
