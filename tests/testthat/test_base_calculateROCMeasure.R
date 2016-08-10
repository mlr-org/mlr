context("calculateROCMeasures")

test_that("calculateROCMeasures", {
  rdesc = makeResampleDesc("CV", iters=3)
  pred = resample(makeLearner("classif.rpart"), binaryclass.task, rdesc)
  r = calculateROCMeasures(pred$pred)
  
  expect_list(r$measures, types = "double", any.missing = FALSE, len = 12)
  expect_matrix(r$confusionMatrix, mode = "double", any.missing = FALSE, nrows = 2, ncols = 2)
  expect_true(all(rowSums(r$confusionMatrix) == 1))
  
  response = getPredictionResponse(pred$pred)
  truth = getPredictionTruth(pred$pred)
  positive = pred$pred$task.desc$positive
  negative = pred$pred$task.desc$negative
  
  expect_equal(r$measures$TPR, measureTPR(truth, response, positive))
  expect_equal(r$measures$FNR, measureFNR(truth, response, negative, positive))
  expect_equal(r$measures$FDR, measureFDR(truth, response, positive))
  expect_equal(r$measures$TNR, measureTNR(truth, response, negative))
  expect_equal(r$measures$PPV, measurePPV(truth, response, positive))
  expect_equal(r$measures$FDR, 1 - measurePPV(truth, response, positive))
  expect_equal(r$measures$NPV, measureNPV(truth, response, negative))
  expect_equal(r$measures$FOR, 1 - measureNPV(truth, response, negative))
  expect_equal(r$measures$ACC, measureACC(truth, response))
})

