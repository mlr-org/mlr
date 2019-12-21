context("calculateROCMeasures")

test_that("calculateROCMeasures", {

  rdesc = makeResampleDesc("CV", iters = 3)
  pred = resample(makeLearner("classif.rpart"), binaryclass.task, rdesc)
  r = calculateROCMeasures(pred$pred)

  expect_list(r$measures, types = "double", any.missing = FALSE, len = 12)
  expect_matrix(r$confusion.matrix, mode = "double", any.missing = FALSE, nrows = 2, ncols = 2)
  expect_true(all(rowSums(r$confusion.matrix) == table(getTaskTargets(binaryclass.task))))

  response = getPredictionResponse(pred$pred)
  truth = getPredictionTruth(pred$pred)
  positive = pred$pred$task.desc$positive
  negative = pred$pred$task.desc$negative

  expect_equal(r$measures$tpr, measureTPR(truth, response, positive))
  expect_equal(r$measures$fnr, measureFNR(truth, response, negative, positive))
  expect_equal(r$measures$fdr, measureFDR(truth, response, positive))
  expect_equal(r$measures$tnr, measureTNR(truth, response, negative))
  expect_equal(r$measures$ppv, measurePPV(truth, response, positive))
  expect_equal(r$measures$fdr, 1 - measurePPV(truth, response, positive))
  expect_equal(r$measures$npv, measureNPV(truth, response, negative))
  expect_equal(r$measures$fomr, 1 - measureNPV(truth, response, negative))
  expect_equal(r$measures$acc, measureACC(truth, response))
})
