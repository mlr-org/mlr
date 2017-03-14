context("resample getter work")

test_that("resample getter work", {
  lrn = makeLearner("classif.rpart")
  r1 = resample(lrn, binaryclass.task, cv2)
  r2 = resample(lrn, binaryclass.task, cv2, keep.pred = FALSE)

  # getRRPredictions
  expect_equal(getRRPredictions(r1), r1$pred)
  expect_error(getRRPredictions(r2), "keep.pred = FALSE")

  # getRRTaskDesc
  expect_equal(getRRTaskDesc(r1), getTaskDesc(binaryclass.task))

  # getRRPredictionList
  r1 = resample(lrn, binaryclass.task, makeResampleDesc("CV", iters = 2, predict = "test"))
  r2 = resample(lrn, binaryclass.task, makeResampleDesc("CV", iters = 2, predict = "both"))
  # FIXME: add check for "train" after https://github.com/mlr-org/mlr/issues/1284 has been fixed
  #r3 = resample(lrn, binaryclass.task, makeResampleDesc("CV", iters = 2, predict = "train"), setAggregation(mmce, train.mean))

  # check if structure is correct
  expect_named(getRRPredictionList(r1), c("train", "test"))
  expect_null(getRRPredictionList(r1)$train)
  expect_equal(length(getRRPredictionList(r1)$test), 2)

  expect_named(getRRPredictionList(r2), c("train", "test"))
  expect_equal(length(getRRPredictionList(r2)$test), 2)
  expect_equal(length(getRRPredictionList(r2)$train), 2)

  # check if performance value is correct
  expect_equal(r1$measures.test$mmce, unname(vnapply(getRRPredictionList(r1)$test, performance, mmce)))
  expect_equal(r2$measures.train$mmce, unname(vnapply(getRRPredictionList(r2)$train, performance, mmce)))
  expect_equal(r2$measures.test$mmce, unname(vnapply(getRRPredictionList(r2)$test, performance, mmce)))
})
