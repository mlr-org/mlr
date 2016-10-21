context("resample getter work")

test_that("resample getter work", {
  r1 = resample(makeLearner("classif.rpart"), binaryclass.task, cv2)
  r2 = resample(makeLearner("classif.rpart"), binaryclass.task, cv2, keep.pred = FALSE)

  # getRRPredictions
  expect_equal(getRRPredictions(r1), r1$pred)
  expect_error(getRRPredictions(r2), "keep.pred = FALSE")

  # getRRTaskDescription
  expect_equal(getRRTaskDescription(r1), getTaskDescription(binaryclass.task))
})
