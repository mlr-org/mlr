
test_that("resample works with weights", {
  task = makeClassifTask(data = iris, target = "Species", weights = as.integer(iris$Species))
  res = resample(task = task, learner = "classif.rpart", resampling = makeResampleDesc("CV", iters = 2))
  expect_s3_class(res$pred, "ResamplePrediction")
})
