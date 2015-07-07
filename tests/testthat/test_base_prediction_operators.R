context("prediction operators")

mypredict = function(task) predict(train(sprintf("%s.rpart", task$task.desc$type), task), task)
classif.pred = mypredict(binaryclass.task)
regr.pred = mypredict(regr.task)
surv.pred = mypredict(surv.task)
cluster.pred = predict(train("cluster.kmeans", agri.task), agri.task)

test_that("getPredictionResponse", {
  expect_true(is.factor(getPredictionResponse(classif.pred)))
  expect_true(is.numeric(getPredictionResponse(regr.pred)))
  expect_true(is.numeric(getPredictionResponse(surv.pred)))
  expect_true(is.integer(getPredictionResponse(cluster.pred)))
})

test_that("getPredictionTruth", {
  expect_true(is.factor(getPredictionTruth(classif.pred)))
  expect_true(is.numeric(getPredictionTruth(regr.pred)))
  expect_true(is.Surv(getPredictionTruth(surv.pred)))
  expect_error(getPredictionTruth(cluster.pred), "no truth")
})

test_that("getPredictionProbabilities", {
  expect_error(getPredictionProbabilities(classif.pred, "R"), "Probabilities")
  expect_error(getPredictionProbabilities(regr.pred, "xxx"), "ClassifTask")
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  pred = predict(train(lrn, binaryclass.task), binaryclass.task)
  expect_true(is.numeric(getPredictionProbabilities(pred, "R")))
})
