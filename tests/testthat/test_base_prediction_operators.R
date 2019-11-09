context("prediction_operators")

mypredict = function(task) predict(train(sprintf("%s.rpart", getTaskType(task)), task), task)
classif.pred = mypredict(binaryclass.task)
multilabel.pred = predict(train("multilabel.rFerns", multilabel.task), multilabel.task)
lrn1 = makeLearner("classif.rpart")
lrn2 = makeMultilabelBinaryRelevanceWrapper(lrn1)
multilabel.pred2 = predict(train(lrn2, multilabel.task), multilabel.task)
regr.pred = mypredict(regr.task)
surv.pred = mypredict(surv.task)
cluster.pred = predict(train("cluster.kmeans", agri.task), agri.task)

test_that("getPredictionResponse", {
  expect_true(is.factor(getPredictionResponse(classif.pred)))
  expect_true(is.logical(getPredictionResponse(multilabel.pred)))
  expect_true(is.logical(getPredictionResponse(multilabel.pred2)))
  expect_true(is.numeric(getPredictionResponse(regr.pred)))
  expect_true(is.numeric(getPredictionResponse(surv.pred)))
  expect_true(is.integer(getPredictionResponse(cluster.pred)))
})

test_that("getPredictionTruth", {
  expect_true(is.factor(getPredictionTruth(classif.pred)))
  expect_true(is.logical(getPredictionTruth(multilabel.pred)))
  expect_true(is.logical(getPredictionTruth(multilabel.pred2)))
  expect_true(is.numeric(getPredictionTruth(regr.pred)))
  expect_true(is.Surv(getPredictionTruth(surv.pred)))
  expect_error(getPredictionTruth(cluster.pred), "no truth")
})

test_that("getPredictionProbabilities", {
  expect_error(getPredictionProbabilities(classif.pred, "R"), "Probabilities")
  expect_error(getPredictionProbabilities(multilabel.pred2, "y2"), "Probabilities")
  expect_error(getPredictionProbabilities(regr.pred, "xxx"), "ClassifTask")
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  classif.pred = predict(train(lrn, binaryclass.task), binaryclass.task)
  expect_true(is.numeric(getPredictionProbabilities(classif.pred, "R")))

  lrn2 = makeMultilabelBinaryRelevanceWrapper(lrn)
  multilabel.pred2 = predict(train(lrn2, multilabel.task), multilabel.task)
  expect_true(is.numeric(getPredictionProbabilities(multilabel.pred2, "y2")))
})
