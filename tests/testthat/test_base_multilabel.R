context("multilabel")

test_that("multilabel", {
  lrn = makeLearner("multilabel.rFerns")

  # train predict eval
  mod = train(lrn, multilabel.task)
  pred = predict(mod, multilabel.task)
  p = performance(pred)
  expect_true(!is.na(p))
  # with newdata df
  pred = predict(mod, newdata = multilabel.df)
  p = performance(pred)
  expect_true(!is.na(p))
  # resample
  r = holdout(lrn, multilabel.task)
  expect_true(!is.na(r$aggr))
})


test_that("MultiLabelWrapper", {
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeMultilabelWrapper(lrn1)

  # train predict eval
  mod = train(lrn2, multilabel.task)
  pred = predict(mod, multilabel.task)
  p = performance(pred)
  expect_true(!is.na(p))
  # with newdata df
  pred = predict(mod, newdata = multilabel.df)
  p = performance(pred)
  expect_true(!is.na(p))
  # resample
  r = holdout(lrn2, multilabel.task)
  expect_true(!is.na(r$aggr))

  lrn1 = makeLearner("classif.rpart", predict.type = "prob")
  lrn2 = makeMultilabelWrapper(lrn1)
  r = holdout(lrn2, multilabel.task)
  expect_true(!is.na(r$aggr))
  p = getPredictionProbabilities(r$pred)
  expect_true(is.data.frame(p))
  p = getPredictionProbabilities(r$pred, getTaskClassLevels(multilabel.task))
  expect_true(is.data.frame(p))

  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeMultilabelWrapper(lrn1)
  lrn2 = setPredictType(lrn2, "prob")
  r = holdout(lrn2, multilabel.task)
  expect_true(!is.na(r$aggr))
  # check some stuff for probs
  cls = getTaskClassLevels(multilabel.task)
  p = getPredictionProbabilities(r$pred)
  expect_true(is.data.frame(p))
  expect_equal(colnames(p), cls)
  p = getPredictionProbabilities(r$pred, cls[1L])
  expect_true(is.data.frame(p))
})




