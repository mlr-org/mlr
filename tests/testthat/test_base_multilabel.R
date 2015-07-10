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
  # Learner with Impute-Preprocessing
  lrn = makeImputeWrapper(lrn, classes = list(integer = imputeMedian(), numeric = imputeMedian(), factor = imputeConstant("Const")))
  multilabel.df2 = multilabel.df
  multilabel.df2[c(2,10,14), c(1,5)] = NA
  multilabel.task2 = makeMultilabelTask("multilabel", data = multilabel.df2, target = multilabel.target)
  mod = train(lrn, multilabel.task2)
  pred = predict(mod, multilabel.task2) 
  # Learner with Hyperparameters
  lrn = makeLearner("multilabel.rFerns", par.vals = list(depth=6, ferns=100))
  mod = train(lrn, multilabel.task)
  pred = predict(mod, multilabel.task) 
})


test_that("MultilabelBinaryRelevanceWrapper", {
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeMultilabelBinaryRelevanceWrapper(lrn1)

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
  lrn2 = makeMultilabelBinaryRelevanceWrapper(lrn1)
  r = holdout(lrn2, multilabel.task)
  expect_true(!is.na(r$aggr))
  p = getPredictionProbabilities(r$pred)
  expect_true(is.data.frame(p))
  p = getPredictionProbabilities(r$pred, getTaskClassLevels(multilabel.task))
  expect_true(is.data.frame(p))

  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeMultilabelBinaryRelevanceWrapper(lrn1)
  lrn2 = setPredictType(lrn2, "prob")
  r = holdout(lrn2, multilabel.task)
  expect_true(!is.na(r$aggr))
  # check some stuff for probs
  cls = getTaskClassLevels(multilabel.task)
  p = getPredictionProbabilities(r$pred)
  expect_true(is.data.frame(p))
  expect_equal(colnames(p), cls)
  p = getPredictionProbabilities(r$pred, cls[1L])
  expect_true(is.numeric(p))
  # now test that we can tune the thresholds
  tr = tuneThreshold(r$pred, nsub = 2L, control= list(maxit = 2L))
  expect_true(!is.na(tr$perf))
  expect_equal(length(tr$th), length(getTaskClassLevels(multilabel.task)))
  # Learner with Impute-Preprocessing
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeMultilabelBinaryRelevanceWrapper(lrn1)
  lrn2 = makeImputeWrapper(lrn2, classes = list(integer = imputeMedian(), numeric = imputeMedian(), factor = imputeConstant("Const")))
  multilabel.df2 = multilabel.df
  multilabel.df2[c(2,10,14), c(1,5)] = NA
  multilabel.task2 = makeMultilabelTask("multilabel", data = multilabel.df2, target = multilabel.target)
  mod = train(lrn2, multilabel.task2)
  pred = predict(mod, multilabel.task2) 
  # Learner with Hyperparameters
  lrn1 = makeLearner("classif.rpart", par.vals = list(minsplit = 10, cp = 0.005))
  lrn2 = makeMultilabelBinaryRelevanceWrapper(lrn1)
  mod = train(lrn2, multilabel.task)
  pred = predict(mod, multilabel.task) 
})




