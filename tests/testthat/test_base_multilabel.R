context("multilabel")

test_that("multilabel task", {
  mt = multilabel.task
  expect_equal(getTaskTargetNames(mt), c("y1", "y2"))
  expect_equal(getTaskClassLevels(mt), c("y1", "y2"))
  expect_equal(getTaskFormula(mt), y1 + y2 ~ .)
  y = getTaskTargets(mt)
  expect_true(is.data.frame(y) && ncol(y) == 2L)
  expect_true(is.logical(y[, 1]) && is.logical(y[, 2L]))
  expect_equal(colnames(y), c("y1", "y2"))
})

test_that("multilabel learning", {
  lrn = makeLearner("multilabel.rFerns")

  # train predict eval
  mod = train(lrn, multilabel.task)
  pred = predict(mod, multilabel.task)
  p = performance(pred)
  expect_true(!is.na(p))
  pmulti = getMultilabelBinaryPerformances(pred, list(mmce, acc))
  expect_true(is.matrix(pmulti))
  expect_true(!any(is.na(pmulti)))
  expect_equal(rownames(pmulti), getTaskTargetNames(multilabel.task))
  expect_equal(colnames(pmulti), vcapply(list(mmce, acc), mlr:::measureAggrName))
  # with newdata df
  pred = predict(mod, newdata = multilabel.df)
  p = performance(pred)
  expect_true(!is.na(p))
  pmulti = getMultilabelBinaryPerformances(pred, list(mmce, acc))
  expect_true(!any(is.na(pmulti)))
  # resample
  r = holdout(lrn, multilabel.task)
  expect_true(!is.na(r$aggr))
  pmulti = getMultilabelBinaryPerformances(r$pred, list(mmce, acc))
  expect_true(!any(is.na(pmulti)))
  # Learner with Impute-Preprocessing
  lrn = makeImputeWrapper(lrn, classes = list(integer = imputeMedian(), numeric = imputeMedian(), factor = imputeConstant("Const")))
  multilabel.df2 = multilabel.df
  multilabel.df2[c(2, 10, 14), c(1, 5)] = NA
  multilabel.task2 = makeMultilabelTask("multilabel", data = multilabel.df2, target = multilabel.target)
  mod = train(lrn, multilabel.task2)
  pred = predict(mod, multilabel.task2)
  p = performance(pred)
  expect_true(!is.na(p))
  # Learner with Hyperparameters
  lrn = makeLearner("multilabel.rFerns", par.vals = list(depth = 6, ferns = 100))
  mod = train(lrn, multilabel.task)
  pred = predict(mod, multilabel.task)
  p = performance(pred)
  expect_true(!is.na(p))
})

test_that("MultilabelBinaryRelevanceWrapper with glmnet (#958)", {
  requirePackagesOrSkip("glmnet", default.method = "load")
  # multilabelBinaryRelevanceWrapper was not working properly for classif.glmnet, we had a bug here
  lrn = makeLearner("classif.glmnet", predict.type = "response")
  lrn2 = makeMultilabelBinaryRelevanceWrapper(lrn)
  mod = train(lrn2, multilabel.task)
  pred = predict(mod, multilabel.task)
  expect_error(pred, NA)
})

testMultilabelWrapper = function(fun, ...) {
  desc = fun("classif.rpart")$model.subclass[1]
  test_that(desc, {
    lrn1 = makeLearner("classif.rpart")
    lrn2 = fun(lrn1, ...)
    lrn2 = setPredictType(lrn2, "prob")
    # train predict eval
    mod = train(lrn2, multilabel.task)
    pred = predict(mod, multilabel.task)
    p = performance(pred)
    expect_true(!is.na(p))
    pmulti = getMultilabelBinaryPerformances(pred, list(mmce, auc))
    expect_true(!any(is.na(pmulti)))
    expect_true(is.matrix(pmulti))
    # with newdata df
    pred = predict(mod, newdata = multilabel.df)
    p = performance(pred)
    expect_true(!is.na(p))
    pmulti = getMultilabelBinaryPerformances(pred, list(mmce, auc))
    expect_true(is.matrix(pmulti))
    expect_true(!any(is.na(pmulti)))
    expect_equal(rownames(pmulti), getTaskTargetNames(multilabel.task))
    expect_equal(colnames(pmulti), vcapply(list(mmce, auc), mlr:::measureAggrName))
    # resample
    r = holdout(lrn2, multilabel.task)
    expect_true(!is.na(r$aggr))
    pmulti = getMultilabelBinaryPerformances(pred, list(mmce, auc))
    expect_true(is.matrix(pmulti))
    expect_true(!any(is.na(pmulti)))
    expect_equal(rownames(pmulti), getTaskTargetNames(multilabel.task))
    expect_equal(colnames(pmulti), vcapply(list(mmce, auc), mlr:::measureAggrName))
    lrn1 = makeLearner("classif.rpart", predict.type = "prob")
    lrn2 = fun(lrn1, ...)
    r = holdout(lrn2, multilabel.task)
    expect_true(!is.na(r$aggr))
    p = getPredictionProbabilities(r$pred)
    expect_true(is.data.frame(p))
    p = getPredictionProbabilities(r$pred, getTaskClassLevels(multilabel.task))
    expect_true(is.data.frame(p))

    lrn1 = makeLearner("classif.rpart")
    lrn2 = fun(lrn1, ...)
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
    # setThreshold
    thresh = setThreshold(r$pred, threshold = c("y1" = 0.9, "y2" = 0.9))
    expect_true(is.data.frame(thresh$data))
    # now test that we can tune the thresholds
    tr = tuneThreshold(r$pred, nsub = 2L, control = list(maxit = 2L))
    expect_true(!is.na(tr$perf))
    expect_equal(length(tr$th), length(getTaskClassLevels(multilabel.task)))
    # Learner with Impute-Preprocessing
    lrn1 = makeLearner("classif.rpart")
    lrn2 = fun(lrn1, ...)
    lrn2 = makeImputeWrapper(lrn2, classes = list(integer = imputeMedian(), numeric = imputeMedian(), factor = imputeConstant("Const")))
    multilabel.df2 = multilabel.df
    multilabel.df2[c(2, 10, 14), c(1, 5)] = NA
    multilabel.task2 = makeMultilabelTask("multilabel", data = multilabel.df2, target = multilabel.target)
    mod = train(lrn2, multilabel.task2)
    pred = predict(mod, multilabel.task2)
    p = performance(pred)
    expect_true(!is.na(p))
    # Learner with Hyperparameters
    lrn1 = makeLearner("classif.rpart", par.vals = list(minsplit = 10, cp = 0.005))
    lrn2 = fun(lrn1)
    mod = train(lrn2, multilabel.task)
    pred = predict(mod, multilabel.task)
    p = performance(pred)
    expect_true(!is.na(p))
    # 3 targets
    three.target.df = getTaskData(multilabel.task)
    three.target.df$y3 = three.target.df$y2
    multilabel3t.task = makeMultilabelTask(data = three.target.df, target = c("y1", "y2", "y3"))
    mod = train(lrn2, multilabel3t.task)
    pred = predict(mod, multilabel3t.task)
    p = performance(pred)
    expect_true(!is.na(p))
    pmulti = getMultilabelBinaryPerformances(pred, list(mmce))
    expect_true(!any(is.na(pmulti)))
    # check order
    args = list(...)
    if (!is.null(args$order)) {
      lrn2 = fun(lrn1, ...)
      expect_error(train(lrn2, multilabel3t.task), "Must be equal to set")
    }
  })
}

testMultilabelWrapper(makeMultilabelBinaryRelevanceWrapper)
testMultilabelWrapper(makeMultilabelDBRWrapper)
testMultilabelWrapper(makeMultilabelClassifierChainsWrapper)
testMultilabelWrapper(makeMultilabelNestedStackingWrapper)
testMultilabelWrapper(makeMultilabelStackingWrapper)

# check order
testMultilabelWrapper(makeMultilabelClassifierChainsWrapper, order = c("y2", "y1"))
testMultilabelWrapper(makeMultilabelNestedStackingWrapper, order = c("y2", "y1"))
