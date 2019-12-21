context("predict")

test_that("predict", {
  inds = multiclass.train.inds
  data = multiclass.df
  formula = multiclass.formula

  wl.lda = makeLearner("classif.lda", predict.type = "prob")

  cm2 = train(makeLearner("classif.lda"), multiclass.task, subset = inds)
  cp2 = predict(cm2, newdata = data[inds, ])
  cp2b = predict(cm2, newdata = data[inds, -5])
  requirePackagesOrSkip("MASS", default.method = "load")
  ext2 = MASS::lda(formula, data = data[inds, ])
  pred2 = predict(ext2, newdata = data[inds, ])$class

  expect_equal(cp2$data$response, pred2)
  expect_equal(cp2b$data$response, pred2)

  cm3 = train(wl.lda, multiclass.task, subset = inds)
  cp3 = predict(cm3, newdata = data[multiclass.test.inds, ])
  ext3 = MASS::lda(formula, data = data[inds, ])
  pred3 = predict(ext3, newdata = data[multiclass.test.inds, ])$class
  prob3 = predict(ext3, newdata = data[multiclass.test.inds, ])$post
  expect_equal(cp3$data$response, pred3)
  expect_equal(prob3, as.matrix(getPredictionProbabilities(cp3, colnames(prob3))))
  expect_true(is.numeric(getPredictionProbabilities(cp3, "setosa")))
  expect_equal(colnames(getPredictionProbabilities(cp3, c("setosa", "versicolor"))), c("setosa", "versicolor"))
  expect_equal(colnames(getPredictionProbabilities(cp3, c("versicolor", "setosa"))), c("versicolor", "setosa"))

  cp4 = predict(cm3, task = multiclass.task, subset = multiclass.test.inds)
  expect_equal(cp4$data$response, pred3)
  expect_equal(cp4$data$truth, data[multiclass.test.inds, multiclass.target])
  expect_equal(cp4$data$id, multiclass.test.inds)

  df3 = as.data.frame(cp3)
  df4 = as.data.frame(cp4)
  expect_equal(df3, df4[, -1])

  cm5 = train(wl.lda, binaryclass.task, subset = binaryclass.train.inds)
  cp5a = predict(cm5, task = binaryclass.task, subset = binaryclass.test.inds)
  cp5b = predict(cm5, task = binaryclass.task, subset = binaryclass.test.inds)
  cp5c = setThreshold(cp5b, 0)
  cp5d = setThreshold(cp5b, 1)
  cp5e = predict(cm5, task = binaryclass.task, subset = 1)
  expect_equal(cp5a$data$response, cp5b$data$response)
  f1 = factor(rep(getTaskDesc(binaryclass.task)$positive, length(binaryclass.test.inds)),
    levels = getTaskClassLevels(binaryclass.task))
  expect_equal(cp5c$data$response, f1)
  f2 = factor(rep(getTaskDesc(binaryclass.task)$negative, length(binaryclass.test.inds)),
    levels = getTaskDesc(binaryclass.task)$class.levels)
  expect_equal(cp5d$data$response, f2)
  expect_true(setequal(levels(cp5e$data$response), c("M", "R")))
})


test_that("predict works with type = se", {
  lrn = makeLearner("regr.lm", predict.type = "se")
  mod = train(lrn, regr.task)
  p = predict(mod, regr.task)
  expect_equal(colnames(p$data), c("id", "truth", "response", "se"))
})


test_that("predict works with strange class labels", {
  df = binaryclass.df
  levels(df[, binaryclass.target]) = c(-1, 1)
  task = makeClassifTask(data = df, target = binaryclass.target)
  mod = train(makeLearner("classif.lda", predict.type = "prob"), task = task)
  p = predict(mod, task = task)
  expect_equal(colnames(p$data), c("id", "truth", "prob.-1", "prob.1", "response"))
})

test_that("predict correctly propagates exception in predictLearner", {
  capture.output(expect_error(holdout("classif.__mlrmocklearners__1", multiclass.task), "foo"))
})

test_that("predict works with newdata / subset", {
  mod = train(makeLearner("classif.lda"), multiclass.task)
  p1 = predict(mod, newdata = multiclass.df, subset = 1:10)
  expect_equal(nrow(p1$data), 10)

  p2 = predict(mod, newdata = multiclass.df, subset = c(rep(TRUE, 10)))
  expect_equal(getPredictionResponse(p1), getPredictionResponse(p2))
})

test_that("predict preserves rownames", {
  data = multiclass.df
  rownames(data) = rev(seq_len(nrow(data)))
  task = makeClassifTask(data = data, target = multiclass.target)
  # kknn (or its mlr intergration) seems to NOT preserve rownames, issue 142 was reported
  mod = train("classif.kknn", task = task)
  nd = data[1:2, ]
  p = predict(mod, task = task, subset = 1:2)
  expect_equal(rownames(as.data.frame(p)), as.character(c(nrow(data), nrow(data) - 1L)))

  p = predict(mod, newdata = nd, subset = 2)
  expect_equal(rownames(as.data.frame(p)), as.character(nrow(data) - 1L))
})

test_that("setThreshold does not produce NAs for extreme thresholds", {
  # we had bug / issue 168 here
  data(GermanCredit, package = "caret")
  credit.task = makeClassifTask(data = GermanCredit, target = "Class")
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  mod = train(lrn, credit.task)
  p1 = predict(mod, task = credit.task)
  p2 = setThreshold(p1, 0)
  expect_true(!any(is.na(p2$data$response)))
})

test_that("predict.threshold", {
  td = getTaskDesc(binaryclass.task)
  lrn = makeLearner("classif.lda", predict.type = "prob", predict.threshold = 0)
  r = holdout(lrn, binaryclass.task)
  expect_true(all(r$pred$data$response == td$positive))

  lrn = makeLearner("classif.lda", predict.type = "prob", predict.threshold = 1)
  r = holdout(lrn, binaryclass.task)
  expect_true(all(r$pred$data$response == td$negative))

  lrn = makeLearner("classif.lda", predict.type = "prob",
    predict.threshold = c(setosa = 1000000000, virginica = 0, versicolor = 100000))
  r = holdout(lrn, multiclass.task)
  expect_true(all(r$pred$data$response == "virginica"))

  # now with wrapper
  lrn1 = makeLearner("classif.lda")
  lrn2 = makeFilterWrapper(lrn1, fw.method = "anova.test", fw.perc = 0.1)
  lrn2 = setPredictType(lrn2, "prob")
  lrn2 = setPredictThreshold(lrn2, 0)
  r = holdout(lrn2, binaryclass.task)
  expect_true(all(r$pred$data$response == td$positive))
})

test_that("predict doesn't warn if 'on.learner.error' is 'quiet'", {
  lrn = makeLearner("classif.qda", predict.type = "prob",
    config = list(on.learner.error = "quiet"))
  mod = train(lrn, iris.task, subset = c(1L, 51L, 101L))
  expect_true(inherits(mod, "FailureModel"))
  expect_warning(predict(mod, multiclass.task), NA)
})

test_that("predict works with data.table as newdata", {
  lrn = makeLearner("classif.qda")
  mod = train(lrn, iris.task)
  expect_warning(predict(mod, newdata = data.table(iris)),
    regexp = "Provided data for prediction is not a pure data.frame but from class data.table, hence it will be converted.")
})
