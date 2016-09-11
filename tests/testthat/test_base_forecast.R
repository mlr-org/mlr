context("forecast")

test_that("forecast regression task", {
  fc = fcregr.task
  expect_equal(getTaskTargetNames(fc), "test_data")
  y = getTaskTargets(fc)
  expect_true(is.numeric(y) && is.null(ncol(y)) )
  expect_true(is.null(colnames(y)))
})

test_that("forecast tasks and learner", {
  lrn = makeLearner("fcregr.Arima")

  # train predict eval
  mod = train(lrn, fcregr.task)
  pred = predict(mod, newdata = fcregr.test)
  p = performance(pred)
  expect_true(!is.na(p))
  # with newdata df
  pred = predict(mod, newdata = fcregr.test)
  p = performance(pred)
  expect_true(!is.na(p))
  # resample
  r = holdout(lrn, fcregr.task, split = .99)
  expect_true(!is.na(r$aggr))
  # Learning with Lambert W
  lrn = makePreprocWrapperLambert(lrn, type = "h", verbose = FALSE)
  fcregr.df2 = fcregr.df
  fcregr.task2 = makeForecastRegrTask("fcregr", data = fcregr.df2, target = fcregr.target, check.data = FALSE)
  expect_error(mod = train(lrn, fcregr.task2))
  # Learner with Hyperparameters
  lrn = makeLearner("fcregr.Arima", par.vals = list(order = c(2, 0, 1), include.mean = FALSE))
  mod = train(lrn, fcregr.task)
  pred = predict(mod, newdata = fcregr.test)
  p = performance(pred)
  expect_true(!is.na(p))
})

test_that("forecast with regular tasks", {
  inds = multiclass.train.inds.lag
  data = multiclass.df.lag
  formula = multiclass.formula

  # multiclass response works
  cm2 = train(makeLearner("classif.lda"), multiclass.task.lag, subset = inds)
  cp2  = forecast(cm2, h = multiclass.h, newdata = data[-inds, , drop = FALSE])
  cp2b = forecast(cm2, h = multiclass.h, newdata = data[-inds, -1, drop = FALSE])
  cp2c = forecast(cm2, h = multiclass.h)
  # multiclass probs
  wl.lda = makeLearner("classif.lda", predict.type = "prob")
  cm3 = train(wl.lda, multiclass.task.lag, subset = inds)
  cp3 = forecast(cm3, h = multiclass.h, newdata = data[multiclass.test.inds.lag, , drop = FALSE])
  test.pred = getPredictionProbabilities(cp3)
  expect_true(is.numeric(getPredictionProbabilities(cp3, "setosa")))
  expect_equal(colnames(getPredictionProbabilities(cp3, c("setosa", "versicolor"))), c("setosa", "versicolor"))
  expect_equal(colnames(getPredictionProbabilities(cp3, c("versicolor", "setosa"))), c("versicolor", "setosa"))

  cm5 = train(wl.lda, binaryclass.task.lag, subset = binaryclass.train.inds.lag)
  cp5a = forecast(cm5, h = binaryclass.h)
  cp5b = forecast(cm5, h = binaryclass.h, newdata = binaryclass.test.lag)
  cp5c = setThreshold(cp5b, 0)
  cp5d = setThreshold(cp5b, 1)
  expect_equal(cp5a$data$response, cp5b$data$response)
  f1 = factor(rep(getTaskDesc(binaryclass.task.lag)$positive, length(binaryclass.test.inds.lag)),
    levels = getTaskClassLevels(binaryclass.task.lag))
  expect_equal(cp5c$data$response, f1)
  f2 = factor(rep(getTaskDesc(binaryclass.task.lag)$negative, length(binaryclass.test.inds.lag)),
    levels = getTaskDesc(binaryclass.task.lag)$class.levels)
  expect_equal(cp5d$data$response, f2)
})


test_that("forecast works with type = se", {
  lrn = makeLearner("regr.lm", predict.type = "se")
  mod = train(lrn, regr.task.lag)
  p = forecast(mod, h = regr.h, newdata = regr.test.lag)
  expect_equal(colnames(p$data), c("truth", "response", "se"))
})


#test_that("forecast works with strange class labels", {
# df = binaryclass.df.lag
#  levels(df[,binaryclass.target]) = c(-1,1)
#  task = makeClassifTask(data = df, target = binaryclass.target)
#  task.lag = createLagDiffFeatures(task, lag = 1L:5L, na.pad = FALSE)
#  mod = train(makeLearner("classif.lda", predict.type = "prob"), task = task.lag)
#  p = forecast(mod, h = binaryclass.h, newdata = df[binaryclass.test.inds,,drop=FALSE])
#  expect_equal(colnames(p$data), c("truth", "prob.truth", "prob.1", "prob.-1", "prob.response", "response"))
#})

test_that("forecast correctly propagates exception in predictLearner", {
  capture.output(expect_error(holdout("classif.__mlrmocklearners__1", multiclass.task.lag), "foo"))
})

test_that("setThreshold does not produce NAs for extreme thresholds", {
  # we had bug / issue 168 here
  data(GermanCredit, package = "caret")
  german.credit.dates = as.POSIXct("1992-01-14") + seq_len(nrow(GermanCredit))
  credit.task.lag = makeClassifTask(data = GermanCredit[, "Class", drop = FALSE], target = "Class")
  credit.task.lag = createLagDiffFeatures(credit.task.lag, lag = 1:5L, date.col = german.credit.dates)
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  mod = train(lrn, credit.task.lag)
  p1 = forecast(mod, h = 10L)
  p2 = setThreshold(p1, 0)
  expect_true(!any(is.na(p2$data$response)))
})


test_that("predict works with data.table as newdata", {
  lrn = makeLearner("classif.lda")
  mod = train(lrn, multiclass.task.lag)
  expect_warning(forecast(mod, newdata = data.table(iris[1:5, ]), h = 5), regexp = "Provided data for prediction is not a pure data.frame but from class data.table, hence it will be converted.")
})

