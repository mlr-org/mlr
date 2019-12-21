context("classif_xgboost")

test_that("classif_xgboost", {
  requirePackagesOrSkip("xgboost", default.method = "load")

  parset.list = list(
    list(),
    list(nrounds = 20L)
  )

  parset.probs.list = list(
    list(),
    # We had a bug here that 'multi:softprob' didn't work with binaryclass
    list(objective = "multi:softprob")
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(data = data.matrix(binaryclass.train[, 1:60]),
      label = as.numeric(binaryclass.train[, 61]) - 1)
    if (is.null(parset$objective)) parset$objective = "binary:logistic"
    if (is.null(parset$verbose)) parset$verbose = 0L
    if (is.null(parset$nround)) parset$nrounds = 1L
    pars = c(pars, parset)
    model = do.call(xgboost::xgboost, pars)
    pred = predict(model, data.matrix(binaryclass.test[, 1:60]))
    old.predicts.list[[i]] = factor(as.numeric(pred > 0.5),
      labels = binaryclass.class.levs)
  }

  for (i in seq_along(parset.probs.list)) {
    parset = parset.probs.list[[i]]
    pars = list(data = data.matrix(binaryclass.train[, 1:60]),
      label = as.numeric(binaryclass.train[, 61]) - 1)
    if (is.null(parset$objective)) parset$objective = "binary:logistic"
    if (is.null(parset$verbose)) parset$verbose = 0L
    if (is.null(parset$nround)) parset$nrounds = 1L
    if (parset$objective == "multi:softprob") {
      parset$num_class = length(binaryclass.class.levs)
    }
    pars = c(pars, parset)
    model = do.call(xgboost::xgboost, pars)
    pred = predict(model, data.matrix(binaryclass.test[, 1:60]))
    if (parset$objective == "multi:softprob") {
      y = matrix(pred, nrow = length(pred) / length(binaryclass.class.levs),
        ncol = length(binaryclass.class.levs), byrow = TRUE)
      old.probs.list[[i]] = y[, 1]
    } else {
      old.probs.list[[i]] = 1 - pred
    }
  }

  testSimpleParsets("classif.xgboost", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)

  testProbParsets("classif.xgboost", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.probs.list)
})

test_that("xgboost works with different 'missing' arg vals", {
  expect_silent(makeLearner("classif.xgboost", missing = NA_real_))
  expect_silent(makeLearner("classif.xgboost", missing = NA))
  expect_silent(makeLearner("classif.xgboost", missing = NULL))
})

test_that("xgboost objective 'multi:softmax' does not work with predict.type = 'prob'", {
  expect_error(train(makeLearner("classif.xgboost", predict.type = "prob",
    objective = "multi:softmax"), binaryclass.task))
})

test_that("multiclass xgboost with 'multi:softmax' does not produce NA predictions", {
  mod = train(makeLearner("classif.xgboost", objective = "multi:softmax"),
    task = multiclass.task)
  pred = predict(mod, multiclass.task)
  expect_false(any(is.na(pred$data$response)))
})

# from https://github.com/mlr-org/mlr3learners/issues/32
test_that("xgboost with multi:softprob", {
  learner = makeLearner("classif.xgboost", nrounds = 5L,
    objective = "multi:softprob")
  mod = train(learner, sonar.task)
  pred = predict(mod, sonar.task)
  expect_equal(unname(performance(pred,
    measures = getDefaultMeasure(sonar.task))), 0)
})

# from https://github.com/mlr-org/mlr3learners/issues/32
test_that("xgboost with binary:logistic", {
  learner = makeLearner("classif.xgboost", nrounds = 5L)
  mod = train(learner, sonar.task)
  pred = predict(mod, sonar.task)
  expect_equal(unname(performance(pred,
    measures = getDefaultMeasure(sonar.task))), 0)
})
