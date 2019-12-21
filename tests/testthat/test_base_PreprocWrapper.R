context("PreprocWrapper")

test_that("PreprocWrapper", {
  f1 = function(data, target, args) {
    data[, 2] = args$x * data[, 2]
    return(list(data = data, control = list()))
  }
  f2 = function(data, target, args, control) {
    data[, 2] = args$x * data[, 2]
    return(data)
  }
  ps = makeParamSet(
    makeNumericLearnerParam(id = "x"),
    makeNumericLearnerParam(id = "y")
  )
  lrn1 = makeLearner("classif.rpart", minsplit = 10)
  lrn2 = makePreprocWrapper(lrn1, train = f1, predict = f2, par.set = ps, par.vals = list(x = 1, y = 2))
  capture.output(print(lrn2))

  expect_true(setequal(getHyperPars(lrn2), list(xval = 0, minsplit = 10, x = 1, y = 2)))
  expect_true(setequal(getHyperPars(lrn2, "train"), list(xval = 0, minsplit = 10, x = 1, y = 2)))
  expect_true(setequal(lrn2$par.vals, list(x = 1, y = 2)))

  lrn3 = setHyperPars(lrn2, minsplit = 77, x = 88)
  expect_true(setequal(getHyperPars(lrn3), list(xval = 0, minsplit = 77, x = 88, y = 2)))
  expect_true(setequal(lrn3$par.vals, list(x = 88, y = 2)))

  m = train(lrn2, task = multiclass.task)
  capture.output(print(m))
  expect_true(setequal(getHyperPars(m$learner), list(xval = 0, minsplit = 10, x = 1, y = 2)))
})

test_that("getLearnerModel on nested PreprocWrapper", {
  lrn = makeLearner("classif.rpart")
  lrn = makeDummyFeaturesWrapper(lrn)
  lrn = makeImputeWrapper(lrn, classes = list(numeric = imputeMax(5), factor = imputeConstant("NA")))
  m = train(lrn, binaryclass.task)
  expect_is(getLearnerModel(m), "PreprocModel")
  expect_is(getLearnerModel(m, TRUE), "rpart")
})

test_that("PreprocWrapper with glmnet (#958)", {
  requirePackagesOrSkip("glmnet", default.method = "load")
  lrn = makeLearner("classif.glmnet", predict.type = "response")
  lrn2 = makePreprocWrapper(lrn,
    train = function(data, target, args) {
      return(list(data = data, control = list()))
    },
    predict = function(data, target, args, control) {
      return(data)
    })
  mod = train(lrn2, multiclass.task)
  pred = predict(mod, multiclass.task)
  expect_error(pred, NA)
})
