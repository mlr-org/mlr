context("oneclass")

test_that("oneclass task", {
  oc = oneclass.task
  expect_equal(getTaskTargetNames(oc), "normal")
  expect_equal(getTaskClassLevels(oc), c("FALSE", "TRUE"))
  y = getTaskTargets(oc)
  expect_true(is.factor(y))
  expect_true(length(levels(y)) == 2)
  expect_set_equal(levels(y), c("FALSE", "TRUE"))
})

test_that("oneclass learning", {
  lrn = makeLearner("oneclass.svm")

  # train predict eval
  mod = train(lrn, oneclass.task)
  pred = predict(mod, oneclass.task)
  p = performance(pred)
  expect_true(!is.na(p))

  # with newdata df
  pred = predict(mod, newdata = oneclass.df)
  p = performance(pred)
  expect_true(!is.na(p))

  # resample
  r = holdout(lrn, oneclass.task)
  expect_true(!is.na(r$aggr))

   # Learner with Impute-Preprocessing
  lrn = makeImputeWrapper(lrn, classes = list(integer = imputeMedian(), numeric = imputeMedian(), factor = imputeConstant("Const")))
  oneclass.df2 = oneclass.df
  oneclass.df2[c(2, 10, 14), c(1, 2)] = NA
  oneclass.task2 = makeOneClassTask("oneclass", data = oneclass.df2, target = oneclass.target, positive = oneclass.positive, negative = oneclass.negative)
  mod = train(lrn, oneclass.task2)
  pred = predict(mod, oneclass.task2)
  p = performance(pred)
  expect_true(!is.na(p))
  # Learner with Hyperparameters
  lrn = makeLearner("oneclass.svm", par.vals = list(kernel = "radial", gamma = 0.3))
  mod = train(lrn, oneclass.task)
  pred = predict(mod, oneclass.task)
  p = performance(pred)
  expect_true(!is.na(p))
})
