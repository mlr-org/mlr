context("forecast")

test_that("forecast regression task", {
  fc = fcregr.task
  expect_equal(getTaskTargetNames(fc), c("test_data"))
  y = getTaskTargets(fc)
  expect_true(is.numeric(y) && is.null(ncol(y)) )
  expect_true(is.null(colnames(y)))
})

test_that("forecast learning", {
  lrn = makeLearner("fcregr.Arima")

  # train predict eval
  mod = train(lrn, fcregr.task)
  pred = predict(mod, newdata = fcregr.test)
  p = performance(pred)
  expect_true(!is.na(p))
  # with newdata xts
  pred = predict(mod, newdata = fcregr.test)
  p = performance(pred)
  expect_true(!is.na(p))
  # resample
  r = holdout(lrn, fcregr.task,split = .99)
  expect_true(!is.na(r$aggr))
  # Learning with Lambert W
  lrn = makePreprocWrapperLambert(lrn, type = "h", verbose = FALSE)
  fcregr.xts2 = fcregr.xts
  fcregr.task2 = makeForecastRegrTask("fcregr", data = fcregr.xts2, target = fcregr.target, check.data = FALSE)
  expect_error(mod = train(lrn, fcregr.task2))
#  pred = predict(mod, newdata = fcregr.test)
#  p = performance(pred)
#  expect_true(!is.na(p))
  # Learner with Hyperparameters
  lrn = makeLearner("fcregr.Arima", par.vals = list(order = c(2,0,1), include.mean = FALSE))
  mod = train(lrn, fcregr.task)
  pred = predict(mod, newdata = fcregr.test)
  p = performance(pred)
  expect_true(!is.na(p))
})

# Need to test model multiplexer
