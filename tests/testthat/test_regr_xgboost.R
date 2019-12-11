context("regr_xgboost")

test_that("regr_xgboost", {
  requirePackagesOrSkip("xgboost", default.method = "load")

  parset.list = list(
    list(),
    list(nrounds = 20)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    if (is.null(parset$verbose)) parset$verbose = 0L
    if (is.null(parset$nrounds)) parset$nrounds = 1L
    if (is.null(parset$objective)) parset$objective = "reg:linear"
    pars = list(data = data.matrix(regr.num.train[, -regr.num.class.col]),
      label = as.numeric(regr.num.train[, regr.num.class.col]))
    pars = c(pars, parset)
    model = do.call(xgboost::xgboost, pars)
    # model = xgboost::xgboost(data = data.matrix(regr.num.train[,-regr.num.class.col]), verbose = 0L,
    # label = as.numeric(regr.num.train[,regr.num.class.col]),
    # nrounds = 20, objective = "reg:linear", missing = NULL)
    old.predicts.list[[i]] = predict(model,
      data.matrix(regr.num.test[, -regr.num.class.col]))
  }

  # set.seed(getOption("mlr.debug.seed"))
  testSimpleParsets("regr.xgboost", regr.num.df, regr.num.target, regr.num.train.inds,
    old.predicts.list, parset.list)
})

test_that("xgboost works with different 'missing' arg vals", {
  expect_silent(makeLearner("regr.xgboost", missing = NA_real_))
  expect_silent(makeLearner("regr.xgboost", missing = NA))
  expect_silent(makeLearner("regr.xgboost", missing = NULL))
})
