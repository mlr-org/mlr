context("regr_nnet")

test_that("regr_nnet", {
  requirePackagesOrSkip("nnet", default.method = "load")

  # test with empty paramset
  set.seed(getOption("mlr.debug.seed"))
  capture.output({
    set.seed(getOption("mlr.debug.seed"))
    m = nnet::nnet(regr.formula, size = 3L, data = regr.train, linout = TRUE)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = regr.test)[, 1L]
    print(p)
  })
  testSimple("regr.nnet", regr.df, regr.target, regr.train.inds, p,
    parset = list())

  # test with params passed
  set.seed(getOption("mlr.debug.seed"))
  capture.output({
    set.seed(getOption("mlr.debug.seed"))
    m = nnet::nnet(regr.formula, size = 7L, data = regr.train, linout = TRUE)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = regr.test)[, 1L]
    print(p)
  })
  testSimple("regr.nnet", regr.df, regr.target, regr.train.inds, p,
    parset = list(size = 7L))

  # tt = function (formula, data, subset = 1:150, ...) {
    # nnet::nnet(formula, data = data[subset,], size = 3L, maxit = 50L)
  # }
  # tp = function(model, newdata) as.factor(predict(model, newdata, type = "class"))

  # testCV("regr.nnet", regr.df, regr.target, tune.train = tt, tune.predict = tp,
    # parset = list(size = 3L, maxit = 50L))
})
