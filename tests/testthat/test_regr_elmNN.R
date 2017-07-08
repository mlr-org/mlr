context("regr_elmNN")

test_that("regr_elmNN", {
  requirePackagesOrSkip("elmNN", default.method = "load")

  parset.list = list(
    list(),
    list(nhid = 2L),
    list(nhid = 2L, actfun = "radbas")
  )

  old.predicts.list = list()

  num = sapply(regr.train, is.numeric)
  xind = num & names(regr.train) != regr.target

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(y = regr.train[, regr.target], x = regr.train[, xind])
    pars = c(pars, parset)
    if (!"nhid" %in% names(pars)) pars$nhid = 1L
    if (!"actfun" %in% names(pars)) pars$actfun = "sig"
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(elmNN::elmtrain.default, pars)
    set.seed(getOption("mlr.debug.seed"))
    old.predicts.list[[i]] = predict(m, newdata = regr.test[, xind])[, 1]
  }
  # FIXME:
  # Does not yet work because we can not yet set the seed for elmNN
  # testSimpleParsets("regr.elmNN", regr.df[, num], regr.target, regr.train.inds, old.predicts.list, parset.list)
  for (i in seq_along(parset.list)){
    expect_true(length(old.predicts.list[[i]]) == nrow(regr.test))
  }
})
