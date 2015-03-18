context("regr_xgboost")

test_that("regr_xgboost", {
  requirePackages("xgboost", default.method = "load")
  parset.list = list(
    list(nrounds = 1),
    list(nrounds = 5, max.depth = 4L)
    # list(nrounds = 3, booster = "gblinear", lambda = 0.4, alpha = 0.2)
    # FIXME: why is this not working?
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    d = xgboost::xgb.DMatrix(
      data = as.matrix(regr.num.df[regr.num.train.inds, - regr.num.class.col]),
      label = regr.num.df[regr.num.train.inds, regr.num.target]
    )
    pars = list(data = d, objective = "reg:linear")
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output(
      m <- do.call(xgboost::xgb.train, pars)
    )
    set.seed(getOption("mlr.debug.seed"))
    p = xgboost::predict(m, newdata = as.matrix(regr.num.df[regr.num.test.inds, - regr.num.class.col]))
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.xgboost", regr.num.df, regr.num.target, regr.num.train.inds, old.predicts.list, parset.list)
})
