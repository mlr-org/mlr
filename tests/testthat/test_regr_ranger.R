context("regr_ranger")

test_that("regr_ranger", {
  requirePackagesOrSkip("ranger", default.method = "load")

  parset.list = list(
    list(),
    list(num.trees = 100),
    list(num.trees = 250, mtry = 4),
    list(num.trees = 500, min.node.size = 2)
  )
  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = regr.train, formula = regr.formula, respect.unordered.factors = "order"))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(ranger::ranger, parset)
    p  = predict(m, data = regr.test)
    old.predicts.list[[i]] = p$predictions
  }

  testSimpleParsets("regr.ranger", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
})

test_that("regr_ranger se", {
  requirePackagesOrSkip("ranger", default.method = "load")

  parset.list = list(
    list(),
    list(num.trees = 100),
    list(num.trees = 250, mtry = 4),
    list(num.trees = 500, min.node.size = 2)
  )
  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset.list[[i]] = c(parset, predict.type = "se")
    parset = c(parset, list(data = regr.train, formula = regr.formula, respect.unordered.factors = "order", keep.inbag = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(ranger::ranger, parset)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, data = regr.test, type = "se")
    old.predicts.list[[i]] = cbind(p$predictions, p$se)
  }

  testSimpleParsets("regr.ranger", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
})
