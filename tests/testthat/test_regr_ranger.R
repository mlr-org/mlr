context("regr_ranger")

test_that("regr_ranger", {
  requirePackagesOrSkip("ranger", default.method = "load")

  parset.list = list(
    list(num.trees = 10),
    list(num.trees = 10, mtry = 4),
    list(num.trees = 10, min.node.size = 2)
  )
  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = regr.train, formula = regr.formula,
      respect.unordered.factors = "order"))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(ranger::ranger, parset)
    p = predict(m, data = regr.test)
    old.predicts.list[[i]] = p$predictions
  }

  testSimpleParsets("regr.ranger", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)
})

test_that("se with se.method = sd", {
  requirePackagesOrSkip("ranger", default.method = "load")

  parset.list = list(
    list(num.trees = 10),
    list(num.trees = 10, se.method = "jack"),
    list(num.trees = 10, mtry = 4),
    list(num.trees = 10, min.node.size = 2)
  )

  old.predicts.list = list()

  parset.only.for.predict = "se.method"

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset.list[[i]] = c(parset, predict.type = "se")
    parset = c(parset, list(data = regr.train, formula = regr.formula,
      keep.inbag = TRUE))
    parset = dropNamed(parset, parset.only.for.predict)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(ranger::ranger, parset)
    set.seed(getOption("mlr.debug.seed"))
    predict.parset = parset[parset.only.for.predict]
    predict.parset = c(list(object = m, data = regr.test, type = "se"),
      predict.parset)
    p = do.call(predict, predict.parset)
    old.predicts.list[[i]] = cbind(p$predictions, p$se)
  }

  testSimpleParsets("regr.ranger", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)
})
