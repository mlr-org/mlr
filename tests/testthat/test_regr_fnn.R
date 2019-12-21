context("regr_fnn")

test_that("regr_fnn", {
  requirePackagesOrSkip("FNN", default.method = "load")

  parset.list = list(
    list(),
    list(k = 1),
    list(k = 4),
    list(k = 10)
  )

  rdf = regr.df[, -4]
  rtrain = regr.train[, -4]
  rtest = regr.test[, -4]
  rtask = makeRegrTask("regrtask", data = rdf, target = "medv")

  old.predicts.list1 = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    j = which(colnames(rtrain) == regr.target)
    pars = list(train = rtrain[, -j], test = rtest[, -j], y = rtrain[, j])
    pars = c(pars, parset)
    old.predicts.list1[[i]] = do.call(FNN::knn.reg, pars)$pred
  }

  testSimpleParsets("regr.fnn", rdf, regr.target, regr.train.inds,
    old.predicts.list1, parset.list)

  tt = function(formula, data, k = 3) {
    j = which(colnames(data) == as.character(formula)[2])
    list(train = data[, -j], y = data[, j], k = k, target = j)
  }
  tp = function(model, newdata) {
    newdata = newdata[, -model$target]
    FNN::knn.reg(train = model$train, test = newdata, y = model$y,
      k = model$k)$pred
  }

  testCVParsets("regr.fnn", rdf, regr.target, tune.train = tt,
    tune.predict = tp, parset.list = parset.list)
})
