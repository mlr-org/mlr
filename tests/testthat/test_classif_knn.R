context("classif_knn")

test_that("classif_knn", {
  requirePackagesOrSkip("class", default.method = "load")

  parset.list = list(
    list(),
    list(k = 10)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    train = multiclass.train
    y = train[, multiclass.target]
    train[, multiclass.target] = NULL
    test = multiclass.test
    test[, multiclass.target] = NULL
    pars = list(train = train, cl = y, test = test)
    pars = c(pars, parset)
    p = do.call(class::knn, pars)
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("classif.knn", multiclass.df, multiclass.target, multiclass.train.inds,
    old.predicts.list, parset.list)

  tt = function(formula, data, k = 1) {
    return(list(formula = formula, data = data, k = k))
  }
  tp = function(model, newdata) {
    target = as.character(model$formula)[2]
    train = model$data
    y = train[, target]
    train[, target] = NULL
    newdata[, target] = NULL
    class::knn(train = train, cl = y, test = newdata, k = model$k)
  }

  testCVParsets("classif.knn", multiclass.df, multiclass.target, tune.train = tt,
    tune.predict = tp, parset.list = parset.list)
})
