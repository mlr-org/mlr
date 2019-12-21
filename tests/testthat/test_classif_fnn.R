context("classif_fnn")

test_that("classif_fnn", {
  requirePackagesOrSkip("FNN", default.method = "load")

  parset.list = list(
    list(),
    list(k = 1),
    list(k = 4),
    list(k = 10)
  )

  old.predicts.list1 = list()
  old.predicts.list2 = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]

    j = which(colnames(multiclass.train) == multiclass.target)
    pars = list(train = multiclass.train[, -j], test = multiclass.test[, -j],
      cl = multiclass.train[, j])
    pars = c(pars, parset)
    old.predicts.list1[[i]] = do.call(FNN::knn, pars)

    j = which(colnames(binaryclass.train) == binaryclass.target)
    pars = list(train = binaryclass.train[, -j], test = binaryclass.test[, -j],
      cl = binaryclass.train[, j])
    pars = c(pars, parset)
    old.predicts.list2[[i]] = do.call(FNN::knn, pars)
  }

  testSimpleParsets("classif.fnn", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list1, parset.list)
  testSimpleParsets("classif.fnn", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list2, parset.list)

  tt = function(formula, data, k = 1) {
    j = which(colnames(data) == as.character(formula)[2])
    list(train = data[, -j], cl = data[, j], k = k, target = j)
  }
  tp = function(model, newdata) {
    newdata = newdata[, -model$target]
    FNN::knn(train = model$train, test = newdata, cl = model$cl, k = model$k)
  }

  testCVParsets("classif.fnn", multiclass.df, multiclass.target, tune.train = tt,
    tune.predict = tp, parset.list = parset.list)
  testCVParsets("classif.fnn", binaryclass.df, binaryclass.target, tune.train = tt,
    tune.predict = tp, parset.list = parset.list)
})
