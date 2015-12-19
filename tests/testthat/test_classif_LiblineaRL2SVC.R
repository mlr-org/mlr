context("classif_LiblineaRL2SVC")

test_that("classif_LiblineaRL2SVC", {
  requirePackages("LiblineaR", default.method = "load")

  parset.list = list(
    list(type = 1),
    list(type = 2),
    list(type = 1, cost = 5L),
    list(type = 2, cost = 5L)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1L:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(data = binaryclass.train[, -binaryclass.class.col],
      target = binaryclass.train[, binaryclass.target])
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(LiblineaR::LiblineaR, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newx = binaryclass.test[, -binaryclass.class.col])
    old.predicts.list[[i]] = as.factor(p$predictions)
  }
  parset.list = list(
    list(type = 1),
    list(),
    list(type = 1, cost = 5L),
    list(cost = 5L)
  )

  testSimpleParsets("classif.LiblineaRL2SVC", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)

})
