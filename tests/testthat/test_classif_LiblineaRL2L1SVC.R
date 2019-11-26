context("classif_LiblineaRL2L1SVC")

test_that("classif_LiblineaRL2L1SVC", {
  requirePackagesOrSkip("LiblineaR", default.method = "load")

  parset.list = list(
    list(),
    list(cost = 5L, epsilon = 0.1),
    list(cost = 5L, epsilon = 0.5),
    list(cost = 2L, epsilon = 0.1),
    list(cost = 2L, epsilon = 0.5)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(data = binaryclass.train[, -binaryclass.class.col],
      target = binaryclass.train[, binaryclass.target], type = 3L)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(LiblineaR::LiblineaR, pars)
    p = predict(m, newx = binaryclass.test[, -binaryclass.class.col])
    old.predicts.list[[i]] = as.factor(p$predictions)
  }

  testSimpleParsets("classif.LiblineaRL2L1SVC", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)
})
