context("classif_LiblineaRL2SVC")

test_that("classif_LiblineaRL2SVC", {
  requirePackagesOrSkip("LiblineaR", default.method = "load")

  parset.list1 = list(
    list(type = 2L),
    list(type = 1L),
    list(type = 1L, cost = 5L),
    list(type = 2L, cost = 5L)
  )

  parset.list2 = list(
    list(),
    list(type = 1L),
    list(type = 1L, cost = 5L),
    list(type = 2L, cost = 5L)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(data = binaryclass.train[, -binaryclass.class.col],
      target = binaryclass.train[, binaryclass.target])
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(LiblineaR::LiblineaR, pars)
    p = predict(m, newx = binaryclass.test[, -binaryclass.class.col])
    old.predicts.list[[i]] = as.factor(p$predictions)
  }

  testSimpleParsets("classif.LiblineaRL2SVC", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list2)
})
