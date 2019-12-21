context("classif_LiblineaRL2LogReg")

test_that("classif_LiblineaRL2LogReg", {
  requirePackagesOrSkip("LiblineaR", default.method = "load")

  parset.list = list(
    list(),
    list(type = 0),
    list(type = 7),
    list(type = 0, cost = 5L),
    list(type = 7, cost = 5L)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(data = binaryclass.train[, -binaryclass.class.col],
      target = binaryclass.train[, binaryclass.target])
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(LiblineaR::LiblineaR, pars)
    p = predict(m, newx = binaryclass.test[, -binaryclass.class.col], proba = TRUE)
    old.predicts.list[[i]] = as.factor(p$predictions)
    old.probs.list[[i]] = p$probabilities[, 2L]
  }

  testSimpleParsets("classif.LiblineaRL2LogReg", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.LiblineaRL2LogReg", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)
})
