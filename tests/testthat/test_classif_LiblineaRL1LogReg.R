context("classif_LiblineaRL1LogReg")

test_that("classif_LiblineaRL1LogReg", {
  requirePackagesOrSkip("LiblineaR", default.method = "load")

  parset.list = list(
    list(),
    list(cost = 2L),
    list(cost = 5L)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(data = binaryclass.train[, -binaryclass.class.col],
      target = binaryclass.train[, binaryclass.target], type = 6L)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(LiblineaR::LiblineaR, pars)
    p = predict(m, newx = binaryclass.test[, -binaryclass.class.col], proba = TRUE)
    old.predicts.list[[i]] = as.factor(p$predictions)
    old.probs.list[[i]] = p$probabilities[, 2L]
  }

  testSimpleParsets("classif.LiblineaRL1LogReg", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.LiblineaRL1LogReg", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)
})
