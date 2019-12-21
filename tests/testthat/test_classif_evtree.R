context("classif_evtree")

test_that("classif_evtree", {
  requirePackagesOrSkip("evtree", default.method = "load")

  parset.list = list(
    list(),
    list(maxdepth = 2),
    list(ntrees = 200)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = binaryclass.formula, data = binaryclass.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(evtree::evtree, pars)
    old.predicts.list[[i]] = predict(m, newdata = binaryclass.test)
    p = predict(m, newdata = binaryclass.test, type = "prob")
    old.probs.list[[i]] = p[, 1]
  }

  testSimpleParsets("classif.evtree", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.evtree", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)
})
