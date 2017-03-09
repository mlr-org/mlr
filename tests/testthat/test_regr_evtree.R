context("regr_evtree")

test_that("regr_evtree", {
  requirePackagesOrSkip("evtree", default.method = "load")

  parset.list = list(
    list(),
    list(maxdepth = 2),
    list(ntrees = 200)
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(regr.formula, data=regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(evtree::evtree, pars)
    set.seed(getOption("mlr.debug.seed"))
    old.predicts.list[[i]] = as.vector(predict(m, newdata = regr.test))
  }

  testSimpleParsets("regr.evtree", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)

})
