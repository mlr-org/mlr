context("regr_blackboost")

test_that("regr_blackboost", {
  requirePackages(c("mboost","party"), default.method = "load")
  parset.list1 = list(
    list(family=mboost::GaussReg(), tree_controls=party::ctree_control(maxdepth=2)),
    list(family=mboost::GaussReg(), tree_controls=party::ctree_control(maxdepth=4), control=mboost::boost_control(nu=0.03))
  )

  parset.list2 = list(
    list(family=mboost::Gaussian(), maxdepth=2),
    list(family=mboost::Gaussian(), maxdepth=4, nu=0.03)
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(regr.formula, data=regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(mboost::blackboost, pars)
    set.seed(getOption("mlr.debug.seed"))
    old.predicts.list[[i]] = predict(m, newdata=regr.test)[,1]
  }

  testSimpleParsets("regr.blackboost", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list2)
})
