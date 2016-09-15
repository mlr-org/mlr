context("regr_blackboost")

test_that("regr_blackboost", {
  requirePackagesOrSkip(c("mboost","party"), default.method = "load")

  parset.list1 = list(
    list(tree_controls=party::ctree_control(maxdepth=2)),
    list(family=mboost::Gaussian(), tree_controls=party::ctree_control(maxdepth=2)),
    list(family=mboost::Huber(d = 0.3), tree_controls=party::ctree_control(maxdepth=4), control=mboost::boost_control(nu=0.03))
  )

  parset.list2 = list(
    list(maxdepth = 2),
    list(family = "Gaussian", maxdepth=2),
    list(family = "Huber", d = 0.3, maxdepth=4, nu=0.03)
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


test_that("regr_blackboost works with families for count data", {
  # change target to count data
  new.regr.df = regr.df
  new.regr.df[, regr.target] = as.integer(floor(new.regr.df[,regr.target]))
  new.regr.train = new.regr.df[regr.train.inds,]
  new.regr.test = new.regr.df[regr.test.inds,]
  
  parset.list1 = list(
    list(family = mboost::Poisson(), control = mboost::boost_control(nu = 0.02), tree_controls=party::ctree_control(maxdepth=2)),
    list(family = mboost::NBinomial(), tree_controls=party::ctree_control(maxdepth=2)),
    list(family = mboost::Hurdle(),tree_controls=party::ctree_control(maxdepth=2))
  )
  
  parset.list2 = list(
    list(family = "Poisson", nu = 0.02, maxdepth = 2),
    list(family= "NBinomial", maxdepth = 2),
    list(family = "Hurdle", maxdepth = 2)
  )
  old.predicts.list = list()
  for (i in 1:length(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(regr.formula, data = new.regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(mboost::blackboost, pars)
    set.seed(getOption("mlr.debug.seed"))
    old.predicts.list[[i]] = predict(m, newdata = new.regr.test, type = "response")[,1]
  }
  testSimpleParsets("regr.blackboost", new.regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list2)
})

