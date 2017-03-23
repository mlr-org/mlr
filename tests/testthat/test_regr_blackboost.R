context("regr_blackboost")

test_that("regr_blackboost", {
  requirePackagesOrSkip(c("mboost", "party"), default.method = "load")

  parset.list1 = list(
    list(),
    # the blackboost defaults for tree_controls needs to be passed explicitely,
    # since the defaults of party::ctree_control() differ from defaults used within blackboost
    list(family = mboost::GammaReg(), tree_controls = party::ctree_control(teststat = "max",
      testtype = "Teststatistic", mincriterion = 0, maxdepth = 4, savesplitstats = FALSE)),
    list(family = mboost::Laplace(), control = mboost::boost_control(nu = 0.03))
  )

  parset.list2 = list(
    list(),
    list(family = "GammaReg", maxdepth = 4),
    list(family = "Laplace", nu = 0.03)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(mboost::blackboost, pars)
    set.seed(getOption("mlr.debug.seed"))
    old.predicts.list[[i]] = predict(m, newdata = regr.test)[, 1]
  }

  testSimpleParsets("regr.blackboost", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list2)
})


test_that("regr_blackboost works with families for count data", {
  # change target to count data
  new.regr.df = regr.df
  new.regr.df[, regr.target] = as.integer(floor(new.regr.df[, regr.target]))
  new.regr.train = new.regr.df[regr.train.inds, ]
  new.regr.test = new.regr.df[regr.test.inds, ]
  parset.list1 = list(
    list(family = mboost::Poisson(), control = mboost::boost_control(nu = 0.02)),
    list(family = mboost::NBinomial()),
    list(family = mboost::Hurdle())
  )
  parset.list2 = list(
    list(family = "Poisson", nu = 0.02),
    list(family = "NBinomial"),
    list(family = "Hurdle")
  )
  old.predicts.list = list()
  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(regr.formula, data = new.regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(mboost::blackboost, pars)
    set.seed(getOption("mlr.debug.seed"))
    old.predicts.list[[i]] = predict(m, newdata = new.regr.test)[, 1]
  }
  testSimpleParsets("regr.blackboost", new.regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list2)
})

