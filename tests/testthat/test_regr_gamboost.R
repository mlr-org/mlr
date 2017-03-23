context("regr_gamboost")
test_that("regr_gamboost", {
  requirePackagesOrSkip("mboost", default.method = "attach")

  parset.list1 = list(
    list(),
    list(family = mboost::Gaussian(), baselearner = "bols", dfbase = 4,
      control = mboost::boost_control(nu = 0.03, mstop = 200)),
    list(family = mboost::GammaReg(nuirange = c(0, 50)), baselearner = "btree",
       control = mboost::boost_control(mstop = 100)),
    list(family = mboost::Family(ngradient = function(y, f, w = 1) y - f,
      loss = function(y, f) (y - f)^2,
      name = "My Gauss Variant"))
  )
  parset.list2 = list(
    list(),
    list(family = "Gaussian", baselearner = "bols", dfbase = 4, nu = 0.03, mstop = 200),
    list(family = "GammaReg", baselearner = "btree", nuirange = c(0, 50), mstop = 100),
    list(family = "custom.family", custom.family.definition =  mboost::Family(ngradient = function(y, f, w = 1) y - f,
      loss = function(y, f) (y - f)^2,
      name = "My Gauss Variant"))
  )
  old.predicts.list = list()
  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(mboost::gamboost, pars)
    set.seed(getOption("mlr.debug.seed"))
    old.predicts.list[[i]] = as.vector(predict(m, newdata = regr.test))
  }
  testSimpleParsets("regr.gamboost", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list2)
})


test_that("regr_gamboost works with families for count data", {
  # set some dummy counts
  new.regr.df = regr.df
  new.regr.df[, regr.target] = as.integer(floor(new.regr.df[, regr.target]))
  new.regr.df[, "chas"] = NULL
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
    m = do.call(mboost::gamboost, pars)
    set.seed(getOption("mlr.debug.seed"))
    old.predicts.list[[i]] = as.vector(predict(m, newdata = new.regr.test))
  }
  testSimpleParsets("regr.gamboost", new.regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list2)
})
