context("regr_glmboost")
test_that("regr_glmboost", {
  requirePackagesOrSkip("mboost", default.method = "load")

  parset.list1 = list(
    list(),
    list(family = mboost::Gaussian(), control = mboost::boost_control(nu = 0.03)),
    list(family = mboost::GammaReg(nuirange = c(0,50)), control = mboost::boost_control(mstop = 600), center = TRUE),
    list(family = mboost::Family(ngradient = function(y, f, w = 1) y - f,
      loss = function(y, f) (y - f)^2,
      name = "My Gauss Variant"))
  )
  parset.list2 = list(
    list(),
    list(family = "Gaussian", nu = 0.03),
    list(family= "GammaReg", nuirange = c(0,50), mstop = 600, center = TRUE),
    list(family = "custom.family", custom.family.definition =  mboost::Family(ngradient = function(y, f, w = 1) y - f,
    loss = function(y, f) (y - f)^2,
    name = "My Gauss Variant"))
  )
  old.predicts.list = list()
  for (i in 1:length(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(mboost::glmboost, pars)
    set.seed(getOption("mlr.debug.seed"))
    old.predicts.list[[i]] = as.vector(predict(m, newdata=regr.test))
  }
  testSimpleParsets("regr.glmboost", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list2)
})


test_that("regr_glmboost works with poisson", {
  # set some dummy counts
  d = regr.df
  d[, regr.target] = sample(1:100, getTaskSize(regr.task), replace = TRUE)
  task = makeRegrTask(data = d, target = regr.target)
  lrn = makeLearner("regr.glmboost", par.vals = list(family = "Poisson"))
  r = holdout(lrn, task)
  expect_true(!is.na(r$aggr))
})
