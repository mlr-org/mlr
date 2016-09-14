context("classif_gamboost")

test_that("classif_gamboost", {
  requirePackagesOrSkip("mboost", default.method = "attach")
  
  parset.list1 = list(
    list(family = mboost::Binomial(), baselearner = "bols", control = mboost::boost_control(nu = 0.03)),
    list(family = mboost::Binomial(link = "probit"), baselearner = "btree", control = mboost::boost_control(mstop = 600))
  )
  
  parset.list2 = list(
    list(family = "Binomial", baselearner = "bols", nu = 0.03),
    list(family = "Binomial", Binomial.link = "probit", baselearner = "btree", mstop = 600)
  )
  
  old.predicts.list = list()
  old.probs.list = list()
  
  for (i in 1:length(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(binaryclass.formula, data=binaryclass.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(mboost::gamboost, pars)
    set.seed(getOption("mlr.debug.seed"))
    old.predicts.list[[i]] = predict(m, newdata=binaryclass.test, type="class")
    set.seed(getOption("mlr.debug.seed"))
    old.probs.list[[i]] = 1 - predict(m, newdata=binaryclass.test, type="response")[,1]
  }
  
  testSimpleParsets("classif.gamboost", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.predicts.list, parset.list2)
  testProbParsets("classif.gamboost", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list2)
})