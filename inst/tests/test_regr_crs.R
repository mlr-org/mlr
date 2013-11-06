context("regr_crs")

test_that("regr_crs", {
  library(crs)
  parset.list = list(
    list(nmulti=1),
    list(degree=rep(3, 12), nmulti=1),
    list(segments=rep(3, 12), nmulti=1)
  )
  
  old.predicts.list = list()
  
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(regr.formula, data=regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(crs, pars)
    set.seed(getOption("mlr.debug.seed"))
    pred = predict(m, newdata=regr.test)
    attr(pred, "lwr") = NULL
    attr(pred, "upr") = NULL
    old.predicts.list[[i]] = pred 
  }
  
  testSimpleParsets("regr.crs", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
})
