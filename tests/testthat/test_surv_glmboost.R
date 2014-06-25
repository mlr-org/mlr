context("surv_glmboost")

test_that("surv_glmboost", {
  library(survival)
  parset.list = list(
    list(mstop = 100L, nu = 0.1),
    list(mstop = 50L, nu = 1),
    list(mstop = 200L, nu = 0.5),
    list(mstop = 250L, nu = 0.05)
  )
  
  old.predicts.list = list()
  
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    ctrl = boost_control(mstop = parset$mstop, nu = parset$nu)
    f = getTaskFormula(surv.task, env=as.environment("package:survival"))
    pars = list(f, data = surv.train, control = ctrl, family = CoxPH())
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(glmboost, pars)
    
    p  = predict(m, newdata = surv.test, type = "link")
    old.predicts.list[[i]] = drop(p)
  }
  
  testSimpleParsets("surv.glmboost", surv.df, surv.target, surv.train.inds, old.predicts.list, parset.list)
})
