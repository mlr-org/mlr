context("surv_ranger")

test_that("surv_ranger", {
  requirePackages("survival", default.method = "load")
  requirePackages("ranger", default.method = "load")
  
  parset.list = list(
    list(),
    list(num.trees = 100),
    list(num.trees = 250, mtry = 4),
    list(num.trees = 500, min.node.size = 2)
  )
  old.predicts.list = list()
  
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = surv.train, formula = surv.formula, write.forest = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(ranger::ranger, parset)
    p  = predict(m, data = surv.test)
    old.predicts.list[[i]] = p$predictions
  }
  
  testSimpleParsets("surv.ranger", surv.df, surv.target, surv.train.inds, old.predicts.list, parset.list)
})
