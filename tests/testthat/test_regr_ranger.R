context("regr_ranger")

test_that("regr_ranger", {
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
    parset = c(parset, list(data = regr.train, formula = regr.formula, write.forest = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(ranger::ranger, parset)
    p  = predict(m, data = regr.test)
    old.predicts.list[[i]] = p$predictions
  }
  
  testSimpleParsets("regr.ranger", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
})
