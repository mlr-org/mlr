context("regr_randomForestSRC")

test_that("regr_randomForestSRC", {
  requirePackages("randomForestSRC", default.method = "load")

  parset.list = list(
    list(),
    list(ntree = 100),
    list(ntree = 50, mtry = 4),
    list(ntree = 50, nodesize = 2, na.action = "na.impute")
  )
  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = regr.train, formula = regr.formula, importance = "none", proximity = FALSE, forest = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(randomForestSRC::rfsrc, parset)
    # versison 2.0 of randomForestSRC returns an array here :(
    p  = as.numeric(predict(m, newdata = regr.test, importance = "none", na.action = "na.impute")$predicted)
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.randomForestSRC", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
})
