context("regr_randomForestSRCSyn")

test_that("regr_randomForestSRCSyn", {
  requirePackages("randomForestSRC", default.method = "load")

  parset.list = list(
    list(),
    list(ntree = 10L),
    list(ntree = 5L, mtry = 4L),
    list(ntree = 5L, nodesize = 2L, na.action = "na.impute")
  )
  old.predicts.list = list()

  for (i in 1L:length(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = regr.train, formula = regr.formula, newdata = regr.test,
     importance = "none", proximity = FALSE, forest = TRUE, verbose = FALSE))
    set.seed(getOption("mlr.debug.seed"))
    p = do.call(randomForestSRC::rfsrcSyn, parset)$rfSynPred$predicted
    # versison 2.0 of randomForestSRC returns an array here :(
    old.predicts.list[[i]] = as.numeric(p)
  }

  testSimpleParsets("regr.randomForestSRCSyn", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
})
