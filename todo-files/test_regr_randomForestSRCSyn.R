context("regr_randomForestSRCSyn")

test_that("regr_randomForestSRCSyn", {
  requirePackagesOrSkip("randomForestSRC", default.method = "load")

  parset.list = list(
    list(),
    list(ntree = 10L),
    list(ntree = 5L, mtry = 4L),
    list(ntree = 5L, nodesize = 2L, nsplit = 5, splitrule = "random")
  )
  old.predicts.list = list()

  for (i in 1L:length(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = regr.train, formula = regr.formula, forest = TRUE, na.action = "na.impute", verbose = FALSE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(randomForestSRC::rfsrcSyn, parset)
    p = randomForestSRC::rfsrcSyn(object = m, newdata = regr.test, na.action = "na.impute", verbose = FALSE, membership = FALSE)$rfSynPred$predicted    
    # version > 2.0 of randomForestSRC returns an array here :(
    old.predicts.list[[i]] = as.numeric(p)
  }

  testSimpleParsets("regr.randomForestSRCSyn", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
})
