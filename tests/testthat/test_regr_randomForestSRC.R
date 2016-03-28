context("regr_randomForestSRC")

test_that("regr_randomForestSRC", {
  requirePackagesOrSkip("randomForestSRC", default.method = "load")

  parset.list = list(
    list(),
    list(ntree = 100),
    list(ntree = 50, mtry.ratio = 0.9),
    list(ntree = 50, nodesize = 2, na.action = "na.impute", importance = "permute", proximity = FALSE)
  )
  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    if (!is.null(parset$mtry.ratio)) {
      parset$mtry = parset$mtry.ratio * (ncol(regr.df) - 1)
      parset$mtry.ratio = NULL
    } 
    parset = c(parset, list(data = regr.train, formula = regr.formula, forest = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(randomForestSRC::rfsrc, parset)
    # versison 2.0 of randomForestSRC returns an array here :(
    p = as.numeric(predict(m, newdata = regr.test, membership = FALSE, na.action = "na.impute")$predicted)
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.randomForestSRC", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
})
