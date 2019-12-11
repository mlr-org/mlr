context("regr_randomForestSRC")

test_that("regr_randomForestSRC", {
  requirePackagesOrSkip("randomForestSRC", default.method = "load")

  parset.list = list(
    list(ntree = 10),
    list(ntree = 10, mtry = 5L),
    list(ntree = 10, nodesize = 2, na.action = "na.impute",
      importance = "permute", proximity = FALSE)
  )
  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = regr.train, formula = regr.formula,
      forest = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(randomForestSRC::rfsrc, parset)
    # versison 2.0 of randomForestSRC returns an array here :(
    p = as.numeric(predict(m, newdata = regr.test, membership = FALSE,
      na.action = "na.impute")$predicted)
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.randomForestSRC", regr.df, regr.target,
    regr.train.inds, old.predicts.list, parset.list)
})
