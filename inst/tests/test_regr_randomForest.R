context("regr_randomForest")

test_that("regr_randomForest", {
  library(randomForest)
  parset.list = list(
    list(),
    list(ntree = 5, mtry = 2),
    list(ntree = 5, mtry = 4)
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula=regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(randomForest, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = regr.test, type = "response")
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.randomForest", regr.df, regr.target,
    regr.train.inds, old.predicts.list, parset.list)

  tt = randomForest

  testCVParsets("regr.randomForest", regr.df, regr.target, tune.train = tt, parset.list = parset.list)

})
