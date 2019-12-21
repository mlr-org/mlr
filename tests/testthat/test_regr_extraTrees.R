context("regr_extraTrees")

test_that("regr_extraTrees", {
  requirePackagesOrSkip("extraTrees", default.method = "load")

  parset.list = list(
    list(),
    list(ntree = 100L),
    list(ntree = 250L, mtry = 4L),
    list(ntree = 250L, nodesize = 2L, numRandomCuts = 2L)
  )

  x.vars = setdiff(names(regr.num.df), regr.num.target)
  x.test = as.matrix(regr.num.test[, x.vars])
  x.train = as.matrix(regr.num.train[, x.vars])
  y = regr.num.train[, regr.num.target]

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(x = x.train, y = y))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(extraTrees::extraTrees, parset)
    old.predicts.list[[i]] = predict(m, x.test)
  }

  testSimpleParsets("regr.extraTrees", regr.num.df, regr.num.target,
    regr.num.train.inds, old.predicts.list, parset.list)
})
