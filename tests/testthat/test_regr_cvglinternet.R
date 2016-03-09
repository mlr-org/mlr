context("regr_cvglinternet")

test_that("regr_cvglinternet", {
  requirePackages("glinternet", default.method = "load")
  parset.list = list(
    list(),
    list(nFolds = 3, screenLimit = 5, lambda = 2^(-(10:11)), lambdaType = "lambdaHat1Std")
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    lambdaType = parset[["lambdaType"]]
    if (is.null(lambdaType))
      lambdaType = "lambdaHat"
    parset[["lambdaType"]] = NULL

    ind = match(regr.target, names(regr.train))
    x = regr.train[, -ind]
    numLevels = sapply(x, nlevels)
    numLevels[numLevels == 0] = 1
    x$chas = as.integer(x$chas) - 1L
    y = regr.train[, ind]
    pars = list(X = as.matrix(x), Y = y, family = "gaussian", numLevels = numLevels)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(glinternet::glinternet.cv, pars)

    newx = regr.test[,-ind]
    newx$chas = as.integer(newx$chas) - 1L

    old.predicts.list[[i]] = drop(predict(m, as.matrix(newx), lambdaType = lambdaType))
  }
  test.dat = regr.df
  testSimpleParsets("regr.cvglinternet", test.dat, regr.target, regr.train.inds, old.predicts.list, parset.list)
})
