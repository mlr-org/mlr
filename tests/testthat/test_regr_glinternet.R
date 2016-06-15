context("regr_glinternet")

test_that("regr_glinternet", {
  requirePackages("glinternet", default.method = "load")
  parset.list = list(
    list(),
    list(nLambda = 10, lambdaMinRatio = 0.05, predictLambda = 0.02, numToFind = 10),
    list(lambda = 2^(-(6:10)), predictLambda = 0.02, screenLimit = 5)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    predictLambda = parset[["predictLambda"]]
    if (is.null(predictLambda))
      predictLambda = 0.01
    parset[["predictLambda"]] = NULL

    ind = match(regr.target, names(regr.train))
    x = regr.train[, -ind]
    numLevels = sapply(x, nlevels)
    numLevels[numLevels == 0] = 1
    x$chas = as.integer(x$chas) - 1L
    y = regr.train[, ind]
    pars = list(X = as.matrix(x), Y = y, family = "gaussian", numLevels = numLevels)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(glinternet::glinternet, pars)

    newx = regr.test[,-ind]
    newx$chas = as.integer(newx$chas) - 1L

    lambda = m$lambda
    lambda = lambda[which.min(abs(lambda - predictLambda))]

    old.predicts.list[[i]] = drop(predict(m, as.matrix(newx), lambda = lambda))
  }
  test.dat = regr.df
  testSimpleParsets("regr.glinternet", test.dat, regr.target, regr.train.inds, old.predicts.list, parset.list)
})
