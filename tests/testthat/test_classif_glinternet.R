context("classif_glinternet")

test_that("classif_glinternet", {
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

    x = binaryclass.train
    y = x[, binaryclass.class.col]
    Y = ifelse(y == "M", 1, 0)	# first factor level M is positive class by default
    x[, binaryclass.class.col] = NULL
    numLevels = sapply(x, nlevels)
    numLevels[numLevels == 0] = 1
    pars = list(X = as.matrix(x), Y = Y, family = "binomial", numLevels = numLevels)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(glinternet::glinternet, pars)

    newx = binaryclass.test
    newx[, binaryclass.class.col] = NULL
    lambda = m$lambda
    lambda = lambda[which.min(abs(lambda - predictLambda))]

    p = drop(predict(m, as.matrix(newx), type = "response", lambda = lambda))
    p2 = as.factor(binaryclass.class.levs[1 + (p <= 0.5)])
   
    old.probs.list[[i]] = p
    old.predicts.list[[i]] = p2
  }

  testSimpleParsets("classif.glinternet", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)
  testProbParsets ("classif.glinternet", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)

})
