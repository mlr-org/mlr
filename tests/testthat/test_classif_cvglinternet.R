context("classif_cvglinternet")

test_that("classif_cvglinternet", {
  requirePackages("glinternet", default.method = "load")
  parset.list = list(
    list(),
    list(nFolds = 3, screenLimit = 5, lambda = 2^(-(8:11)), lambdaType = "lambdaHat1Std")
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    lambdaType = parset[["lambdaType"]]
    if (is.null(lambdaType))
      lambdaType = "lambdaHat"
    parset[["lambdaType"]] = NULL

    x = binaryclass.train
    y = x[, binaryclass.class.col]
    Y = ifelse(y == "M", 1, 0)	# first factor level M is positive class by default
    x[, binaryclass.class.col] = NULL
    numLevels = rep(1, ncol(x))
    pars = list(X = as.matrix(x), Y = Y, family = "binomial", numLevels = numLevels)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(glinternet::glinternet.cv, pars)

    newx = binaryclass.test
    newx[, binaryclass.class.col] = NULL

    p = drop(predict(m, as.matrix(newx), type = "response", lambdaType = lambdaType))
    p2 = as.factor(binaryclass.class.levs[1 + (p <= 0.5)])
    
    old.probs.list[[i]] = p
    old.predicts.list[[i]] = p2
  }

  testSimpleParsets("classif.cvglinternet", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)
  testProbParsets ("classif.cvglinternet", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)

})
