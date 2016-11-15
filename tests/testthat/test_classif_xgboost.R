context("classif_xgboost")

test_that("classif_xgboost", {
  requirePackagesOrSkip("xgboost", default.method = "load")

  parset.list = list(
    list(),
    list(nrounds = 20L)
  )
  
  old.predicts.list = list()
  
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(data = data.matrix(binaryclass.train[,1:60]),
      label = as.numeric(binaryclass.train[,61])-1)
    if (is.null(parset$objective)) parset$objective = "binary:logistic"
    if (is.null(parset$verbose)) parset$verbose = 0L
    if (is.null(parset$nround)) parset$nrounds = 1L
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    model = do.call(xgboost::xgboost, pars)
    pred = xgboost::predict(model, data.matrix(binaryclass.test[,1:60]))
    old.predicts.list[[i]] = factor(as.numeric(pred>0.5), labels = binaryclass.class.levs)
  }

  testSimpleParsets("classif.xgboost", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)
})

test_that("xgboost works with different 'missing' arg vals", {
  lrn = makeLearner("classif.xgboost", missing = NA_real_)
  lrn = makeLearner("classif.xgboost", missing = NA)
  lrn = makeLearner("classif.xgboost", missing = NULL)
})


