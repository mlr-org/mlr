context("regr_xgboost")

test_that("regr_xgboost", {
  requirePackagesOrSkip("xgboost", default.method = "load")
  
  parset.list = list(
    list(),
    list(nrounds = 20)
  )

  old.predicts.list = list()
  
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    if (is.null(parset$verbose)) parset$verbose = 0L
    if (is.null(parset$nrounds)) parset$nrounds = 1L
    if (is.null(parset$objective)) parset$objective = "reg:linear"
    pars = list(data = data.matrix(regr.train[,1:13]), label = as.numeric(regr.train[,14]))
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    model = do.call(xgboost::xgboost, pars)
    #model = xgboost::xgboost(data = data.matrix(regr.train[,1:13]), verbose = 0L,
    #label = as.numeric(regr.train[,14]),
    #nrounds = 20, objective = "reg:linear", missing = NULL)
    old.predicts.list[[i]] = predict(model, data.matrix(regr.test[,1:13]))
  }
  
  #set.seed(getOption("mlr.debug.seed"))
  testSimpleParsets("regr.xgboost", regr.df, regr.target, regr.train.inds,
    old.predicts.list, parset.list)
})

test_that("xgboost works with different 'missing' arg vals", {
  lrn = makeLearner("classif.xgboost", missing = NA_real_)
  lrn = makeLearner("classif.xgboost", missing = NA)
  lrn = makeLearner("classif.xgboost", missing = NULL)
})


