context("classif_xgboost")

test_that("classif_xgboost", {
  requirePackagesOrSkip("xgboost", default.method = "load")

  set.seed(getOption("mlr.debug.seed"))
  model = xgboost::xgboost(data = data.matrix(binaryclass.train[,1:60]), verbose = 0L,
    label = as.numeric(binaryclass.train[,61])-1,
    nrounds = 20, objective = "binary:logistic", missing = NULL)
  pred = xgboost::predict(model, data.matrix(binaryclass.test[,1:60]))
  pred = factor(as.numeric(pred>0.5), labels = binaryclass.class.levs)

  set.seed(getOption("mlr.debug.seed"))
  testSimple("classif.xgboost", binaryclass.df, binaryclass.target, binaryclass.train.inds, pred,
    parset = list(nrounds = 20))
})

test_that("xgboost works with different 'missing' arg vals", {
  lrn = makeLearner("classif.xgboost", missing = NA_real_)
  lrn = makeLearner("classif.xgboost", missing = NA)
  lrn = makeLearner("classif.xgboost", missing = NULL)
})


