context("regr_xgboost")

test_that("regr_xgboost", {
  requirePackages("xgboost", default.method = "load")
  
  set.seed(getOption("mlr.debug.seed"))
  model = xgboost::xgboost(data = data.matrix(regr.train[,1:13]), 
                           label = as.numeric(regr.train[,14]),
                           nrounds = 20, objective = "reg:linear")
  pred = xgboost::predict(model, data.matrix(regr.test[,1:13]))
  
  set.seed(getOption("mlr.debug.seed"))
  testSimple("regr.xgboost", regr.df, regr.target, regr.train.inds, pred, 
             parset = list(nrounds = 20))
})
