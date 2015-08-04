context("classif_xgboost")

test_that("classif_xgboost", {
  requirePackages("xgboost", default.method = "load")
  
  set.seed(getOption("mlr.debug.seed"))
  model = xgboost::xgboost(data = data.matrix(binaryclass.train[,1:60]), 
                           label = as.numeric(binaryclass.train[,61])-1,
                           nrounds = 20, objective = "binary:logistic")
  pred = xgboost::predict(model, data.matrix(binaryclass.test[,1:60]))
  pred = factor(as.numeric(pred>0.5), labels = binaryclass.class.levs)
  
  set.seed(getOption("mlr.debug.seed"))
  testSimple("classif.xgboost", binaryclass.df, binaryclass.target, binaryclass.train.inds, pred, 
             parset = list(nrounds = 20))
})
