context("classif_dcSVM")

test_that("classif_dcSVM", {
  requirePackages("SwarmSVM", default.method = "load")
  set.seed(getOption("mlr.debug.seed"))
  
  # Early Prediction
  model = SwarmSVM::dcSVM(x = data.matrix(binaryclass.train[,-61]), y = binaryclass.train[,61], 
                          m = 100, k = 10, max.levels = 1, early = 1, seed = 0)
  p = predict(model, data.matrix(binaryclass.test[,-61]))
  p = factor(p, labels = levels(binaryclass.train[,61]))
  
  testSimple("classif.dcSVM", binaryclass.df, binaryclass.target, binaryclass.train.inds, p,
             parset = list(m = 100, k = 10, max.levels = 1, early = 1, seed = 0))
  
  # Exact Prediction
  model = SwarmSVM::dcSVM(x = data.matrix(binaryclass.train[,-61]), y = binaryclass.train[,61], 
                          m = 100, k = 2, max.levels = 3, early = 0, seed = 0)
  p = predict(model, data.matrix(binaryclass.test[,-61]))
  p = factor(p, labels = levels(binaryclass.train[,61]))
  
  testSimple("classif.dcSVM", binaryclass.df, binaryclass.target, binaryclass.train.inds, p,
             parset = list(m = 100, k = 2, max.levels = 3, early = 0, seed = 0))
  
})
