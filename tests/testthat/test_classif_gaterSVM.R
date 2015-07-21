context("classif_gaterSVM")

test_that("classif_gaterSVM", {
  requirePackages("SwarmSVM", default.method = "load")
  set.seed(getOption("mlr.debug.seed"))
  
  # Early Prediction
  model = SwarmSVM::gaterSVM(x = data.matrix(binaryclass.train[,-61]), y = binaryclass.train[,61], 
                             m = 2, max.iter = 1, seed = 0)
  p = predict(model, data.matrix(binaryclass.test[,-61]))
  p = factor(p, labels = levels(binaryclass.train[,61]))
  
  testSimple("classif.gaterSVM", binaryclass.df, binaryclass.target, binaryclass.train.inds, p,
             parset = list(m = 2, max.iter = 1, seed = 0))
})
