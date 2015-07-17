context("classif_clusterSVM")

test_that("classif_clusterSVM", {
  requirePackages("SwarmSVM", default.method = "load")
  set.seed(getOption("mlr.debug.seed"))
  model = SwarmSVM::clusterSVM(x = data.matrix(binaryclass.train[,-61]), y = binaryclass.train[,61], 
                               centers = 3, seed = 0)
  p = predict(model, data.matrix(binaryclass.test[,-61]))$predictions
  
  testSimple("classif.clusterSVM", binaryclass.df, binaryclass.target, binaryclass.train.inds, p, 
             parset = list(centers = 3, seed = 0))
})

