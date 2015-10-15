context("classif_h2ogbm")

test_that("classif_h2ogbm", {
  requirePackages("h2o", default.method = "load")
  h2o::h2o.init()
  
  parset.list = list(
    list(),
    list(ntrees = 100),
    list(ntrees = 100, min_rows = 20),
    list(ntrees = 100, nbins = 2)
  )
  old.probs.list = list()
  
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset,list(x = colnames(binaryclass.train[, -binaryclass.class.col]),
      y = binaryclass.target, 
      training_frame = h2o::as.h2o(binaryclass.train)))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(h2o::h2o.gbm, parset)
    p  = predict(m, newdata = h2o::as.h2o(binaryclass.test))
    old.probs.list[[i]] = as.data.frame(p)[, 2]
  }
  
  testProbParsets ("classif.h2ogbm", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list)
})
