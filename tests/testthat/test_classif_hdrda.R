context("classif_hdrda")

test_that("classif_hdrda", {
  requirePackages("sparsediscrim", default.method = "load")
  
  parset.list = list(
    list(),
    list(lambda = 0.9, gamma = 0.9)
  )

  old.predicts.list = list()
  old.probs.list = list()
  
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    parset$x = as.matrix(binaryclass.train[, -binaryclass.class.col])
    parset$y = binaryclass.train[, binaryclass.class.col]
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(sparsediscrim::hdrda, parset)
    old.predicts.list[[i]] = predict(m, binaryclass.test[, -binaryclass.class.col])$class
    old.probs.list[[i]] = predict(m, binaryclass.test[, -binaryclass.class.col])$posterior[,1L]
  }
  
  testSimpleParsets("classif.hdrda", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.predicts.list, parset.list)
  testProbParsets("classif.hdrda", binaryclass.df, binaryclass.target, binaryclass.train.inds,
                   old.probs.list, parset.list)
})
