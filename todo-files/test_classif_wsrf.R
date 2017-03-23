context("classif_wsrf")

test_that("classif_wsrf", {
  requirePackages("wsrf", default.method = "load")
  
  parset.list = list(
    list(ntrees = 100L),
    list(mtry = 5L, ntrees = 100L, weights = FALSE)
  )

  old.predicts.list = list()
  old.probs.list = list()
  
  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(list(formula = binaryclass.formula, data = binaryclass.train, parallel = FALSE), parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(wsrf::wsrf, parset)
    old.predicts.list[[i]] = wsrf::predict.wsrf(m, binaryclass.test)
    old.probs.list[[i]] = wsrf::predict.wsrf(m, binaryclass.test, type = "prob")[,1L]
  }
  
  testSimpleParsets("classif.wsrf", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.predicts.list, parset.list)
  testProbParsets("classif.wsrf", binaryclass.df, binaryclass.target, binaryclass.train.inds,
                   old.probs.list, parset.list)
})
