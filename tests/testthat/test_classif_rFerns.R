context("classif_rFerns")

test_that("classif_rFerns", {
  requirePackages("rFerns", default.method = "load")
  
  parset.list = list(
    list(),
    list(ferns = 200L, depth = 4L)
  )

  old.predicts.list = list()
  
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    parset = c(list(formula = binaryclass.formula, data = binaryclass.train), parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(rFerns::rFerns, parset)
    old.predicts.list[[i]] = factor(predict(m, binaryclass.test))
  }
  
  testSimpleParsets("classif.rFerns", binaryclass.df, binaryclass.target, binaryclass.train.inds,
    old.predicts.list, parset.list)
})
