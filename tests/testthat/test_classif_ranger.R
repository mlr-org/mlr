context("classif_ranger")

test_that("classif_ranger", {
  requirePackages("ranger", default.method = "load")
  
  parset.list = list(
    list(),
    list(num.trees = 100),
    list(num.trees = 250, mtry = 4),
    list(num.trees = 500, min.node.size = 2)
  )
  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = binaryclass.train, formula = binaryclass.formula, write.forest = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(ranger::ranger, parset)
    p  = predict(m, data = binaryclass.test)
    old.predicts.list[[i]] = p$predictions
  }
  
  testSimpleParsets("classif.ranger", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.predicts.list, parset.list)
})
