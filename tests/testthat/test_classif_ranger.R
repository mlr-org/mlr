context("classif_ranger")

## TODO: Add "response" test if R seed is respected in ranger::predict()
test_that("classif_ranger", {
  requirePackages("ranger", default.method = "load")
  
  parset.list = list(
    list(),
    list(num.trees = 100),
    list(num.trees = 250, mtry = 4),
    list(num.trees = 500, min.node.size = 2)
  )
  old.probs.list = list()
  
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = binaryclass.train, formula = binaryclass.formula, write.forest = TRUE, probability = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(ranger::ranger, parset)
    p  = predict(m, data = binaryclass.test)
    old.probs.list[[i]] = p$predictions[, 1]
  }
  
  testProbParsets ("classif.ranger", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list)
})
