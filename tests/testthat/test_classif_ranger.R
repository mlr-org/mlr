context("classif_ranger")

## FIXME: Add "response" test if R seed is respected in ranger::predict()
test_that("classif_ranger", {
  requirePackagesOrSkip("ranger", default.method = "load")

  parset.list = list(
    list(num.trees = 20),
    list(num.trees = 20, mtry = 4),
    list(num.trees = 20, min.node.size = 2)
  )
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = binaryclass.train,
      formula = binaryclass.formula, write.forest = TRUE, probability = TRUE,
      respect.unordered.factors = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(ranger::ranger, parset)
    p = predict(m, data = binaryclass.test)
    old.probs.list[[i]] = p$predictions[, 1]
  }

  testProbParsets("classif.ranger", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)
})
