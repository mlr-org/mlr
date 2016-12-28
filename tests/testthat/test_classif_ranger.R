context("classif_ranger")

## TODO: Add "response" test if R seed is respected in ranger::predict()
test_that("classif_ranger", {
  requirePackagesOrSkip("ranger", default.method = "load")

  parset.list = list(
    list(),
    list(num.trees = 100),
    list(num.trees = 250, mtry = 4),
    list(num.trees = 500, min.node.size = 2)
  )
  old.probs.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(data = binaryclass.train, formula = binaryclass.formula, write.forest = TRUE, probability = TRUE, respect.unordered.factors = TRUE))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(ranger::ranger, parset)
    p  = predict(m, data = binaryclass.test)
    old.probs.list[[i]] = p$predictions[, 1]
  }

  testProbParsets ("classif.ranger", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list)
  
  #test that weights work as expected
  lrn1 = makeLearner("classif.ranger", num.trees = 200L)
  lrn2 = makeWeightedClassesWrapper(lrn1, wcw.weight = 2)
  tpr1 = holdout(lrn1, pid.task, measures = tpr)$aggr
  tpr2 = holdout(lrn2, pid.task, measures = tpr)$aggr
  expect_true(tpr1 < tpr2)
})
