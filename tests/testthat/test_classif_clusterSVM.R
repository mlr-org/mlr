context("classif_clusterSVM")

test_that("classif_clusterSVM", {
  requirePackagesOrSkip("SwarmSVM", default.method = "load")

  parset.list1 = list(
    list(centers = 2),
    list(centers = 3, seed = 0)
  )

  parset.list2 = list(
    list(),
    list(centers = 3, seed = 0)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    pars = list(data.matrix(binaryclass.train[, -61]), y = binaryclass.train[, 61])
    pars = c(pars, parset)
    m = do.call(SwarmSVM::clusterSVM, pars)
    old.predicts.list[[i]] = predict(m, data.matrix(binaryclass.test[, -61]))$predictions
  }

  testSimpleParsets("classif.clusterSVM", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list2)
})
