context("classif_extraTrees")

test_that("classif_extraTrees", {
  requirePackagesOrSkip("extraTrees", default.method = "load")

  parset.list = list(
    list(),
    list(ntree = 100L),
    list(ntree = 250L, mtry = 4L),
    list(ntree = 250L, nodesize = 2L, numRandomCuts = 2L)
  )

  old.predicts.list = list()
  old.probs.list = list()

  x.vars = setdiff(names(binaryclass.df), binaryclass.target)
  x.test = as.matrix(binaryclass.df[binaryclass.test.inds, x.vars])
  x.train = as.matrix(binaryclass.df[binaryclass.train.inds, x.vars])
  y = binaryclass.df[binaryclass.train.inds, binaryclass.target]


  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(x = x.train, y = y))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(extraTrees::extraTrees, parset)
    old.predicts.list[[i]] = predict(m, x.test)
    old.probs.list[[i]] = predict(m, x.test, probability = TRUE)[, 1L]
  }

  testSimpleParsets("classif.extraTrees", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.extraTrees", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)
})
